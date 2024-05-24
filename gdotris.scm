#!/usr/bin/env -S guile --listen=7777 -e main -s
!#
(use-modules (ice-9 match)
             (ncurses curses)
             (srfi srfi-9)
             (srfi srfi-19)
             (srfi srfi-60))

(define grid-width 10)
(define grid-height 20)
(define cell-width 2)
(define cell-height 4)

;; The states of tetrominoes are represented as 16-bit hex numbers. You can 
;; imagine each number's binary representation as a 4x4 grid, where each digit
;; is a row and 1-bit means the cell is filled by the piece. This could just be
;; arrays of arrays, but I want to keep the code size small.
(define tetr-states
         '((I . (#x2222 #x0F00 #x2222 #x0F00))
           (J . (#x0071 #x0113 #x0047 #x0644))
           (L . (#x00E8 #x0C44 #x002E #x0446))
           (O . (#x0660 #x0660 #x0660 #x0660))
           (S . (#x0036 #x0462 #x0036 #x0462))
           (T . (#x0072 #x0262 #x0270 #x0232))
           (Z . (#x00C6 #x0264 #x00C6 #x0264))))

(define (random-tetromino-type)
  (car (list-ref tetr-states (random (1- (length tetr-states))))))

(define-record-type <tetromino>
  (make-tetromino type position state)
  tetromino?
  (type tetromino-type set-tetromino-type!)
  (position tetromino-position set-tetromino-position!)
  (state tetromino-state set-tetromino-state!))

(define (new-tetromino type)
  (make-tetromino type '(0 0) 0))

(define (tetromino-translate t offset rotate)
  "return a new translated tetromino t by offset"
  (match-let* (((tx ty) (tetromino-position t))
               ((dx dy) offset)
               (new-tetr (make-tetromino (tetromino-type t)
                                         (list (+ tx dx) (+ ty dy))
                                         (tetromino-state t))))
    (begin
      (when rotate
            (tetromino-next-state new-tetr))
      new-tetr)))

(define (tetromino-next-state tetr)
  (set-tetromino-state!
   tetr
   (match (tetromino-state tetr)
     (3 0)
     (current (+ current 1)))))

(define (tetromino->array tetr)
  "convert tetromino to bitvector form for drawing"
  (let ((t (list-ref
            (assoc-ref tetr-states
                       (tetromino-type tetr))
            (tetromino-state tetr))))
    (list->array 1 (integer->list t 16))))

(define (tetromino-for-each-dot tetr expr)
  "run expr on each iteration over a tetromino's layout,
where expr takes (x y val). when expr evaluates to #f it
is like an escape hatch to the iteration. when the loop completes
it returns #t."
  (match-let (((tx ty) (tetromino-position tetr))
              (tvec (tetromino->array tetr)))
    (let loop ((i 0))
      (if (< i 16)
          (let* ((dx (truncate-remainder i 4))
                 (dy (truncate-quotient i 4))
                 (x (+ tx dx))
                 (y (+ ty dy))
                 (res (expr x y (array-ref tvec i))))
            (if (not res)
                res
                (loop (1+ i))))
          #t))))

(define (tetromino-overlay grid tetr op)
  "helper to merge/unmerge a tetromino into a grid"
  (tetromino-for-each-dot
   tetr
   (lambda (x y val)
     (when (and (>= x 0) (>= y 0)
                (< x grid-width) (< y grid-height))
      (array-set! grid
                  (op val
                      (array-ref grid y x))
                  y x)))))

(define (tetromino-valid-placement? tetr grid)
  "checks if a tetromino can be written to the grid"
  (tetromino-for-each-dot
   tetr
   (lambda (x y val)
     (not (and val
               (or (< x 0) (< y 0)
                   (>= x grid-width) (>= y grid-height)
                   (array-ref grid y x)))))))

(define (tetromino-x t)
  (car (tetromino-position t)))

(define (tetromino-y t)
  (cadr (tetromino-position t)))

(define (direction->point dir)
  (match dir
    ('left
     (list -1 0))
    ('right
     (list 1 0))
    ('down
     (list 0 1))
    ('none
     (list 0 0))))

(define-record-type <game-state>
  (make-game-state
   grid
   current-tetr
   next-tetr-type
   held-tetr-type
   score)
  game-state?
  (grid game-state-grid set-game-state-grid!)
  (current-tetr
   game-state-current-tetr
   set-game-state-current-tetr!)
  (next-tetr-type
   game-state-next-tetr-type
   set-game-state-next-tetr-type!)
  (held-tetr-type
   game-state-held-tetr-type
   set-game-state-held-tetr-type!)
  (score game-state-score set-game-state-score!))

(define (new-game-state)
  (make-game-state
   (make-typed-array 'b #f grid-height grid-width)
   (new-tetromino (random-tetromino-type))
   (random-tetromino-type)
   #f
   0))
   
(define braille-offset #x2800)

;; Offset of braille codepoints in unicode, ending at #x28FF
(define (grid-cell-at-pos grid x y)
  "return an integer from grid coordinates that can be converted to a braille char"
  (list->integer
   (let* ((x1 (1+ x))
          (x2 (1+ x1))
          (y1 (1+ y))
          (y2 (1+ y1))
          (y3 (1+ y2)))
    (map (lambda (y x) (array-ref grid y x))
         `(,y3 ,y3 ,y2 ,y1 ,y ,y2 ,y1 ,y)
         `(,x1 ,x ,x1 ,x1 ,x1 ,x ,x ,x)))))

(define (integer->braille i)
  "convert an integer to unicode braille character"
  (integer->char (+ braille-offset i)))

(define stdscr (initscr))
(define (setup)
  "initialize the ncurses screen and other game settings."
  (set! *random-state* (random-state-from-platform))
  (setlocale LC_ALL "")
  (raw!)         ; no line buffering
  (noecho!)      ; don't echo characters entered
  (halfdelay! 1) ; wait 1/10th of a second on getch
  (keypad! stdscr #t) ; function keys
  (curs-set 0))  ; hide the cursor

(define (grid-pos->screen-pos x y)
  (cons
   (quotient x cell-width)
   (quotient y cell-height)))
(define (grid-write-tetrmomino! grid tetr)
  (tetromino-overlay grid tetr (lambda (a b) (or a b))))
(define (grid-remove-tetrmomino! grid tetr)
  (tetromino-overlay grid tetr (lambda (a b) (not (eq? a b)))))
;; (define (grid-clear-lines grid))
(define (grid-draw grid x-off y-off)
  "iterate over a 4x2 frame/window (cell) of the grid and display as braille characters
starting at the position (x-off, y-off)."
  (do ((i 0 (+ 4 i)))
      ((>= i grid-height))
      (do ((j 0 (+ 2 j)))
          ((>= j grid-width))
          (match-let (((x . y) (grid-pos->screen-pos j i)))
           (addch
             stdscr
             (normal (integer->braille
                      (grid-cell-at-pos grid j i)))
             #:x (+ x x-off)
             #:y (+ y y-off))))))
                            
(define (game-state-draw state)
  (let ((grid (game-state-grid state))
        (tetr (game-state-current-tetr state)))
    (grid-write-tetrmomino! grid tetr)
    (grid-draw grid 0 0)
    (grid-remove-tetrmomino! grid tetr)
    (refresh stdscr)))

(define* (game-try-move! state dir #:optional rotate)
  (let* ((grid (game-state-grid state))
         (new-tetr (tetromino-translate
                    (game-state-current-tetr state)
                    (direction->point dir)
                    rotate))
         (valid (tetromino-valid-placement? new-tetr grid)))
    (when valid
      (set-game-state-current-tetr! state new-tetr))
    valid))

(define (game-lock-tetr! state)
  "writes the current piece to the grid"
  (grid-write-tetrmomino!
   (game-state-grid state)
   (game-state-current-tetr state))
  (set-game-state-current-tetr! state
   (new-tetromino (game-state-next-tetr-type state)))
  (set-game-state-next-tetr-type! state (random-tetromino-type)))

(define (game-drop-tetr! state)
  "drop the piece and lock it in"
  (while (game-try-move! state 'down))
  (game-lock-tetr! state))
  
(define (time->milliseconds time)
  "converts time object to milliseconds"
  (+
   (quotient (time-nanosecond time) 1000000)
   (* 1000 (time-second time))))

(define (milliseconds->time-duration ms)
  "returns a time-duration from given milliseconds"
  (let* ((total-nanos (* ms 1000000))
         (seconds (truncate-quotient total-nanos 1000000000))
         (nanos (truncate-remainder total-nanos 1000000000)))
    (make-time time-duration nanos seconds)))

(define (end-game state)
  ;; TODO: print score and stuff
  (endwin)
  (exit 0))

(define (main args)
 ((setup)
  (let loop ((game (new-game-state))
             (now (current-time))
             (last-now (make-time time-utc 0 0))
             (tick-freq 1000))
    (begin
      (when (<= tick-freq
                (time->milliseconds (time-difference now last-now)))
        (let ((moved (game-try-move! game 'down)))
          (cond
           ;; tetromino hit floor and is stuck at the top
           ((and (not moved)
                 (<= 0 (tetromino-y (game-state-current-tetr game))))
            (end-game game))
           ;; tetromino hit the floor
           ((not moved) (game-lock-tetr! game))))
        (set! last-now now))

      (match (getch stdscr)
        (#\q (end-game game))
        (259 ; KEY_UP 
         (game-try-move! game 'none #t))
        (258 ; KEY_DOWN
         (game-try-move! game 'down))
        (260 ; KEY_LEFT
         (game-try-move! game 'left))
        (#\space
         (game-drop-tetr! game))
        (261 ; KEY_RIGHT
         (game-try-move! game 'right))
        (265 ; F1
         (set! game (new-game-state)))
        (_ #f))
      (game-state-draw game)
      (loop game (current-time) last-now tick-freq)))))
