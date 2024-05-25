#!/usr/bin/env -S GUILE_WARN_DEPRECATED=no guile --listen=7777 -e main -s
!#
(use-modules (ice-9 format)
             (ice-9 match)
             (ncurses curses)
             (srfi srfi-9)
             (srfi srfi-19)
             (srfi srfi-60))

(define grid-width 10)
(define grid-height 20)
(define grid-x 1)
(define grid-y 1)
(define cell-width 2)
(define cell-height 4)

;; tetromino states for each type, where each state is represented
;; as a list of integers that will be converted into a 4x4 grid.
;; if you write the numbers as 4-bit binary numbers, you can see that they
;; draw the tetromino shape.
(define tetr-states
  '((I . ((2 2 2 2) (0 15 0 0) (2 2 2 2) (0 15 0 0)))
    (J . ((7 1 0 0) (1 1 3 0) (4 7 0 0) (6 4 4 0)))
    (L . ((14 8 0 0) (12 4 4 0) (2 14 0 0) (4 4 6 0)))
    (O . ((6 6 0 0) (6 6 0 0) (6 6 0 0) (6 6 0 0)))
    (S . ((3 6 0 0) (4 6 2 0) (3 6 0 0) (4 6 2 0)))
    (T . ((7 2 0 0) (2 6 2 0) (2 7 0 0) (2 3 2 0)))
    (Z . ((12 6 0 0) (2 6 4 0) (12 6 0 0) (2 6 4 0)))))

(define (random-tetromino-type)
  (car (list-ref tetr-states (random (1- (length tetr-states))))))

(define-record-type <tetromino>
  (make-tetromino type position state)
  tetromino?
  (type tetromino-type set-tetromino-type!)
  (position tetromino-position set-tetromino-position!)
  (state tetromino-state set-tetromino-state!))

(define (new-tetromino type)
  (make-tetromino type '(3 0) 0))

(define (tetromino-translate t offset rotate)
  "return a new translated tetromino t by offset"
  (match-let* (((tx ty) (tetromino-position t))
               ((dx dy) offset)
               (new-tetr
                (make-tetromino (tetromino-type t)
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
  (let ((p (list-ref
            (assoc-ref tetr-states
                       (tetromino-type tetr))
            (tetromino-state tetr))))
    (list->array 2 (map (lambda (a) (integer->list a 4)) p))))

(define (tetromino-for-each-dot tetr expr)
  (match-let (((tx ty) (tetromino-position tetr))
              (tvec (tetromino->array tetr)))
    (let loop ((i 0)
               (j 0))
      (cond
       ((eq? i 4) #t) 
       ((< j 4)
        (let* ((x (+ tx j))
               (y (+ ty i))
               (res (expr x y (array-ref tvec i j))))
          (if (not res)
              res
              (loop i (1+ j)))))
       (else (loop (1+ i) 0))))))

(define (tetromino-overlay grid tetr op)
  "helper to merge/unmerge a tetromino into a grid"
  (tetromino-for-each-dot
   tetr
   (lambda (x y val)
     (when (and (>= x 0) (>= y 0)
                (< x grid-width) (< y grid-height))
      (array-set! grid
                  (op val (array-ref grid y x))
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
   score
   lines-cleared)
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
  (score game-state-score set-game-state-score!)
  (lines-cleared game-state-lines-cleared set-game-state-lines-cleared!))

(define (new-game-state)
  (make-game-state
   (make-typed-array 'b #f grid-height grid-width)
   (new-tetromino (random-tetromino-type))
   (random-tetromino-type)
   #f 0 0)) 
   
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

(define (grid-pos->screen-pos x y)
  (cons
   (quotient x cell-width)
   (quotient y cell-height)))
(define (grid-write-tetrmomino! grid tetr)
  (tetromino-overlay grid tetr (lambda (a b) (or a b))))
(define (grid-remove-tetrmomino! grid tetr)
  (tetromino-overlay grid tetr (lambda (a b) (not (eq? a b)))))

(define (grid-clear-lines! grid)
  "iterate over grid rows backwards and check if they are filled. if filled,
shift the previous rows down."
  (define (row-filled? row)
    (eq? 1023 ((compose list->integer array->list) (array-cell-ref grid row))))
  (let loop ((cleared 0)
             (row (1- grid-height)))
   (cond
    ((>= row 0)
     (when (row-filled? row)
      (let shift-rows ((start-row row))
        (cond
         ((> start-row 0)
          (array-cell-set! grid
                           (array-cell-ref grid (1- start-row))
                           start-row)
          (shift-rows (1- start-row)))
         ;; clear out the first row
         (else (array-cell-set! grid
                           (make-array #f 10)
                           start-row))))
      (set! cleared (1+ cleared)))
     (loop cleared (1- row)))
    (else cleared))))
        
(define (draw-array array x-off y-off)
  "iterate over a 4x2 frame/window (cell) of the array and display as braille characters
starting at the position (x-off, y-off)."
  (match-let (((dy dx) (array-dimensions array)))
    (do ((i 0 (+ 4 i)))
        ((>= i dy))
        (do ((j 0 (+ 2 j)))
            ((>= j dx))
            (match-let (((x . y) (grid-pos->screen-pos j i)))
             (addch
               stdscr
               (normal (integer->braille
                        (grid-cell-at-pos array j i)))
               #:x (+ x x-off)
               #:y (+ y y-off)))))))

(define (draw-held-tetr state x y)
  (let ((held (game-state-held-tetr-type state)))
    (draw-array (if held
                    (tetromino->array (new-tetromino held))
                    (make-array #f 4 4))
                x y)))
                            
(define (game-state-draw state)
  (let ((grid (game-state-grid state))
        (tetr (game-state-current-tetr state)))
    (grid-write-tetrmomino! grid tetr)
    (draw-array grid grid-x grid-y)
    (grid-remove-tetrmomino! grid tetr)
    (draw-held-tetr state 8 1)
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

(define (game-new-tetr! state)
  (set-game-state-current-tetr! state
   (new-tetromino (game-state-next-tetr-type state)))
  (set-game-state-next-tetr-type! state (random-tetromino-type)))

(define (game-lock-tetr! state)
  "writes the current piece to the grid"
  (grid-write-tetrmomino!
   (game-state-grid state)
   (game-state-current-tetr state))
  (game-new-tetr! state))

(define (game-drop-tetr! state)
  "drop the piece and lock it in. returns drop distance."
  (let loop ((lines 0))
    (cond
     ((game-try-move! state 'down)
      (loop (1+ lines)))
     (else (game-lock-tetr! state)    
           lines))))

(define (game-hold-or-recall-tetr! state)
  (let ((held (game-state-held-tetr-type state)))
    (cond
     (held
      (game-recall-tetr! state))
     (else
      (set-game-state-held-tetr-type!
       state
       (tetromino-type (game-state-current-tetr state)))
      (game-new-tetr! state)))))

(define (game-recall-tetr! state)
  (set-game-state-current-tetr!
   state
   (new-tetromino (game-state-held-tetr-type state)))
  (set-game-state-held-tetr-type! state #f))
  
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
  (endwin)
  (format #t "GAME OVER! You scored ~:d and cleared ~:d lines!\n\r"
          (game-state-score state)
          (game-state-lines-cleared state))
  (exit 0))

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

(define (draw-border)
  (addstr stdscr "┌─────┐" #:x 0 #:y 0)
  (let loop ((i 1))
    (when (<= i 5)
      (addstr stdscr "│     │" #:x 0 #:y i)
      (loop (1+ i))))
  (addstr stdscr "└─────┘" #:x 0 #:y 6))

(define (main args)
  ((setup)
   (draw-border)
   (let loop ((game (new-game-state))
              (now (current-time))
              (last-now (make-time time-utc 0 0))
              (tick-freq 1000))
     (begin
       (define moved #t)
       (when (<= tick-freq
                 (time->milliseconds (time-difference now last-now)))
           (set! moved (game-try-move! game 'down))
           (set! last-now now))

       (cond
        ;; tetromino hit floor and is stuck at the top
        ((and (not moved)
              (eq? (tetromino-y (game-state-current-tetr game)) 0)
              (end-game game)))
        ;; tetromino hit the floor
        ((not moved) (game-lock-tetr! game)))

       (define drop-multiplier 1)
       
       (match (getch stdscr)
         (#\q (end-game game))
         (#\space
          (set! drop-multiplier (game-drop-tetr! game)))
         (#\c
          (game-hold-or-recall-tetr! game))
         ((or 259 #\w) ; KEY_UP or w
          (game-try-move! game 'none #t))
         ((or 258 #\s) ; KEY_DOWN or s
          (game-try-move! game 'down))
         ((or 260 #\a) ; KEY_LEFT or a
          (game-try-move! game 'left))
         ((or 261 #\d) ; KEY_RIGHT or d
          (game-try-move! game 'right))
         ((or 265 #\r) ; F1 or r
          (set! game (new-game-state)))
         (_ #f))

       (let ((cleared (grid-clear-lines! (game-state-grid game))))
        (when (> cleared 0)
          (set-game-state-score!
           game (+ (game-state-score game)
                   (* 100
                      cleared cleared
                      drop-multiplier)))
          (set-game-state-lines-cleared!
           game
           (+ cleared (game-state-lines-cleared game)))))
      
       (game-state-draw game)
       (loop game (current-time) last-now
             (- 1000 (* 10 (game-state-lines-cleared game))))))))
