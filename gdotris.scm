#!/usr/bin/env -S guile --listen=7777
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
           (L . (#x00E1 #x0C44 #x002E #x0446))
           (O . (#x0660 #x0660 #x0660 #x0660))
           (S . (#x0036 #x0462 #x0036 #x0462))
           (T . (#x0072 #x0262 #x0270 #x0232))
           (Z . (#x00C6 #x0264 #x00C6 #x0264))))

(define-record-type <tetromino>
  (make-tetromino type position state)
  tetromino?
  (type tetromino-type set-tetromino-type!)
  (position tetromino-position set-tetromino-position!)
  (state tetromino-state set-tetromino-state!))

(define (new-tetromino type)
  (make-tetromino type '(0 0) 0))

(define (tetromino-next-state tetr)
  (set-tetromino-state!
   tetr
   (match (tetromino-state tetr)
     ((4) 0)
     (current (+ current 1)))))

(define (tetromino->array tetr)
  "convert tetromino to bitvector form for drawing"
  (let ((t (list-ref
            (assoc-ref tetr-states
                       (tetromino-type tetr))
            (tetromino-state tetr))))
   (list->array 1 (integer->list t 16)))) 

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
   (new-tetromino 'T) ; TODO: randomize
   'L                 ; TODO: randomize
   #f
   0))
   
(define braille-offset #x2800)

;; Offset of braille codepoints in unicode, ending at #x28FF
(define (grid-cell-at-pos grid x y)
  "return an integer from grid coordinates that can be converted to a braille char"
  (list->integer
   (list
    (array-ref grid (+ y 3) (+ x 1))      
    (array-ref grid (+ y 3) x)
    (array-ref grid (+ y 2) (+ x 1))
    (array-ref grid (+ y 1) (+ x 1))
    (array-ref grid y (+ x 1))
    (array-ref grid (+ y 2) x)
    (array-ref grid (+ y 1) x)
    (array-ref grid y x))))

(define (integer->braille i)
  "convert an integer to unicode braille character"
  (integer->char (+ braille-offset i)))

(define stdscr (initscr))
(define (setup)
  "initialize the ncurses screen and other game settings."
    (setlocale LC_ALL "")
    (raw!)         ; no line buffering
    (noecho!)      ; don't echo characters entered
    ;; (halfdelay! 1) ; wait 1/10th of a second on getch
    (curs-set 0))  ; hide the cursor

(define (grid-pos->screen-pos x y)
  (cons
   (quotient x cell-width)
   (quotient y cell-height)))

(define (tetromino-overlay grid tetr op)
  "helper to merge/unmerge a tetromino into a grid"
  (match-let (((tx ty) (tetromino-position tetr))
              (tvec (tetromino->array tetr)))
    (do ((i 0 (1+ i)))
        ((= i 16))
      (let* ((dx (truncate-remainder i 4))
             (dy (truncate-quotient i 4))
             (x (+ tx dx))
             (y (+ ty dy)))
        (when (and (>= x 0) (>= y 0)
                   (< x grid-width) (< y grid-height))
          (array-set! grid
                      (op (array-ref tvec i)
                          (array-ref grid y x))
                      y x))))))

(define (grid-write-tetrmomino grid tetr)
  (tetromino-overlay grid tetr (lambda (a b) (or a b))))
(define (grid-remove-tetrmomino grid tetr)
  (tetromino-overlay grid tetr (lambda (a b) (not (eq? a b)))))

(define (grid-draw grid x-off y-off)
  "iterate over a 4x2 frame/window (cell) of the grid and display as braille characters
starting at the position (x-off, y-off)."
  ;; FIXME: i wish this could be recursive or use some nicer helper function
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
  (let ((grid (game-state-grid state)))
    (grid-draw grid 0 0)
    (refresh stdscr)))

(define game (new-game-state))
(define grid (game-state-grid game)) ; testing only

(setup)
(let loop ()
  (begin
    (game-state-draw game)
    (getch stdscr)
    (loop)))
(endwin)
