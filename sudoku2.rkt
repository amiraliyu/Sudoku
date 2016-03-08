#lang racket
(require 2htdp/image 2htdp/universe)
(define ROWS '("A" "B" "C" "D" "E" "F" "G" "H" "I"))
(define COLS (build-list 9 (λ (x) (+ 1 x))))
(define col-hash (make-hash))
(define row-hash (make-hash))
(define block-hash (make-hash))
(define BLOCK-SIZE 30)
(define TEXT-SIZE 15)
(define SQUARE (square BLOCK-SIZE 'outline 'black))

; A Board is a #hash(Pos Int)
; A Pos is a String of length 2: "string-append r c"
;   where r is a member of ROWS and c is a member of COLS.

; Int Image -> image 
; draw n copies of i in a row
(define (draw-row n i)
  (cond [(zero? n) empty-image]
        [else (beside i
                      (draw-row (sub1 n) i))]))
; Int -> Image
; Draw n squares in a column
(define (col n)
  (cond [(zero? n) empty-image]
        [else (above (square BLOCK-SIZE 'outline 'black)
                      (col (sub1 n)))]))

; Board Int [List-of Pos] Image -> Image
; Draw ls in a column. x is the x-coord of the column
(define (render-col Board x ls i)
  (define (acc r n)
    (cond [(empty? r) i]
          [else (define pos (first r))
                (place-image (render-text (hash-ref Board pos))
                             (+ (/ BLOCK-SIZE 2)
                                (* x BLOCK-SIZE))
                             (+ (/ BLOCK-SIZE 2)
                                (* n  BLOCK-SIZE))
                             (acc (rest r) (add1 n)))]))
  (acc ls 0))

; Board [List-of (Int (List-of Pos)] -> img 
; Render a board using association list of (col (list-of pos))
(define (render-board board col-list)
  (cond [(empty? col-list) (draw-row 9 (col 9))]
        [else (define col (first (first col-list)))
              (define ls (rest (first col-list)))
              (render-col board (- col 1) ls
                          (render-board board (rest col-list)))]))
; Board -> Image
(define (draw-board b)
  (render-board b (hash->list col-hash)))

; Int -> Image
(define (render-text n)
  (text (if n
            (number->string n)
            "") TEXT-SIZE 'blue)) 
        

;; Set up column, row and block hash maps
(for ([n COLS])
  (hash-set! col-hash n
             (for/list ([r ROWS])
               (string-append r (number->string n)))))

(for ([r ROWS])
  (hash-set! row-hash r
             (for/list ([n COLS])
               (string-append r (number->string n)))))

(define (blocks)
  (for*/list ([r  '(("A" "B" "C") ("D" "E" "F") ("G" "H" "I"))]
              [c '((1 2 3) (4 5 6) (7 8 9))])
    (for*/list ([a r]
                [b c])
      (string-append a (number->string b)))))

(for* ([b (blocks)]
       [e b])
  (hash-set! block-hash e b))
;;

; (cons Pos Int) -> 
; Create a board from the values in alist, where alist represents the positions
; in the board that have been filled, along with their values
(define (create-board alist)
  (make-immutable-hash (append alist (for*/list ([r ROWS]
                           [c COLS]
                           #:when (not (assoc (string-append r (number->string c)) alist)))
                                       (cons (string-append r (number->string c)) #f))))) 

; Board -> Board
; Finds a solution to board
(define (solve-board board)
  (cond [(is-solved? board) board]
        [else (define boards (add-nos board))
              (if boards
                  (solve-boards boards)
                  #f)]))

; [List-of Board] -> Maybe Board
; works through boards to find a valid solution
(define (solve-boards boards)
  (cond [(empty? boards) #f]
        [else (define sol (solve-board (first boards)))
              (if sol
                  sol
                  (solve-boards (rest boards)))]))

; Board -> [List-of Board]
; generates a list of boards from board, where every valid no
; is added to the first open position
(define (add-nos board)
  (define open (open-pos board))
  (if open
      (for/list ([n (build-list 9 (λ (x) (+ x 1)))]
                 #:when (can-add? n open board))
        (hash-set board open n))
      #f))

; Board -> Pos
; finds the first open position in board 
(define (open-pos board)
  (for/first ([k (hash-keys board)]
              #:when (false? (hash-ref board k)))
    k))

; Int Pos Board -> Boolean
; checks if n can be added to open
(define (can-add? n open board)
  (define block (hash-ref block-hash open board))
  (define col (hash-ref col-hash (- (char->integer (string-ref open 1)) 48)))
  (define row (hash-ref row-hash (~a (string-ref open 0))))
  (and (boolean? (hash-ref board open)))
  (not (or (member n (for/list ([b block])
                       (hash-ref board b)))
           (member n (for/list ([c col])
                       (hash-ref board c)))
           (member n (for/list ([r row])
                       (hash-ref board r))))))


; Board -> Boolean
(define (is-solved? board)
  (cond [(andmap (λ (x) (not (boolean? x))) (hash-values board))]
        [else (define rs (hash-values row-hash))
              (define cs (hash-values col-hash))
              (define bs (hash-values block-hash))
              (for/and ([row rs]
                        [col cs]
                        [blo bs])
                (and (complete (get-vals board row))
                     (complete (get-vals board col))
                     (complete (get-vals board blo))))]))

; Board [List-of Pos] -> [List-of Int]
; gets all values in board from keys in ls
(define (get-vals board ls)
  (for/list ([v ls])
    (hash-ref board v)))


; [List-of Int] -> Boolean
; checks if ls contains every no. from 1 -> 9
(define (complete ls)
  (set=? COLS ls))

(define ex (make-immutable-hash '(("A1" . 5) ("B1". 6) ("C1". #f) ( "D1". 8) ( "E1". 4) ( "F1". 7)
                                             ( "G1". #f) ( "H1". #f) ( "I1". #f)
                                             ( "A2". 3) ( "B2". #f) ( "C2". 9) ( "D2". #f) ( "E2". #f) ( "F2". #f)
                                             ( "G2". 6) ( "H2". #f) ( "I2". #f)
                                             ( "A3". #f) ( "B3". #f) ( "C3". 8) ( "D3". #f) ( "E3". #f) ( "F3". #f)
                                             ( "G3". #f) ( "H3". #f) ( "I3". #f)
                                             ( "A4". #f) ( "B4". 1) ( "C4". #f) ( "D4". #f) ( "E4". 8) ( "F4". #f)
                                             ( "G4". #f) ( "H4". 4) ( "I4". #f)
                                             ( "A5". 7) ( "B5". 9) ( "C5". #f) ( "D5". 6) ( "E5". #f) ( "F5". 2)
                                             ( "G5". #f) ( "H5". 1) ( "I5". 8)
                                             ( "A6". #f) ( "B6". 5) ( "C6". #f) ( "D6". #f) ( "E6". 3) ( "F6". #f)
                                             ( "G6". #f) ( "H6". 9) ( "I6". #f)
                                             ( "A7". #f) ( "B7". #f) ( "C7". #f) ( "D7". #f) ( "E7". #f) ( "F7". #f)
                                             ( "G7". 2) ( "H7". #f) ( "I7". #f)
                                             ( "A8". #f) ( "B8". #f) ( "C8". 6) ( "D8". #f) ( "E8". #f) ( "F8". #f)
                                             ( "G8". 8) ( "H8". #f) ( "I8". 7)
                                             ( "A9". #f) ( "B9". #f) ( "C9". #f) ( "D9". 3) ( "E9". 1) ( "F9". 6)
                                             ( "G9". #f) ( "H9". 5) ( "I9". 9))))

(define ex2 (make-immutable-hash '(("A1" . #f) ("B1". 7) ("C1". #f) ( "D1". 3) ( "E1". #f) ( "F1". 8)
                                             ( "G1". #f) ( "H1". #f) ( "I1". #f)
                                             ( "A2". 3) ( "B2". #f) ( "C2". 8) ( "D2". #f) ( "E2". 5) ( "F2". #f)
                                             ( "G2". #f) ( "H2". #f) ( "I2". #f)
                                             ( "A3". #f) ( "B3". #f) ( "C3". #f) ( "D3". 7) ( "E3". #f) ( "F3". #f)
                                             ( "G3". 6) ( "H3". #f) ( "I3". 8)
                                             ( "A4". #f) ( "B4". #f) ( "C4". 6) ( "D4". #f) ( "E4". #f) ( "F4". 2)
                                             ( "G4". #f) ( "H4". 9) ( "I4". #f)
                                             ( "A5". #f) ( "B5". #f) ( "C5". 2) ( "D5". #f) ( "E5". #f) ( "F5". #f)
                                             ( "G5". 5) ( "H5". #f) ( "I5". #f)
                                             ( "A6". #f) ( "B6". 4) ( "C6". #f) ( "D6". 9) ( "E6". #f) ( "F6". #f)
                                             ( "G6". 1) ( "H6". #f) ( "I6". #f)
                                             ( "A7". 4) ( "B7". #f) ( "C7". 1) ( "D7". #f) ( "E7". #f) ( "F7". 9)
                                             ( "G7". #f) ( "H7". #f) ( "I7". #f)
                                             ( "A8". #f) ( "B8". #f) ( "C8". #f) ( "D8". #f) ( "E8". 4) ( "F8". #f)
                                             ( "G8". 2) ( "H8". #f) ( "I8". 9)
                                             ( "A9". #f) ( "B9". #f) ( "C9". #f) ( "D9". 2) ( "E9". #f) ( "F9". 6)
                                             ( "G9". #f) ( "H9". 8) ( "I9". #f))))

; Board -> Board
(define (main b)
  (big-bang b
            (to-draw draw-board)
            (on-key keyh)))

; Board KeyEvent -> Board
(define (keyh b k)
  (solve-board b))
