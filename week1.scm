; 2. Write a procedure squares that takes a sentence of numbers as
; its argument and returns a sentence of the squares of the numbers:
; > (squares ’(2 3 4 5)) (4 9 16 25)

(define (squares numbers) (square-numbers numbers '()))

(define (square-numbers numbers acc)
    (if (null? numbers)
        acc
        (square-numbers (cdr numbers) (append acc (list (square (first numbers)))))))

(define (square x) (* x x))

; 3. Write a procedure switch that takes a sentence as its
; argument and returns a sentence in which every instance of
; the words I or me is replaced by you, while every instance
; of you is replaced by me except at the beginning of the
; sentence, where it’s replaced by I.
; (Don’t worry about capitalization of letters.)
; Example:
; > (switch ’(You told me that I should wake you up))
; (i told you that you should wake me up)

(define (switch sentence)
    (switch-sentence (reverse sentence)))

(define (switch-sentence sentence '()))

(define (switch-word word)
    (cond (equal? word 'I)))
