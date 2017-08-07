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
    (switch-sentence (reverse sentence) '()))

(define (switch-sentence sentence acc)
    (if (null? sentence)
        acc
        (switch-sentence
            (cdr sentence)
            (cons (switch-word (first sentence) (eq? (length sentence) 1)) acc)) ))

(define (switch-word word first-word?)
    (cond ((eq? word 'I) 'you)
          ((eq? word 'me) 'you)
          ((and first-word? (eq? word 'you)) 'I)
          ((eq? word 'you) 'me)
          (else word)))

; 4. Write a predicate ordered? that takes a sentence of numbers
; as its argument and returns a true value if the numbers are
; in ascending order, or a false value otherwise.

(define (ordered? numbers)
    (check-order (cdr numbers) (first numbers)))

(define (check-order numbers last-number)
    (if (null? numbers)
      #t
      (if (> last-number (first numbers))
        #f
        (check-order (cdr numbers) (first numbers)))))

; 5. Write a procedure ends-e that takes a sentence as its argument
; and returns a sentence containing only those words of the argument
; whose last letter is E:
; > (ends-e ’(please put the salami above the blue elephant)) (please the above the blue)

(define (ends-e sentence)
    (check-ends-e sentence '()))

(define (check-ends-e sentence acc)
    (if (null? sentence)
        acc
        (if (e-last? (first sentence))
            (check-ends-e (cdr sentence) (append acc (list (first sentence))))
            (check-ends-e (cdr sentence) acc))))

(define (e-last? word)
    (eq? (last (string->list (symbol->string word))) #\e))

; 6. devise a test that will tell you whether Scheme’s and
; and or are special forms or ordinary functions
(define (true-procedure) (= 0 0))
(define (false-procedure) (= 0 1))

(define (or-test procedure) (or (procedure) (or-test procedure)))
(define (and-test procedure) (and (procedure) (and-test procedure)))

; (or-test true-procedure) should return true if Scheme's and are special forms because it won't infinitely loop
; (and-test false-procedure) should return false if Scheme's or are special forms because it won't infinitely loop

; However (or-test false-procedure) and (and-test true-procedure) will infinitely loop


