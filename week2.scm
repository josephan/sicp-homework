; 1.31 a
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

; factorial using product
(define (factorial i)
    (define (fact-term n) n)
    (define (fact-next n) (+ n 1))
    (product fact-term 1 fact-next i))

; approximation of pi using Wallis product
(define (pi-approx i)
    (define (pi-term n)
        (* (/ (* 2 n)
              (- (* 2 n) 1))
           (/ (* 2 n)
              (+ (* 2 n) 1))))
    (define (pi-next n) (+ n 1))
    (* 2.0 (product pi-term 1 pi-next i)))

; 1.32
; general accumulator
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

; sum and product using general accumulator
(define (accum-sum term a next b) (accumulate + 0 term a next b))
(define (accum-product term a next b) (accumulate * 1 term a next b))


; testing accumumlate
(define (factorial i)
    (define (fact-term n) n)
    (define (fact-next n) (+ n 1))
    (accum-product fact-term 1 fact-next i))

; 1.33
; filtered accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (if (filter a)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))

; simple procedure that determines if given argument is a prime number
(define (prime? n)
   (cond ((<= n 1) #f)
         ((<= n 3) #t)
         ((= (remainder n 2) 0) #f)
         ((= (remainder n 3) 0) #f)
         (else (all-other-primes n 5 2))))

(define (all-other-primes n i w)
    (if (<= (* i i) n)
        (if (= (remainder n i) 0)
            #f
            (all-other-primes n (+ i w) (- 6 w)))
        #t))


(define (sum-of-square-of-primes a b)
    (define (square x) (* x x))
    (define (plus-one x) (+ x 1))
    (filtered-accumulate prime? + 0 square a plus-one b))
