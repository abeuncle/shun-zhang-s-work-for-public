;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname math) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; helpers:
;; l -last element of a list
;; wt-l list without the last
;; rvs --- reverse a list
;; count--- count elements' number in a list
;; rank --- give the rank of elements in a list
;; big--- bigest                big-in ------ number is in list of list
;; sma --- smallest             sma-in ------ number is in list of list
;; pio ---- smooth list into ranking order sma---big
;; ce ----- count one element in a list
;; factor --- factor of a positive integer
;; rmv-dup ---- remove duplicates in a list
;; negate--- negate a list of number 
;; plus-negate --- append a list with its negation
;; gcd-list ---- gcd of a list of numbers
;; lists-equiv? ----- true if all elements in both list equal
;; lists-include? ------- true if every element in i is in o


(define (l a)
  (cond [(empty? (rest a)) (first a)]
        [else (l (rest a))]))
(define (wt-l a)
  (rvs (rest (rvs a))))
(define (rvs a)
  (reverse a))
(define (big x)
  (cond [(empty? (rest x)) (first x)]
        [(< (first x) (second x)) (big (rest x))]
        [else (big (cons (first x) (rest (rest x))))]))
(define (big-in x)
  (cond [(empty? (rest x)) (first x)]
        [(< (first (first x)) (first (second x))) (big-in (rest x))]
        [else (big-in (cons (first x) (rest (rest x))))]))
(define (wt-b x)
  (cond [(empty? x) empty]
        [(cons? (first x))
         (cond [(equal? (big-in x) (first x))
                (rest x)]
               [else (cons (first x) (wt-b (rest x)))])]
        [else (cond [(equal? (first x) (big x))
                     (rest x)]
                    [else (cons (first x) (wt-b (rest x)))])]))
(define (sma x)
  (cond [(empty? (rest x)) (first x)]
        [(< (first x) (second x)) (sma (cons (first x) (rest (rest x))))]
        [else (sma (rest x))]))
(define (sma-in x)
  (cond [(empty? (rest x)) (first x)]
        [(< (first (first x)) (first (second x))) (sma-in (cons (first x) (rest (rest x))))]
        [else (sma-in (rest x))]))
(define (wt-s x)
  (cond [(empty? x) empty]
        [(cons? (first x))
         (cond [(equal? (sma-in x) (first x))
                (rest x)]
               [else (cons (first x) (wt-s (rest x)))])]
        [else (cond [(equal? (first x) (sma x))
                     (rest x)]
                    [else (cons (first x) (wt-s (rest x)))])]))
(define (pio-s x)
  (cond [(empty? x) empty]
        [(cons? (first x))
         (cons (sma-in x) (pio-s (wt-s x)))]
        [else (cons (sma x) (pio-s (wt-s x)))]))
(define (pio-b x)
  (cond [(empty? x) empty]
        [(cons? (first x))
         (cons (big-in x) (pio-b (wt-b x)))]
        [else (cons (big x) (pio-b (wt-b x)))]))
(define (pio x sign)
  (cond [(equal? sign 's) (pio-s x)]
        [(equal? sign 'b) (pio-b x)]))
(define (ce e l)
  (foldl (lambda (x t) (cond [(equal? x e) (+ 1 t)]
                             [else t])) 0 l))
(define (rmv-dup lon)  
  (foldl (lambda (n l)  
           (cond [(member? n l) l]  
                 [else (foldr cons (list n) l)]))  
         empty lon))
(define (negate lon)
  (map - lon))
(define (plus-negate lon)
  (append (map - lon) lon))
(define (gcd-list lon)
  (foldl gcd (first lon) (rest lon)))
(define (lists-equiv? a b)
  (cond [(and (empty? a) (empty? b)) true]
        [(member? (first a) b)
         (lists-equiv? (rest a) (remove (first a) b))]
        [else false]))
(define (lists-include? i o)
  (andmap (lambda (x) (member? x o)) i))



;; cd --- count down continuous natural number to 1
;; count ---- from ->to
(define (count f t)
  (cond [(> f t) empty]
        [else (cons f (count (+ f 1) t))]))
(define (cd t)
  (count 1 t))

;; digit ---- make a rational number into digital form
(define (digit n)
  (string->list (number->string n)))
(define (num-frq n)
  (map (lambda (t) (cons t (list (ce t (digit n))))) (cons #\0 (digit 123456789))))
;; int ---- just produce the integer part of a number
(define (int x)
  (cond [(integer? x) x]
        [(member? #\. (digit x))
         (local [(define (pre loc)
                   (foldr (lambda (e l)
                            (cond [(equal? e #\.) empty]
                                  [else (cons e l)]))empty loc))]
           (string->number
            (list->string (pre (digit x)))))]
        [else (quotient (numerator x) (denominator x))]))

;; div? ---  divisible?
;; div-l? ---- divisible? in a list
;; ldiv --- disvisible item in a group
(define (div? a b)
  (and (integer? a)
       (integer? b)
       (integer? (/ a b))))
(define (div-l? a l)
  (cond [(empty? l) false]
        [(= a 1) false]
        [(= a 0) true]
        [(div? a (first l)) true]
        [else (div-l? a (rest l))]))
(define (ldiv a b)
  (cond [(empty? b) empty]
        [(div? a (first b)) (cons (first b) (ldiv a (rest b)))]
        [else (ldiv a (rest b))]))


;; lg ---- log b(base) x
;; ln ---- ln x
(define (lg b x)
  (/ (log x) (log b)))
(define (ln x)
  (lg e x))

;; factors -----  produces list of factors of x

(define (factors x)
  (filter (lambda (t) (div? x t)) (cd x)))

;; p --- prime?
(define (p x)
  (local [(define (pp x y)
            (cond [(= x y) true]
                  [(div? x y) false]
                  [else (pp x (+ 2 y))]))]
    (cond [(= x 2) true]
          [(< x 2) false]
          [(div? x 2) false]
          [else (pp x 3)])))

;; extended euclid algorithm-- eea
(define (eea a b)
  (local [(define (q a b)
            (/ (- a (remainder a b)) b))
          (define (make-q-r a b)
            (list a b 'q (q a b) 'r (remainder a b)))]
    (cond [(< (abs a) (abs b)) (cons (make-q-r a b) (eea b a))]
          [(= (remainder a b) 0) (cons (make-q-r a b) empty)]
          [else (cons (make-q-r a b) (eea b (remainder a b)))])))

;; mod
;; a=b mod m? --- mod?
(define (mod a b m)
  (div? (- a b) m))

;; find the mod form of an equation
;; f->function   r-> before mod    m-> after mod
(define (sol-mod f r m)
  (filter (lambda (x) (mod (f x) r m))
          (cons 0 (reverse (rest (reverse (cd m)))))))

;; chinese remainder theorem
(define (ch a b x y)
  (local [(define (pre-ch n a b x y)
            (cond [(and (mod n a b) (mod n x y)) n]
                  [(> n (/ (* b y) (gcd b y))) false]
                  [else (pre-ch (add1 n) a b x y)]))]
    (pre-ch 0 a b x y)))
(define (ch-list a-l b x-l y)
  (local [(define (pre-chl a-l b x y)
            (cond [(empty? a-l) empty]
                  [(number? (ch (first a-l) b x y))
                   (cons (ch (first a-l) b x y)
                         (pre-chl (rest a-l) b x y))]
                  [else (pre-chl (rest a-l) b x y)]))]
    (cond [(empty? x-l) empty]
          [else (append (pre-chl a-l b (first x-l) y)
                        (ch-list a-l b (rest x-l) y))])))
(define (f-ch f a b g x y)
  (cond [(and (cons? (sol-mod f a b))
              (cons? (sol-mod g x y)))
         (ch-list (sol-mod f a b) b
      (sol-mod g x y) y)]
        [else false]))



; rpcl---reciprocal
; sum-rpcl ----- sum of continous reciporocal
(define (rpcl x)
  (/ 1 x))
(define (sum-rpcl x)
  (cond [(< x 0) (sum-rpcl (* -1 x))]
        [(= x 0) 0]
        [else (+ (rpcl x) (sum-rpcl (sub1 x)))]))

; factorial--- multiplation by cintinuous natural numbers
(define (factorial x)
  (foldl *  1 (cd x)))

(define (npr n k)
  (cond [(= k 0) 1]
        [else (* n (npr (sub1 n) (sub1 k)))]))

(define (ncr n k)
  (cond [(> k (- n k)) (ncr n (- n k))]
        [else (/ (npr n k) (factorial k))]))

(define (binomial t n p)
  (* (ncr t n) (expt p n) (expt (- 1 p) (- t n))))

(define (E f lon)
  (foldr (lambda (e l) (+ (f e) l)) 0 lon))
;; de-plnml: presents the division of nu(numerator)
;; by de(denominator)
;; each list of number means the coefficient and the order
;; means their power with minus one
(define (de-plnml nu de)
  (local [(define (tt nu de)
            (/ (first nu) (first de)))
          (define (l-ll nu de t)
            (cond [(empty? de) nu]
                  [else (cons (- (first nu) (* t (first de)))
                              (l-ll (rest nu) (rest de) t))]))
          (define (all-0 lon)
            (empty? (filter (lambda (x) (not (= x 0))) lon)))]
    (cond [(or (empty? nu)
               (all-0 nu)) empty]
          [(> (length de) (length nu))
           (cons 'remainder nu)]
          [else (cons (tt nu de)
                      (de-plnml (l-ll (rest nu)
                                   (rest de) (tt nu de)) de))])))
;; add-plnml: addition of polynomial presented in a and b
(define (add-plnml a b)
  (local [(define l-a (length a))
          (define l-b (length b))
          (define (ag la lb le)
            (cond [(= le 0) empty]
                  [else (cons (+ (first la) (first lb))
                              (ag (rest la) (rest lb) (- le 1)))]))
          (define (af la lea lb leb)
            (cond [(= lea leb)
                   (ag la lb lea)]
                  [else (cons (first la) (af (rest la) (- lea 1) lb leb))]))]
    (cond [(> l-a l-b) (af a l-a b l-b)]
          [else (af b l-b a l-a)])))

;; mul-plnml: multiplication of polynomial presented in a and b
(define (mul-plnml a b)
  (local [(define l (length b))
          (define (s-a l-a l-b )
            (cond [(empty? l-a) l-b]
                  [else (cons (+ (first l-a) (first l-b))
                              (s-a (rest l-a) (rest l-b)))]))
          (define (nm lst num)
            (map (lambda (x) (* x num)) lst))
          (define (pre la lb le)
            (cond [(= le 1) (nm la (first lb))]
                  [else (local [(define lst (nm la (first lb)))]
                          (cons (first lst) (s-a (rest lst) (pre la (rest lb) (- le 1)))))]))]
    (pre a b l)))

;; make-plnml: make a function with the order of polynomial
(define (make-plnml lon)
  (local[(define le (- (length lon) 1))
         (define (make-f lon x l)
           (cond [(= l 0)
                  (first lon)]
                 [else (+ (* (first lon) (expt x l))
                          (make-f (rest lon) x (- l 1)))]))]
    (lambda (x) (make-f lon x le))))

;; rational-roots ---find rational roots of a function
(define (rational-roots lon)
  (local [;; pos-rs ---- possible rational roots
          (define (pre-div lon a)
            (map (lambda (x) (/ a x)) lon))
          (define (pos-rs a b)
            (rmv-dup (foldr append empty (map (lambda (x) (pre-div a x)) b))))]
    (filter (lambda (x) (= ((make-plnml lon) x) 0))
            (plus-negate (pos-rs (factors (abs (first lon)))
                                 (factors (abs (l lon))))))))
;; factor-plnml ------ factor a polynomial
(define (factor-plnml p)
  (local [(define (m-t n)
            (list (denominator n) (- (numerator n))))
          (define (pre p)
            (cond [(or (empty? p)
                       (empty? (rest p))) empty]
                  [(or (empty? (rest (rest p)))
                       (empty? (rational-roots p)))
                   (cons p empty)]
                  [else (append (map m-t (rational-roots p))
                                (pre (foldl (lambda (t y) (de-plnml y t))
                                            p (map m-t (rational-roots p)))))]))]
    (cond [(= (gcd-list p) 1) (pre p)]
          [else (cons (gcd-list p) (pre (map (lambda (x) (/ x (gcd-list p))) p)))])))



; power ---- divisior(b)'s  power in a
; l-div-p ----- list contains a divisor and its power
; lcon ---- list of disvisors and their powers
;           if 1, then empty
; de-lcon----- lcon -> integer
; e-lcon ----- new list of {lcon, t x m)}

(define (power x y)
  (cond [(integer? (/ x y)) (+ 1 (power (/ x y) y))]
        [else 0]))
(define (lcon x)
  (local [(define (m x y l)
            (cond [(= x 1) empty]
                  [(> y l) (cons (list x 'expt 1) empty)]
                  [(integer? (/ x y))
                   (local [(define p (power x y))
                           (define le (/ x (expt y p)))]
                     (cons (list y 'expt p) (m le (+ 2 y)  (+ 1 (sqrt le)))))]
                  [else (m x (+ 2 y) l)]))
          (define (t x y)
            (cond [(integer? (/ x y))
                   (local [(define p (power x y))
                           (define le (/ x (expt y p)))]
                     (cons (list y 'expt p) (m le (+ 1 y) (sqrt le))))]
                  [else (m x (add1 y) (sqrt x))]))]
    (cond [(= x 1) (list (list 1 'expt 1))]
          [(= x 0) (list (list 0 'expt 1))]
          [(< x 0)
           (cons (list -1 'expt 1) (lcon (* -1 x)))]
          [(integer? x) (t x 2)]
          [else empty])))
(define (de-lcon a-l)
  (cond [(empty? a-l) 1]
        [else (* (expt (first (first a-l)) (l (first a-l))) (de-lcon (rest a-l)))]))
(define (e-lcon x y)
  (local[(define (pre-e-lcon x b)
           (cond [(not (div? x b)) (lcon x)]
                 [else (list (lcon (/ x (expt b (power x b)))) (list b 'expt (power x b)))]))
         (define (b-pre-e-lcon x y)
           (cond [(empty? y) (lcon x)]
                 [(= x 1) empty]
                 [(= x 0) empty]
                 [(not (div? x (first y))) (e-lcon x (rest y))]
                 [else (pio (append (list (l (pre-e-lcon x (first y))))
                                    (e-lcon (de-lcon (first (pre-e-lcon x (first y)))) (rest y))) 's)]))]
    (cond [(or (= x 1) (= x 0)) (lcon x)]
          [else (b-pre-e-lcon x y)])))
(define (all-lcon a)
  (cond [(integer? a) (lcon a)]
        [(rational? a) (append (lcon (* a (denominator a))) (list 'de) (lcon (denominator a)))]))
; p-or-lcon --- produces lcon if not prime or irrational
(define (p-or-lcon x)
  (cond [(p x) 'prime]
        [(member? #\. (digit x))
         'irrational]
        [else (all-lcon x)]))


