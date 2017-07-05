;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #t #t none #f () #t)))
(define (mult-list a b)
  (foldl (lambda (e l)
           (append (rest l) (list (* (first l) e)))) a b))
(define (add-element l)
  (foldl (lambda (a b) (+ a b)) 0 l))
(define (add-list a b)
  (foldl (lambda (e l)
           (append (rest l) (list (+ (first l) e)))) a b))

;; determine the type of the matrix

(define (det-type mat)
  (make-posn (length mat) (length (first mat))))

;; get points by its given posn 

(define (get-points po)
  (local [(define m (posn-x po))
          (define n (posn-y po))
          (define (p a b)
            (build-list n (lambda (l) (make-posn a (+ l 1)))))
          (define (ap l)
            (foldr append empty l))]
    (ap (build-list m (lambda (t) (p (+ t 1) n))))))

;; ;; convert matrix to a list with each value with its corresponding point coordination

(define (matrix->list mat)
  (local [(define r
            (foldr append empty mat))
          (define p (get-points (det-type mat)))]
    (foldl (lambda (e l)
             (append (rest l) (list (list e (first l))))) r p)))

;; get the value of certain point in a matrix

(define (get-val mat p)
  (local [(define l (matrix->list mat))]
    (second (first (filter (lambda (e) (member? p e)) l)))))

;; shift the matrix 

(define (shift mat)
  (local [(define (get-first m)
            (map first m))
          (define (get-left m)
            (map rest m))]
    (cond [(empty? (first mat)) empty]
          [else (cons (get-first mat) (shift (get-left mat)))])))

;; convert a list with each value with its corresponding point coordination to matrix

(define (list->matrix lst)
  (local [(define type (first (first (reverse lst))))
          (define n (posn-y type))
          (define l (map second lst))
          (define (fir a b)
            (cond [(= b 0) empty]
                  [else (cons (first a) (fir (rest a) (- b 1)))]))
          (define (lef a b)
            (cond [(= b 0) a]
                  [else (lef (rest a) (- b 1))]))
          (define (sep a b)
            (cond [(empty? a) empty]
                  [else (cons (fir a b) (sep (lef a b) b))]))]
    (sep l n)))

;; build a matrix with m*n

(define (build-matrix m n lon)
  (cond [(not (= (length lon) (* m n))) "wrong input"]
        [else
         (local
           [(define lop (get-points (make-posn m n)))]
           (list->matrix (foldl (lambda (e l)
                                  (append (rest l) (list (list (first l) e)))) lop lon)))]))

;; matrix addition 

(define (add-matrix a b)
  (foldl (lambda (e l)
           (append (rest l) (list (add-list (first l) e)))) a b))

;; matrix multiplication 

(define (mult-matrix a b)
  (local [(define type (make-posn (posn-x (det-type a)) (posn-y (det-type b))))
          (define (get-row m mat)
            (cond [(= m 1) (first mat)]
                  [else (get-row (- m 1) (rest mat))]))
          (define (get-column n mat)
            (get-row n (shift mat)))
          (define (get-value p)
            (add-element (mult-list (get-row (posn-x p) a) (get-column (posn-y p) b))))]
    (list->matrix (map (lambda (p) (list p (get-value p))) (get-points type)))))

;; row echelon form 
(define (ref m)
  (local [(define limit (min (length m) (length (first m))))
          (define (n-th n l)
            (cond [(= n 1) (first l)]
                  [else (n-th (- n 1) (rest l))]))
          (define (subl la lb a b)
            (cond [(empty? la) empty]
                  [else (cons (- (* b (first la)) (* a (first lb))) (subl (rest la) (rest lb) a b))]))
          (define (new-sub la lb n)
            (subl la lb (n-th n la) (n-th n lb)))
          (define (prepare l m n)
            (cond [(> n limit) m]
                  [(empty? m) (prepare empty l (+ n 1))]
                  [(= (n-th n (first m)) 0)
                   (prepare (cons (first m) l) (rest m) n)]
                  [else
                   (local [(define lst (first m))
                           (define f (n-th n lst))]
                     (cons (map (lambda (e) (/ e f)) lst) 
                           (prepare empty
                                    (append l
                                            (map (lambda (lon) (new-sub lst lon n)) (rest m))) (+ n 1))))]))]
    (prepare empty m 1)))
    
;; row reduced echelon form 
(define (rref m)
  (local [(define (n-th n l)
            (cond [(= n 1) (first l)]
                  [else (n-th (- n 1) (rest l))]))
          (define (get l so)
            (cond [(= (first l) 0) (get (rest l) (+ so 1))]
                  [else so]))
          (define (subl la lb a b)
            (cond [(empty? la) empty]
                  [else (cons (- (* b (first la)) (* a (first lb))) (subl (rest la) (rest lb) a b))]))
          (define (new-sub la lb n)
            (subl la lb (n-th n la) (n-th n lb)))
          (define (a lst lof)
            (local [(define n (get lst 1))]
              (map (lambda (l) (new-sub l lst n)) lof)))
          (define (all-0 l)
            (cond [(empty? l) true]
                  [(= (first l) 0) (all-0 (rest l))]
                  [else false]))
          (define (reduce lof los)
            (cond [(empty? lof) los]
                  [else
                   (local [(define lst (first lof))]
                     (reduce (a lst (rest lof)) (cons lst los)))]))
          (define (sep lof los)
            (cond [(all-0 (first lof)) (sep (rest lof) (cons (first lof) los))]
                  [else (reduce lof los)]))]
    (sep (reverse (ref m)) empty)))

;; add-v :      vector addition between a and b
(define (add-v a b)
  (cond [(empty? a) empty]
        [else (cons (+ (first a) (first b))
                    (add-v (rest a) (rest b)))]))

;; dot-mult :        vector dot multiplication production between a and b
(define (dot-mult a b)
  (cond [(empty? a) 0]
        [else(+  (* (first a) (first b))
                 (dot-mult (rest a) (rest b)))]))


;; cross-mult :        vector cross multiplication production between a and b
(define (cross-mult a b)
  (local [(define a-1 (first a))
          (define b-1 (first b))
          (define (cal a1 a2 b1 b2)
            (- (* a1 b2) (* a2 b1)))
          (define (get-value a1 af la b1 bf lb)
            (cond [(empty? la) (list (cal a1 a-1 b1 b-1) (cal a-1 af b-1 bf))]
                  [else (cons (cal a1 (first la) b1 (first lb))
                              (get-value (first la) af (rest la) (first lb) bf (rest lb)))]))
          (define (get-values la lb)
            (get-value (first la) (first la) (rest la) (first lb) (first lb) (rest lb)))]
    (get-values (rest a) (rest b))))

;; li------- linearly independent

(define (li? lov)
  (local [(define (last l)
            (cond [(empty? (rest l)) (first l)]
                  [else (last (rest l))]))
          (define (no-all-0? l)
            (cond [(empty? l) false]
                  [(= (first l) 0) (no-all-0? (rest l))]
                  [else true]))]
    (no-all-0? (last (rref lov)))))

;; im ------- identity matrix which is n * n
(define (im n)
  (local [(define (make a b)
            ;; make a b make a matrix row with b length and
            ;; all 0 except the a-th entry is 1
            (cond [(= b 0) empty]
                  [(= a 1) (cons 1 (make 0 (- b 1)))]
                  [else (cons 0 (make (- a 1)(- b 1)))]))
          (define (makemore a b)
            ;; make a real matrix from a to b 
            (cond [(> a b) empty]
                  [else (cons (make a b) (makemore (+ a 1) b))]))]
    (makemore 1 n)))

;; append-matrix a b -------- append a & b
(define (append-matrix a b)
  (cond [(empty? a) empty]
        [else (cons (append (first a)(first b))
                    (append-matrix (rest a)(rest b)))]))

;; sol m2 by m1
(define (sol-matrix m1 m2)
  (rref (append-matrix m1 m2)))

;; inverse m 
(define (inverse m)
  (sol-matrix m (im (length m))))

          