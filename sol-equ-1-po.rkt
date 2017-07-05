;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sol-equ-1-po) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; sol-equ-1-un : solving the equations with only 1-power values with power 1

  (define (eli lofa lofb)
    (miner (list (map (lambda (x) (* (first (first lofb)) x)) (first lofa))
                 (* (first (first lofb)) (second lofa)))
           (list (map (lambda (x) (* (first (first lofa)) x)) (first lofb))
                 (* (first (first lofa)) (second lofb)))))
(define (miner lofa lofb)
  (local [(define (tt loa lob)
            (cond [(empty? loa) empty]
                  [else (cons (- (first loa) (first lob))
                              (tt (rest loa) (rest lob)))]))]
    (list (rest (tt (first lofa) (first lofb))) (- (second lofa) (second lofb)))))
(define (sep lot)
  (cond [(empty? (rest lot)) empty]
        [else (cons (eli (first lot) (second lot))
                    (sep (rest lot)))]))
(define (mul loa lob)
  (cond [(empty? lob) 0]
        [else (+ (* (first loa) (first lob))
                 (mul (rest loa) (rest lob)))]))
(define (apply-1 f sol)
  (cond [(or (string? sol)
             (symbol? sol)) sol]
        [else
         (/ (- (second f) (mul (rest (first f)) sol)) (first (first f)))]))
(define (f-l lot)
  (cond [(= (first (first (first lot))) 0) (f-l (rest lot))]
        [else (first lot)]))


(define (sol-equ-1-po lot)
  (cond [(empty? (rest lot))
         (cond [(and (equal? (first (first lot)) (list 0))
                     (not (equal? (second (first lot)) 0))) 'error]
               [(equal? (first (first lot)) (list 0)) "infinite solutions"]
               [else (list (/ (second (first lot))
                              (first (first (first lot)))))])]
        [else (cons (apply-1 (f-l lot) (sol-equ-1-po (sep lot)))
                    (sol-equ-1-po (sep lot)))]))

