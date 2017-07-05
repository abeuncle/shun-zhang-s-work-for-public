;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname overflow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(define (overflow n)
  (local [(define t (remainder (- n 2147483648) 4294967296))]
    (cond [(= t 0) -2147483648]
          [(> t 0)
           (- t 2147483648)]
          [else (+ t 2147483648)])))