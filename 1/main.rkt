#lang racket

(define (sum-invs)
  (for/fold ([elfs '()]
             [inventory 0]
             #:result (cons inventory elfs))
            ([x (in-lines)])
       (set! x (string->number x))
       (match x
        [#f (values (cons inventory elfs) 0)]
        [_ (values elfs (+ inventory x))])))

(define (part1 input)
  (with-input-from-file input
       (lambda () (apply max (sum-invs)))))

(define (part2 input)
  (with-input-from-file input
    (lambda () (apply + (take (sort (sum-invs) >) 3)))))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
