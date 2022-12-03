#lang racket
(require util)
(require threading)

(define (priority item)
  (if (char-lower-case? item)
      (add1 (- item #\a))
      (+ 27 (- item #\A))))

(define (part1 input)
  (with-input-from-file input
    (lambda ()
      (for/sum ([line (in-lines)])
        (define len (string-length line))
        (define compartment1 
          (~> (substring line 0 (/ len 2))
              string->list))
        (for/first ([x (substring line (/ len 2))]
                    #:when (member x compartment1))
          (priority x))))))

(printf "Part 1: ~a\n" (part1 "input"))
