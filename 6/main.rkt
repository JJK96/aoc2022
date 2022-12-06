#lang racket
(require util)

(define example1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(define (all-different buffer)
  (for/and ([x buffer])
     (= 1 (for/sum ([y buffer])
             (if (= x y) 1 0)))))

(define (find-start-of-packet buffer [processed 0])
  (if (all-different (take buffer 4))
      (+ processed 4)
      (find-start-of-packet (rest buffer) (+ processed 1))))
            
(define (part1)
  (find-start-of-packet (string->list (port->string (open-input-file "input")))))

(printf "Part 1: ~a\n" (part1))
