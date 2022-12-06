#lang racket
(require util)
(require threading)

(define (all-different buffer)
  (for/and ([x buffer])
     (= 1 (for/sum ([y buffer])
             (if (= x y) 1 0)))))

(define (find-marker length buffer [processed 0])
  (if (all-different (take buffer length))
      (+ processed length)
      (find-marker length (rest buffer) (+ processed 1))))

(define (find-start-of-packet buffer)
  (find-marker 4 buffer))

(define (find-start-of-message buffer)
  (find-marker 14 buffer))

(define (solve find-func)
  (~> "input"
      open-input-file
      port->string
      string->list
      find-func))
            
(define (part1)
  (solve find-start-of-packet))

(define (part2)
  (solve find-start-of-message))

(printf "Part 1: ~a\n" (part1))
(printf "Part 2: ~a\n" (part2))
