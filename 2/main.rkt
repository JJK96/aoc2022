#lang racket

(require util)

(define num-options 3)

(define (to-num shape)
  (match shape
    [#\A 0]
    [#\B 1]
    [#\C 2]
    [#\X 0]
    [#\Y 1]
    [#\Z 2]))

(define (shape-points shape)
  (add1 shape))

(define (get-winning shape)
  (modulo (add1 shape) num-options))

(define (get-losing shape)
  (modulo (sub1 shape) num-options))

(define (round-points me opponent)
  (cond 
      [(= opponent (get-winning me)) 0]
      [(= opponent (get-losing me)) 6]
      [else 3]))

(define (round-points2 outcome)
  (match outcome
         [#\X 0]
         [#\Y 3]
         [#\Z 6]))

(define (get-shape opponent outcome)
  (match outcome
         [#\X (get-losing opponent)]
         [#\Y opponent]
         [#\Z (get-winning opponent)]))

(define (parse-line line)
  (values (to-num (ref line 0))
          (to-num (ref line 2))))

(define (parse-line1 line)
  (values (to-num (ref line 0))
          (ref line 2)))

(define (part1 input)
  (with-input-from-file input
    (lambda () 
      (for/sum ([line (in-lines)])
         (let-values ([(opponent me) (parse-line line)])
           (+ (shape-points me)
              (round-points me opponent)))))))

(define (part2 input)
  (with-input-from-file input
    (lambda () 
      (for/sum ([line (in-lines)])
         (let-values ([(opponent outcome) (parse-line1 line)])
           (+ (shape-points (get-shape opponent outcome))
              (round-points2 outcome)))))))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
