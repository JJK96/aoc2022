#lang racket

(require util)

(define (shape-points shape)
  (match shape
    [#\X 1]
    [#\Y 2]
    [#\Z 3]
    [#\A 1]
    [#\B 2]
    [#\C 3]))

(define choose-to-lose
  (make-hash (list 
              '(#\A . #\C)
              '(#\B . #\A)
              '(#\C . #\B))))

(define choose-to-win
  (make-hash (list 
              '(#\A . #\B)
              '(#\B . #\C)
              '(#\C . #\A))))

(define (round-points me opponent)
  (set! me (shape-points me))
  (set! opponent (shape-points opponent))
  (match (- me opponent)
      [0 3]
      [-1 0]
      [2 0]
      [-2 6]
      [1 6]))

(define (round-points2 outcome)
  (match outcome
         [#\X 0]
         [#\Y 3]
         [#\Z 6]))

(define (get-shape opponent outcome)
  (match outcome
         [#\X (ref choose-to-lose opponent)]
         [#\Y opponent]
         [#\Z (ref choose-to-win opponent)]))

(define (part1 input)
  (with-input-from-file input
    (lambda () 
      (for/sum ([line (in-lines)])
         (define opponent (ref line 0))
         (define me (ref line 2))
         (+ (shape-points me)
            (round-points me opponent))))))

(define (part2 input)
  (with-input-from-file input
    (lambda () 
      (for/sum ([line (in-lines)])
         (define opponent (ref line 0))
         (define outcome (ref line 2))
         (+ (shape-points (get-shape opponent outcome))
            (round-points2 outcome))))))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
