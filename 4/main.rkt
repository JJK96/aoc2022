#lang racket
(require util)

(define (parse-range range)
  (map string->number (string-split range "-")))

(define (parse-line line)
  (map parse-range (string-split line ",")))

(define (fully-contains line)
  (match-let ([(list (list start1 end1)
                     (list start2 end2))
               (parse-line line)])
    (or (and (<= start1 start2)
             (>= end1 end2))
        (and (<= start2 start1)
             (>= end2 end1)))))

(define (overlap? line)
  (match-let ([(list (list start1 end1)
                     (list start2 end2))
               (parse-line line)])
    (or (and (<= start1 start2)
             (>= end1 start2))
        (and (<= start2 start1)
             (>= end2 start1))
        (and (<= start2 end1)
             (>= end2 end1))
        (and (<= start1 end2)
             (>= end1 end2)))))

(define (part1 input)
  (with-input-from-file input
    (lambda ()
      (for/sum ([line (in-lines)]
                #:when (fully-contains line))
        1))))

(define (part2 input)
  (with-input-from-file input
    (lambda ()
      (for/sum ([line (in-lines)]
                #:when (overlap? line))
        1))))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
