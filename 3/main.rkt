#lang racket
(require util)

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
          (string->list (substring line 0 (/ len 2))))
        (for/first ([x (substring line (/ len 2))]
                    #:when (member x compartment1))
          (priority x))))))

(define (find-badge elfs)
  (define elfs1 (map string->list elfs))
  (for/first ([item (ref elfs1 2)]
              #:when (and (member item (ref elfs1 0))
                          (member item (ref elfs1 1))))
      item))

(define (part2 input)
  (with-input-from-file input
    (lambda ()
      (for/fold ([elfs '()]
                 [badge-sum 0]
                 #:result badge-sum)
                ([line (in-lines)])
        (define elfs1 (cons line elfs))
        (if (= 3 (length elfs1))
          (values '()
                  (+ badge-sum (priority (find-badge elfs1))))
          (values elfs1
                  badge-sum))))))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
