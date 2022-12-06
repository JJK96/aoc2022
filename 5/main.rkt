#lang racket
(require util)

(define (get-drawing-lines)
    (for/list ([line (in-lines)]
               #:break (= line ""))
      line))

(define (parse-crate line)
  (if (= (ref line 0) #\[)
      (ref line 1)
      #f))

(define (parse-drawing)
    (define drawing-lines (reverse (get-drawing-lines)))
    (define num-stacks (/ (add1 (string-length (first drawing-lines)))
                          4))
    (define stacks (build-vector num-stacks (lambda (x) '())))
    (for ([line (rest drawing-lines)])
       (for/fold ([line-rest line])
                 ([i (in-range num-stacks)])
           (define crate (parse-crate line-rest))
           (define stack (ref stacks i))
           (define new-stack 
             (if crate
                (cons crate stack)
                stack))
           (vector-set! stacks i new-stack)
           (substring line-rest 4)))
    stacks)

(define (parse-action line)
   (match-let ([(list amount src dst) 
                (map string->number (rest (regexp-match #px"move (\\d+) from (\\d+) to (\\d+)" line)))])
       (list amount (sub1 src) (sub1 dst))))
                         
(define (process-actions stacks [reverse? #t])
  (for ([line (in-lines)])
     (match-let ([(list amount src dst) (parse-action line)])
         (define stack (ref stacks src))
         (define crates (if reverse? 
                          (reverse (take stack amount))
                          (take stack amount)))
         (vector-set! stacks src (drop stack amount))
         (vector-set! stacks dst (append crates (ref stacks dst)))))
  stacks)
                         
(define (get-tops stacks)
  (list->string (for/list ([stack stacks])
                   (first stack))))
                         
(define (solve [input "example.input"] [reverse? #t])
  (with-input-from-file input
      (lambda ()
         (get-tops (process-actions (parse-drawing) reverse?)))))

(define (part1 [input "example.input"])
  (solve input))
                         
(define (part2 [input "example.input"])
  (solve input #f))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
