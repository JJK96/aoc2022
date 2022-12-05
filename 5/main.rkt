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

(with-input-from-file "example.input"
    (lambda ()
      (define drawing-lines (reverse (get-drawing-lines)))
      (define num-stacks (/ (add1 (string-length (first drawing-lines)))
                            4))
      (for/fold ([stacks (build-vector num-stacks (lambda (x) '()))])
                ([line (rest drawing-lines)])
         (for/fold ([new-stacks '()]
                    [line-rest line]
                    #:result new-stacks)
                   ([stack stacks])
             (define crate (parse-crate line-rest))
             (define new-stack 
               (if crate
                  (cons crate stack)
                  stack))
             (values (append new-stacks
                             (list new-stack))
                     (substring line-rest 4))))))
