#lang racket
(require util)

(struct dir (children parent))
(struct file (size))

(define totalspace 70000000)
(define neededspace 30000000)

(define (cd parent childname)
  (match-let* ([(dir children _) parent])
    (ref children childname)))
  
(define (ls x children)
  (match-let ([(dir old-children parent) x])
    (define new-children 
      (for/hash ([key (hash-keys children)])
         (values key (if (hash-has-key? old-children key)
                        (ref old-children key)
                        (ref children key)))))
    (dir new-children parent)))

(define (get-root x)
  (define parent (dir-parent x))
  (if (null? parent)
    x
    (get-root parent)))

(define (parse-input)
  (define filesystem (dir (make-hash) null))
  (for ([line (in-lines)])
    (match (string-split line " ")
        [(list "$" "cd" "/") empty]
        [(list "$" "cd" "..") (match-let ([(dir _ parent) filesystem])
                                  (set! filesystem parent))]
        [(list "$" "cd" dirname) (set! filesystem (cd filesystem dirname))]
        [(list "dir" name) (hash-set! (dir-children filesystem) name (dir (make-hash) filesystem))]
        [(list "$" "ls") empty]
        [(list size filename) (hash-set! (dir-children filesystem) 
                                         filename 
                                         (file (string->number size)))]))
  (get-root filesystem))

(define (get-size x [callback (lambda (size) null)])
  (define size
    (for/sum ([child (hash-values (dir-children x))])
       (match child
          [(file size) size]
          [(dir _ _) (get-size child callback)])))
  (callback size)
  size)

(define (part2 [input "example.input"])
  (with-input-from-file input
     (lambda ()
        "TODO")))

(define (part1 [input "example.input"])
  (with-input-from-file input
     (lambda ()
        (define filesystem (parse-input))
        (define running-sum 0)
        (define (callback size)
          (if (<= size 100000)
            (set! running-sum (+ running-sum size))
            '()))
        (get-size filesystem callback)
        running-sum)))

(printf "Part 1: ~a\n" (part1 "input"))
;(printf "Part 2: ~a\n" (part2 "input"))
