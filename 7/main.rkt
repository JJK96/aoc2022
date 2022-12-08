#lang racket
(require util)
(require racket/class)

(struct dir (children parent))
(struct file (size))

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

(define (parse-ls-output parent)
  (for/hash ([line (in-lines)]
             #:break (= #\$ (ref line 0)))
     (match (string-split line " ")
        [(list "dir" name) (values name (dir (make-hash) parent))]
        [(list size filename) (values filename (file size))])))

(define (parse-input)
  (define filesystem (dir (make-hash) null))
  (for ([line (in-lines)])
    (match (string-split line " ")
        [(list "$" "cd" "/") empty]
        [(list "$" "cd" "..") (match-let ([(dir _ parent) filesystem])
                                  (set! filesystem parent))]
        [(list "$" "cd" dirname) (set! filesystem (cd filesystem dirname))]
        [(list "$" "ls") (set! filesystem (ls filesystem (parse-ls-output filesystem)))]
        [_ (println line)])
    (println line)
    (println (dir-children filesystem)))
  filesystem)

(define (part2 [input "example.input"])
  (with-input-from-file input
     (lambda ()
        "TODO")))

(define (part1 [input "example.input"])
  (with-input-from-file input
     (lambda ()
        (parse-input))))

(define filesystem (part1))
;(printf "Part 1: ~a\n" (part1 "input"))
;(printf "Part 2: ~a\n" (part2 "input"))
