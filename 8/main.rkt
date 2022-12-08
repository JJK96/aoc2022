#lang racket
(require util)

(define up '(-1 . 0))
(define down '(1 . 0))
(define left '(0 . -1))
(define right '(0 . 1))

(define (get-tree grid coordinates)
  (match-let ([(cons y x) coordinates])
    (ref (ref grid y) x)))

(define (add-direction pos direction)
  (cons (+ (car pos) (car direction))
        (+ (cdr pos) (cdr direction))))

(define (get-visible-trees-direction grid current direction [visible-trees (set)] [highest -1])
  (with-handlers ([exn:fail:contract? (lambda (e) visible-trees)])
    (define height (get-tree grid current))
    (define next (add-direction current direction))
    (if (height . > . highest)
        (get-visible-trees-direction grid next direction (set-add visible-trees current) height)
        (get-visible-trees-direction grid next direction visible-trees highest))))

(define (grid-size grid)
  (define height (vector-length grid))
  (define width (vector-length (ref grid 0)))
  (cons height width))

(define (get-visible-trees grid)
  (match-let ([(cons height width) (grid-size grid)])
    (define visible-trees (mutable-set))
    (for ([y (in-range height)])
       (set-union! 
          visible-trees
          (get-visible-trees-direction grid (cons y 0) right)
          (get-visible-trees-direction grid (cons y (sub1 width)) 
                                            left)))
    (for ([x (in-range width)])
       (set-union! 
          visible-trees
          (get-visible-trees-direction grid (cons 0 x) down)
          (get-visible-trees-direction grid (cons (sub1 height) x)
                                            up)))
    visible-trees))

(define (parse-grid)
  (for/vector ([line (in-lines)])
    (for/vector ([char line])
       (char->number char))))

(define (get-viewing-distance grid coordinates direction [distance 0] [height 0])
  (define my-height (if (= 0 height)
                     (get-tree grid coordinates)
                     height))
  (with-handlers ([exn:fail:contract? (lambda (e) distance)])
    (define next (add-direction coordinates direction))
    (if ((get-tree grid next) . >= . my-height)
        (add1 distance)
        (get-viewing-distance grid next direction (add1 distance) my-height))))

(define (scenic-score grid coordinates)
  (apply * (for/list ([direction (list up down left right)])
              (get-viewing-distance grid coordinates direction))))

(define (part1 [input "example.input"])
  (with-input-from-file input
    (lambda ()
        (define grid (parse-grid))
        (set-count (get-visible-trees grid)))))

(define (part2 [input "example.input"])
  (with-input-from-file input
     (lambda ()
        (define grid (parse-grid))
        (match-let ([(cons height width) (grid-size grid)])
          (apply max (for*/list ([y (in-range height)]
                                 [x (in-range width)])
                          (scenic-score grid (cons y x))))))))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
