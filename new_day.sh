#!/usr/bin/env bash
highest=$(ls | grep -E "\d+" | sort | tail -n 1)
new_dir=$(echo "$highest+1" | bc)
mkdir -p $new_dir
cd $new_dir
touch input
touch example.input
cat > main.rkt << EOF
#lang racket
(require util)

(define (part1 [input "example.input"])
  (with-input-from-file input
     (lambda ()
        "TODO")))

(define (part2 [input "example.input"])
  (with-input-from-file input
     (lambda ()
        "TODO")))

(printf "Part 1: ~a\n" (part1 "input"))
(printf "Part 2: ~a\n" (part2 "input"))
EOF
