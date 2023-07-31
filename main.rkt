#!/usr/bin/env racket

#lang racket

(require "dlist.rkt")

(define main
  (let* (
         (dl1 (->dlist '(1 2 3 4 5 6 7 8 9)))
         (dl2 (->dlist '(4 5 6 7 8 9 0 1 2)))
         (dl3 (->dlist '(9 8 7 6 5 4 3 2 1)))
         (dl4 (->dlist '(9 8 7 6 5 4 3 2 1)))
         (dl5 (->dlist '(0 0 0 0 0 0 0 0 0)))
         (dl (dlist-reverse (dlist-append dl1 dl2 )))
         (dl2 (dlist-append dl3 dl4))
         )
    
    (begin
      (dlist-foldr
       (lambda (val el acc)
         (printf "El: ~a : ~a \n" val el))
        (void) dl (dlist-reverse dl2))
      )))

main
