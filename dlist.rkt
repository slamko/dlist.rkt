#!/usr/bin/env racket

#lang racket

(define (->dlist lst)
  (lambda (tail)
    (append lst tail)))

(define (dlist/append dlist1 dlist2)
  (lambda (tail)
    (dlist1 (dlist2 tail))))

(define (->list dlist)
  (dlist '()))

(define (empty)
  (->dlist '()))

;; (define (length dlist)
  

(define main
  (let* ((dl1 (->dlist '(1 2 3)))
         (dl2 (->dlist '(4 5 6)))
         (dl (dlist/append dl1 dl2))
         (l (->list dl))
        )

    (map
     (lambda (val)
       (printf "hello: ~a \n" val))
     l)))

main
