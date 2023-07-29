#!/usr/bin/env racket

#lang racket

(define (->dlist lst)
  (lambda (tail)
    (append lst tail)))

(define (dlist-append dlist1 . dlist2)
  (lambda (tail)
    (dlist1
     (foldr (lambda (dl acc)
              (dl acc)) tail dlist2))))

(define (dlist->list dlist)
    (dlist '()))

(define (empty)
  (->dlist '()))

(define (dlist-length dlist)
  (length (dlist->list dlist)))

(define (dlist-reverse dlist)
  (lambda (tail)
    (reverse (dlist tail))))

(define (dlist-car dlist)
  (car (dlist->list dlist)))

(define (dlist-cdr dlist)
  (cdr (dlist->list dlist)))

(define (dlist-ref dlist)
  (list-ref (dlist->list dlist)))

(define main
  (let* (
         (dl1 (->dlist '(1 2 3 4 5 6 7 8 9)))
         (dl2 (->dlist '(4 5 6 7 8 9 0 1 2)))
         (dl3 (->dlist '(9 8 7 6 5 4 3 2 1)))
         (dl4 (->dlist '(9 8 7 6 5 4 3 2 1)))
         (dl5 (->dlist '(0 0 0 0 0 0 0 0 0)))
         (dl (dlist-append dl1 dl2 dl3 dl4 dl5))
         (l (dlist->list dl))
         )
    
    (begin
      (printf "Length: ~a \n" (dlist-length dl))
      (for-each
       (lambda (val)
         (printf "hello: ~a \n" val))
       l)
      )))

main
