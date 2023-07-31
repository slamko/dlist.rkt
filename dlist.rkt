#!/usr/bin/env racket

#lang racket

(struct dlist
  (f))

(define (->dlist lst)
  (dlist
   (lambda (tail)
     (append lst tail))))

(define (dlist-append dlist1 . dlist2)
  (dlist
   (lambda (tail)
     ((dlist-f dlist1)
      (foldr (lambda (dl acc)
              ((dlist-f dl) acc)) tail dlist2)))))

(define (dlist->list dlst)
    ((dlist-f dlst) '()))

(define (empty)
  (->dlist '()))

(define (dlist-length dlist)
  (length (dlist->list dlist)))

(define (dlist-reverse dlst)
  (dlist
   (lambda (tail)
    (reverse ((dlist-f dlst) tail)))))

(define (dlist-car dlst)
  (car (dlist->list dlst)))

(define (dlist-cdr dlst)
  (cdr (dlist->list dlst)))

(define (dlist-ref dlst)
  (list-ref (dlist->list dlst)))

(define (dlist-foldl proc init . dlsts)
  (apply (curry foldl proc init)
         (map
          (lambda (dlst)
            (dlist->list dlst)) dlsts)))

(define (dlist-foldr proc init . dlsts)
  (apply (curry foldr proc)
         init
         (map
          (lambda (dlst)
            (dlist->list dlst)) dlsts)))

(define (dlist-map proc . dlsts)
  (apply (curry map proc)
         (map
          (lambda (dlst)
            (dlist->list dlst)) dlsts)))

(define (dlist-for-each proc . dlists)
  (apply (curry for-each proc)
            (map
             (lambda (dlst)
               (dlist->list dlst)) dlists)))

(define main
  (let* (
         (dl1 (->dlist '(1 2 3 4 5 6 7 8 9)))
         (dl2 (->dlist '(4 5 6 7 8 9 0 1 2)))
         (dl3 (->dlist '(9 8 7 6 5 4 3 2 1)))
         (dl4 (->dlist '(9 8 7 6 5 4 3 2 1)))
         (dl5 (->dlist '(0 0 0 0 0 0 0 0 0)))
         (dl (dlist-append dl1 dl2 ))
         (dl2 (dlist-append dl3 dl4))
         )
    
    (begin
      ;; (printf "Length: ~a \n" (dlist-length dl))
      (dlist-foldr
       (lambda (val el acc)
         (printf "El: ~a : ~a \n" val el))
        (void) dl dl2)
      ;; (printf "Dlist?: ~a \n" (dlist-car (dlist-cdr (->dlist dl))))
      )))

main
