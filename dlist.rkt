#lang racket

(provide
 ->dlist
 dlist-append
 dlist->list
 dlist-length
 dlist-foldl
 dlist-foldr
 dlist-map
 dlist-for-each
 dlist-reverse
 dlist-ref
 dlist-empty
 dlist-car
 dlist-cdr
 )

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

(define (dlist-empty)
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
