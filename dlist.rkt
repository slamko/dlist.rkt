#|
Copyright (c) 2023, Viacheslav <vaceslavkozin619@gmail.com>
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

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
