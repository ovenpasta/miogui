;; * MIOGUI *
;;
;; Copyright 2016 Aldo Nicolas Bruno
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define stylesheet (make-parameter '()))

(define (hashtable->alist ht)
   (let-values ([(keys values) (hashtable-entries ht)])
     (vector->list (vector-map list keys values))))

(define (alist->hashtable alist)
  (define hash (make-eq-hashtable))
  (for-each
   (lambda (x)  
     (let ([v (if (and (list? x) (< (length x) 2)) 
				      (cadr x) 
				      (cadr x))])
       (hashtable-set! hash (car x) v)
       v))
   alist)
  hash)

(define (compare-specificity x y)
  ;(printf "~d ~d~n" x y)
    (cond
     [(equal? x y) #f]
     [(< (list-ref x 0) (list-ref y 0)) #t]
     [(> (list-ref x 0) (list-ref y 0)) #f]
     [(< (list-ref x 1) (list-ref y 1)) #t]
     [(> (list-ref x 1) (list-ref y 1)) #f]
     [(< (list-ref x 2) (list-ref y 2)) #t]
     [(> (list-ref x 2) (list-ref y 2)) #f]
     [(< (list-ref x 3) (list-ref y 3)) #t]
     [else #f]))

(include "css-preprocess.ss")

;(define (preprocess-attrs aa) aa)

(define (stylesheet-resolve element)
  (import (only (srfi s1 lists) last drop-right fold))
  (define id (mi-element-id element))
  (define class (mi-element-class element))
  (define parent (mi-element-parent element))
  (define pseudo (mi-element-pseudo element))
  (define el (mi-element-el element))
  (define style (mi-style))
  
  (define matches '())
  (define hash (make-eq-hashtable))
  (define entries+style (append (list `(%%style ==>  ,@style)) (stylesheet)))
  (let loop ([entries entries+style] [props '()])
    (unless (null? entries)
	    (let ([e (car entries)])
	      (let ([selectors (reverse (cdr (memq '==> (reverse e))))]
		    [attribs (preprocess-attrs element (cdr (memq '==> e)))])
		(define null-spec '(0 0 0 0 0))
		(define (process-selector selector type id* class* element* pseudo* specificity)
		 ;(printf "selector: ~d id: ~d pseudo: ~d~n" selector id* pseudo*)
		  (match selector
		    ['%%style (match specificity [(a b c d e) (list 1 b c d e)])]
		    [('id (? (cut eq? <> id*) x)) 
		     (match specificity [(a b c d e) (list a (+ 1 b) c d e)])]
		    [('class (? (cut eq? <> class*) x)) 
		     (match specificity [(a b c d e) (list a b (+ 1 c) d e)])] 
		    [(': (? (cut memq <> pseudo*) x)) (match specificity [(a b c d e) (list a b (+ 1 c) d e)])]
		    [('and sel ...) 
		     (let ([l (map (cut process-selector <> 'and 
					id* class* element* pseudo* specificity) sel)])
					;(printf "L: ~d\n" l)
		       (if (let loop ([l l])
			     (if (null? l) #t
				 (if (equal? null-spec (car l)) #f
				     (loop (cdr l)))))
			   (fold (lambda (x acc) (map + x acc)) null-spec l)
			   null-spec))]
		    [('or sel ...) 
		     (apply map + (map (cut process-selector <> 'or 
					    id* class* element* pseudo* specificity) sel))]
		    [('> e f) 
		     (let ([a (process-selector e '>e 
						(mi-element-id parent) 
						(mi-element-class parent)
						(mi-element-el parent)
						(mi-element-pseudo parent)
						null-spec)]
			   [b (process-selector f '>f id class el pseudo null-spec)])
		       (if (not (or (equal? a null-spec) (equal? b null-spec)))
			   (map + a b)
			   specificity))]
					;[('+ e f) (for-each (cut process-selector <> '+) sel)]
		    ['* '(0 0 0 0 1)]
		    [(? (cut eq? <> element*) e) 
		     (match specificity [(a b c d e) (list a b c (+ d 1) e)])]
		    [else 
		     ;;(printf "~d does not match~n" (car selectors)) 
		     specificity]))
		(let ([sp (process-selector (car selectors) 'type id class el pseudo '(0 0 0 0 0))])
					;(printf "Specificity: ~d~n" sp)
		  (unless (equal? sp null-spec)
		    (set! matches (cons (cons sp attribs) matches))))))
	    (loop (cdr entries) '())))
  ;(pretty-print matches)
  (for-each
   (lambda (x)
     (for-each (lambda (pair)
		 ;(printf "pair: ~d~n" pair)
		 (let ([val (cdr pair)]
		       [e (hashtable-ref hash (car pair) #f)])
		   (if (= 1 (length val))
		       (set! val (car val)))
		   ;(printf "k: ~d e: ~d val: ~d~n" (car pair) e val)
		   (if (not (and e (pair? e) (eq? (last e) '!important))) 
		       (hashtable-set! hash (car pair) val))))
	       (cdr x)))
   (sort (lambda (x y) 
	   (compare-specificity (car x) (car y))) 
	 (reverse matches)))
  
  (alist->hashtable 
   (map 
    (lambda (x)
      ;;(printf "x: ~d cdr ~d ~d~n" x (cdr x) (and (list? (cadr x)) 1 (last (cdr x))))
      (if (and (list? (cadr x)) (eq? (last (cadr x)) '!important))
	 (cons (car x) (drop-right (cadr x) 1))
	 (if (and (list? (cadr x)) (< (length (cadr x)) 2))
	     (cons (car x) (cadr x))
	     (cons (car x) (cdr x)))))
   (hashtable->alist hash))))
