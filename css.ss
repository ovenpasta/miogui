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

(define default-stylesheet '(box ))

(define stylesheet '([* ==> 
			(position static)
			(box-sizing border-box)
			(font-family "Sans")
			(font-size 12)]
		     [button ==>
			     (width 100) 
			     (height 50) 
			     (color red)
			     (background-color (rgbf 0 1 0 0.5))
			     (border-style solid)
			     (border-color red)
			     (border-width 1) 
			     (border-radius 7) ]
		     [(and button (: hover)) ==> (border-color green) (background-color blue)]
		     [(and (: hover) (id button2)) ==> (background-color (rgbf 0.5 0.5 0.5)) (color blue)]
		     [(and (: pressed) (id button2)) ==>
		      (background-color (rgb 200 200 200)) (color blue) (transition-duration 0)]
		     [(> * (id button1)) ==> (border-width 4)]
		     ;[(or button (id button1)) ==> (width 300)]
		     [(id button1) ==>
		      (font-weight bold)
		      (padding 7)]
		     [(id button2) ==> (left 200) (top 200) (width 50 !important)
		      (background-color black)
		      (transition-duration 1)]
		     [(id panel-1) ==> 
		      (width 100 %) (height 89 %) (top 0) (left 0) (position absolute) 
		      (background-color (rgb 125 125 125))]
		     [(> (id panel-1) button) ==> (width 27 %) (height 10 %)]
		     [panel ==> 
			    (padding 10) (width 100 %) (border-style solid)
			    (border-width 1) (border-color black)]
		     [label ==>
			     (color black) (padding 5)
			     (border-width 1) (border-color blue)]
		     [slider ==> (height 20) (color black)]
		     [(id lbl2) ==> (width 90 %) (margin 0)]
		     [(id tg1) ==> (border-width 1) (border-color red) (background-color red)
		      (width expand) (height 200) (padding 5)]
		     [(id panel-2) ==> (height 200) (width expand) (border-color red) (border-width 1)]
		     [(id tg1::panel) ==>  (height 100 ) (width expand)]
		     [(id tg1::button) ==> (height 50  ) (width expand)]
		     [(> (id panel-1) slider) ==> (width expand) (margin 5)]
		     [(> (id tg1::panel) panel)
			  ==> (width expand) (height expand) ]
		     [(> (id panel3) label) ==> (width expand)]
		     [slider-box ==> (background-color blue) (border-style none)
				 (border-radius 4)]
		     [(id slider1) ==> (height 25) (width expand) (padding 5)] 
		     [(id slider2) ==> (width 25) (height expand) (padding 2)]
		     ))

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

(define (compare-specifity x y)
  ;(printf "~d ~d~n" x y)
    (cond
     [(< (list-ref x 0) (list-ref y 0)) #t]
     [(> (list-ref x 0) (list-ref y 0)) #f]
     [(< (list-ref x 1) (list-ref y 1)) #t]
     [(> (list-ref x 1) (list-ref y 1)) #f]
     [(< (list-ref x 2) (list-ref y 2)) #t]
     [(> (list-ref x 2) (list-ref y 2)) #f]
     [(< (list-ref x 3) (list-ref y 3)) #t]
     [else #f]))
(define (preprocess-attrs element aa)
  (define parent (mi-element-parent element))
  (let loop ([l aa] [res '()])
    (if (null? l) res
	(let* ([x (car l)]
	       [attr (car x)] [val (cdr x)])
	  ;(printf "attr: ~d val: ~d ~n" attr val)
	  (loop (cdr l)
		(append ;(case attr
			#;[(border) `((border-top ,@val) 
				    (border-left ,@val) 
				    (border-bottom ,@val) 
				    (border-right ,@val))]
		 (match val 
			[(v '% . y) (cond [(eq? attr 'width)
					   (list 
					    (append 
					     (list attr (* (/ v 100.)
							   (mi-element-h parent)) ) y))]
					   [(eq? attr 'height)
					    (list (append
						   (list attr (* (/ v 100.)
								 (mi-element-h parent))) y))])]
			[else (list x)])
		      res))))))

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
  (define entries+style (append (list `(%%style ==>  ,@style)) stylesheet))
  (let loop ([entries entries+style] [props '()])
    (unless (null? entries)
	    (let ([e (car entries)])
	      (let ([selectors (reverse (cdr (memq '==> (reverse e))))]
		    [attribs (preprocess-attrs element (cdr (memq '==> e)))])
		(define null-spec '(0 0 0 0 0))
		(define (process-selector selector type id* class* element* pseudo* specifity)
		 ; (printf "selector: ~d id: ~d pseudo: ~d~n" selector id* pseudo*)
		  (match selector
			 ['%%style (match specifity [(a b c d e) (list 1 b c d e)])]
			 [('id (? (cut eq? <> id*) x)) (match specifity [(a b c d e) (list a (+ 1 b) c d e)])]
			 [('class (? (cut eq? <> class*) x)) (match specifity [(a b c d e) (list a b (+ 1 c) d e)])] 
			 [(': (? (cut eq? <> pseudo*) x)) (match specifity [(a b c d e) (list a b (+ 1 c) d e)])]
			 [('and sel ...) (let ([l (map (cut process-selector <> 'and 
							    id* class* element* pseudo* specifity) sel)])
					   ;(printf "L: ~d\n" l)
					   (if (let loop ([l l])
						 (if (null? l) #t
						     (if (equal? null-spec (car l)) #f
							 (loop (cdr l)))))
					       (fold (lambda (x acc) (map + x acc)) null-spec l)
					       null-spec))]
			 [('or sel ...) (apply map + (map (cut process-selector <> 'or 
							       id* class* element* pseudo* specifity) sel))]
			 [('> e f) (let ([a (process-selector e '>e 
							      (mi-element-id parent) 
							      (mi-element-class parent)
							      (mi-element-el parent)
							      (mi-element-pseudo parent)
							      null-spec)]
					 [b (process-selector f '>f id class el pseudo null-spec)])
				     (if (not (or (equal? a null-spec) (equal? b null-spec)))
					 (map + a b)
					 specifity))]
			 ;[('+ e f) (for-each (cut process-selector <> '+) sel)]
			 ['* '(0 0 0 0 1)]
			 [(? (cut eq? <> element*) e) 
			  (match specifity [(a b c d e) (list a b c (+ d 1) e)])]
			 [else 
			  ;;(printf "~d does not match~n" (car selectors)) 
			  specifity]))
		(let ([sp (process-selector (car selectors) 'type id class el pseudo '(0 0 0 0 0))])
		  ;(printf "Specifity: ~d~n" sp)
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
	   (compare-specifity (car x) (car y))) 
	 matches))
  
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
