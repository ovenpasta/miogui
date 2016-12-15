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

(define-record-type (mi-element make-mi-element% mi-element?)
  (fields el id class parent 
	  (mutable children)
	  (mutable style) 
	  (mutable pseudo)
	  (mutable mi-state) 
	  (mutable x) (mutable y) (mutable w) (mutable h) 
	  (mutable content-size)
	  (mutable layout-state)))

(define (mi-element->string e)
  (format "id: ~d el: ~d\nxywh: ~d ~d ~d ~d\ncontent-size: ~d\npseudo: ~d\nstyle: ~d\n" 
	  (mi-element-id e) (mi-element-el e)
	  (mi-element-x e) (mi-element-y e)
	  (mi-element-w e) (mi-element-h e)
	  (mi-element-content-size e)
	  (mi-element-pseudo e)
	  (fold (lambda (x acc) (string-append acc (format "~d" x) "\n")) ""
			(hashtable->alist (mi-element-style e)))))
  
(define element-table (make-eq-hashtable))

(define old-element-table (make-eq-hashtable))


(define (make-mi-element el id class parent)
  ;(printf "make-mi-element ~d ~d ~d~n" el id class )
  (apply make-mi-element% 
	 (map 
	  (lambda (k) 
	    (case k
	      [el el]
	      [id id]
	      [class class]
	      [pseudo '()]
	      [parent parent]
	      [mi-state #f]
	      [children '()]
	      [style (make-eq-hashtable)]
	      [(x y w h) 0]
	      [else #f]))
	  (vector->list (record-type-field-names (record-type-descriptor mi-element))))))

(define mi-el (make-parameter #f))

(define (value-or-list r)
  (if (and (list? r) (< (length r) 2))
      (car r)
      r))

(define (style-query style attr default)
  (let ([r (hashtable-ref style attr default)])
    (value-or-list r)))

(define (mi-style-query element attr default inherited)
  (let* ([r (hashtable-ref (mi-element-style element) attr #f)]
	 [v (if r (value-or-list r)
		(if inherited 'inherit default))])
    (case v
      [inherit 
       (cond [(mi-element-parent element)
	      => (lambda (parent)
		   (mi-style-query (mi-element-parent element) attr default inherited))]
	     [else default])]
      [initial default]
      [else v])))

(define (mi-element-pseudo-append! element pseudo)
  (mi-element-pseudo-set! element (append (mi-element-pseudo element)
					  pseudo)))

(define (check-activable element old-element activable)
  (define id (mi-element-id element))
  (when old-element
	(when activable
	      (when (not (mi-kbd-item))
		    (mi-kbd-item id)
	      (printf "KBD ITEM: ~d~n" id))
	      (when (eq? (mi-kbd-item) id)
		    (mi-element-pseudo-append! element '(focus))
		    (when (mi-key)
			  (case (mi-key)
			    [tab
			     (if (memq 'shift (mi-keymod))
				 (mi-kbd-item (mi-last-activable))
				 (mi-kbd-item #f))
			     (mi-key #f)]
			    [return
			     (mi-hot-item id) (mi-active-item id)
			     (mi-key #f)
			     ])))
	      (mi-last-activable id))
	
	(when (region-hit? (mi-element-x old-element) (mi-element-y old-element)
		     (mi-element-w old-element) (mi-element-h old-element))
	      (mi-hot-item id)
	      (when (and activable (not (mi-active-item)) (mi-mouse-down?))
		    (mi-active-item id)
		    (mi-kbd-item id))
	      (mi-element-pseudo-append!
		element
			(if (eq? (mi-active-item) id)
			    '(pressed)
			    '(hover))))))

(define (create-element el id activable thunk)
  (define element (mi-el-by-id id))
  (define parent (mi-el))
  (define old-element (hashtable-ref old-element-table id #f))
  (p10e ([mi-el element])
	;; (printf "id ~d old-element: ~d~n " id old-element)
	(when 
	 ;; if we just created the element and we are after the first pass then
	 ;; we'll need to wait until the next frame to create it
	 (or (eq? (mi-state) 'first) element)

	 (if element
	     (unless id
		     (set! id (mi-id id))))
	 (case (mi-state)
	   [ready 
	    (mi-draw-border)
	    (mi-draw-outline)
	    ]
	   [first
	    (set! element (make-mi-element el id (mi-class) parent))
	    (mi-el element)
	    (check-activable element old-element activable)
	    
	    (let ([style (guard (e [else (printf "ops in stylesheet resolve: ") 
					 (display-condition e)
					 (newline) (raise e)])
				(stylesheet-resolve element))]
		  [td 0])
	      (when old-element 
		    (let ([old-style (mi-element-style old-element) ] )
		      (set! td (mi-style-query element 'transition-duration 0 #f))
		      
		      (when (and (> td 0) (not (compare-hashes style old-style)))
			    (let ([tr (hashtable-ref transitions id #f)])
			      (if tr ;; already in transition
				  (start-transition element (style-transition 
							     (list-ref tr 1) (list-ref tr 2)
							     (get-transition-ratio tr td)) style)
				  (start-transition element old-style style)))))
	      
		    (let ([tr (hashtable-ref transitions id #f)])
		      (when tr
			    (set! style (style-transition (list-ref tr 1) (list-ref tr 2) 
							  (get-transition-ratio tr td)))))
		    (eventually-end-transition element td))
		    
	      (mi-element-style-set! element style)) ;;STYLE STUFF
	    
	    (hashtable-set! element-table id element)

	    (cond [(eq? (mi-element-position element) 'absolute)
		   (absolute-position element)])
	    (cond [(mi-element-parent element) 
		   (mi-element-add-child (mi-element-parent element) element)])])

	 (p10e ([mi-style '()])
	       (let* ([r (thunk)])
		 r)))))


(define (mi-x) (mi-element-x (mi-el)))
(define (mi-y) (mi-element-y (mi-el)))
(define (mi-w) (mi-element-w (mi-el)))
(define (mi-h) (mi-element-h (mi-el)))
(define (mi-font-style) (mi-element-font-style (mi-el)))
(define (mi-font-weight) (mi-element-font-weight (mi-el)))
(define (mi-font-size) (mi-element-font-size (mi-el)))
(define (mi-font-family) (mi-element-font-family (mi-el)))
(define (mi-color) (mi-element-color (mi-el)))
(define (mi-bg-color) (mi-element-background-color (mi-el)))
(define (mi-border-radius) (mi-element-border-radius (mi-el)))
(define (mi-border) (mi-element-border (mi-el)))
(define (mi-border-left)
  (list-ref (mi-border) 3))
(define (mi-border-top)
  (list-ref (mi-border) 0))
(define (mi-border-right)
  (list-ref (mi-border) 1))
(define (mi-border-bottom)
  (list-ref (mi-border) 2))

(define (mi-border-left-width)
  (car (mi-border-left)))
(define (mi-border-left-style)
  (cadr (mi-border-left)))
(define (mi-border-left-color)
  (caddr (mi-border-left)))

(define (mi-border-right-width)
  (car (mi-border-right)))
(define (mi-border-right-style)
  (cadr (mi-border-right)))
(define (mi-border-right-color)
  (caddr (mi-border-right)))

(define (mi-border-top-width)
  (car (mi-border-top)))

(define (mi-border-top-style)
  (cadr (mi-border-top)))
(define (mi-border-top-color)
  (caddr (mi-border-top)))

(define (mi-border-bottom-width)
  (car (mi-border-bottom)))
(define (mi-border-bottom-style)
  (cadr (mi-border-bottom)))
(define (mi-border-bottom-color)
  (caddr (mi-border-bottom)))



(define (mi-outline-color) (mi-element-outline-color (mi-el)))
(define (mi-outline-width) (mi-element-outline-width (mi-el)))
(define (mi-outline-style) (mi-element-outline-style (mi-el)))
(define (mi-z-index) (mi-element-z-index (mi-el)))
(define (mi-line-height) (mi-element-line-height (mi-el)))
(define (mi-text-align) (mi-element-text-align (mi-el)))
(define (mi-parent) (mi-element-parent (mi-el)))
(define (mi-padding) (mi-element-padding (mi-el)))
(define mi-class (make-parameter #f))
(define mi-style (make-parameter '()))
(define (mi-display) (mi-element-display (mi-el)))

(define (mi-el-by-id id) 
  (let ([e (hashtable-ref element-table id #f)])
    ;(printf "Mi-el-by-id: ~d ~d~n" id (if e #t #f))
    e))
(define mi-id
  (case-lambda 
    [() (mi-element-id (mi-el))]
    [(sub-id)
     (let ([x (if sub-id 
		  (symbol->string sub-id)
		  (format "~d" (if (mi-el) (length (mi-element-children (mi-el)) ) 0)))]) 
       (string->symbol (string-append (symbol->string (mi-id)) "-" x)))]))


(define (mi-wset id name value)
  (putprop id name value))

(define (mi-wget id name default)
  (getprop id name default))
