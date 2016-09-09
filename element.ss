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
  (fields el id class parent (mutable children)
	  (mutable style) (mutable pseudo)
	  (mutable layout-state) (mutable position) (mutable x) (mutable y) (mutable w) (mutable h) 
	  (mutable color) (mutable bg-color)
	  (mutable border-width) (mutable border-color) (mutable border-style) (mutable border-radius)  
	  (mutable transition-duration) 
	  (mutable font-family) (mutable font-size) (mutable font-weight) (mutable font-style)
	  (mutable line-height)
	  (mutable padding) (mutable margin)
	  (mutable content-size)
	  (mutable z-index)
	  (mutable display)
	  (mutable justify-content)
	  (mutable flex-direction)
	  (mutable flex)
	  (mutable direction)
	  (mutable align-items) (mutable align-self)
	  (mutable min-width) (mutable min-height)
	  (mutable text-align)
	  (mutable box-sizing)
	  (mutable order)))
  
(define element-table (make-eq-hashtable))

(define style-table (make-eq-hashtable))
(define content-size-table (make-eq-hashtable))

(define (make-mi-element el id class pseudo parent)
  ;(printf "make-mi-element ~d ~d ~d~n" el id class )
  (apply make-mi-element% 
	 (map 
	  (lambda (k) 
	    (case k
	      [el el]
	      [id id]
	      [class class]
	      [pseudo pseudo]
	      [parent parent]
	      [children '()]
	      [(x y w h margin padding z-index) 0]
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

(define (create-element el id activable thunk)
  (unless id
    (set! id (mi-id id)))
  (let-values ([(last-x last-y last-w last-h) (get-last-coords id)])
    (let ([old-style (widget-old-style id)]
	  [element #f] [td #f] [style #f] [pseudo #f])
      (when (and (number? last-w) (number? last-h) (region-hit? last-x last-y last-w last-h))
	(mi-hot-item id)
	(when (and activable (not (mi-active-item)) (mi-mouse-down?))
	  (mi-active-item id))
	(if (eq? (mi-active-item) id)
	    (set! pseudo 'pressed)
	    (set! pseudo 'hover)))

      (set! element (make-mi-element el id (mi-class) pseudo (mi-el)))
      
      (set! style (stylesheet-resolve element))
      (hashtable-set! style-table id style)
      
      (set! td (style-query style 'transition-duration 0))

      (when (not (compare-hashes style old-style))
	(let ([tr (hashtable-ref transitions id #f)])
	  (if tr ;; already in transition
	      (start-transition element (style-transition (list-ref tr 1) (list-ref tr 2)
							  (get-transition-ratio tr td)) style)
	      (start-transition element old-style style))))

      (let ([tr (hashtable-ref transitions id #f)])
	(when tr
	  (set! style (style-transition (list-ref tr 1) (list-ref tr 2) 
					(get-transition-ratio tr td)))))
      (eventually-end-transition element td)
      (mi-element-content-size-set! element (hashtable-ref content-size-table id #f))
      (mi-element-style-set! element style)
      (mi-element-position-set! element (style-query style 'position 'static))
      
      (mi-element-padding-set! element (style-query style 'padding 0))
      (mi-element-margin-set! element (style-query style 'margin 0))

      (mi-element-border-color-set! element (->color (style-query style 'border-color 'black)))
      (mi-element-border-style-set! element (style-query style 'border-style 'none))
      (mi-element-border-width-set! element (style-query style 'border-width 1))
      (mi-element-border-radius-set! element (style-query style 'border-radius 0))

      (mi-element-color-set! element (->color (style-query style 'color 'white)))
      (mi-element-bg-color-set! element (->color (style-query style 'background-color 'white)))

      (mi-element-font-family-set! element (style-query style 'font-family "Sans"))
      (mi-element-font-size-set! element (style-query style 'font-size 12))
      (mi-element-font-weight-set! element (style-query style 'font-weight 'normal))
      (mi-element-font-style-set! element (style-query style 'font-style 'normal))
      (mi-element-line-height-set! element (style-query style 'line-height 1.2))

      (mi-element-x-set! element (style-query style 'left 0))
      (mi-element-y-set! element (style-query style 'top 0))
      (mi-element-w-set! element (style-query style 'width 'none))
      (mi-element-h-set! element (style-query style 'height 'none))

      (mi-element-display-set! element (style-query style 'display 'block))

      (mi-element-justify-content-set! element (style-query style 'justify-content 'flex-start))
      (mi-element-align-items-set! element (style-query style 'align-items 'stretch))
      (mi-element-align-self-set! element (style-query style 'align-self 'auto))
      (mi-element-flex-direction-set! element (style-query style 'flex-direction 'row))
      (mi-element-flex-set! element (style-query style 'flex '1))
      (mi-element-min-width-set! element (style-query style 'min-width 0))
      (mi-element-min-height-set! element (style-query style 'min-height 0))
      (mi-element-text-align-set! element (style-query style 'text-align 'left))
      (mi-element-direction-set! element (style-query style 'direction 'ltr))
      (mi-element-box-sizing-set! element (style-query style 'box-sizing 'border-box))
      (mi-element-order-set! element (style-query style 'order 0))
      
      (let ([display (style-query style 'display 'block)])
	(mi-element-display-set! element display)    
	(case display
	  [block
	   (let-values ([(x y w h) (layout-element element)])
	     (mi-element-x-set! element x)
	     (mi-element-y-set! element y)
	     (mi-element-w-set! element w)
	     (mi-element-h-set! element h))]
	  [flex
					;(mi-element-x-set! element last-x)
					;(mi-element-y-set! element last-y)
					;(mi-element-w-set! element last-w)
					;(mi-element-h-set! element last-h)
					;(mi-element-add-child (mi-element-parent element) element)
	   #t
	   ]
	  [else (printf "create-element: error wrong value for display: ~d~n" display)]))

      (cond [(eq? (mi-element-display (mi-element-parent element)) 'flex)
	     (mi-element-x-set! element last-x)
	     (mi-element-y-set! element last-y)
	     (mi-element-w-set! element last-w)
	     (mi-element-h-set! element last-h)
	     (mi-element-add-child (mi-element-parent element) element)])

      (let ([z-index (style-query style 'z-index 'auto)])
	(if (eq? z-index 'auto)
	    (mi-element-z-index-set! element (mi-element-z-index (mi-element-parent element)))
	    z-index))

					;(define border (style-query style 'border 'none))
					;
      ;; (define-values (border-type border-width border-color)
      ;;   (match border 
      ;; 	   ['none (values 'none 0 #f)] 
      ;; 	   [(solid ,width ,color) (values 'solid width color)] ))


      (parameterize ([mi-el element])
	(let* ([r (thunk)]
	       [sz (mi-element-content-size element)])
	  (hashtable-set! layout-state id #f)
	  (hashtable-set! content-size-table id sz)
	  (hashtable-set! element-table id element)
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
(define (mi-bg-color) (mi-element-bg-color (mi-el)))
(define (mi-border-radius) (mi-element-border-radius (mi-el)))
(define (mi-border-color) (mi-element-border-color (mi-el)))
(define (mi-border-width) (mi-element-border-width (mi-el)))
(define (mi-border-style) (mi-element-border-style (mi-el)))
(define (mi-z-index) (mi-element-z-index (mi-el)))
(define (mi-line-height) (mi-element-line-height (mi-el)))

(define (mi-parent) (mi-element-parent (mi-el)))
(define (mi-padding) (mi-element-padding (mi-el)))
(define mi-class (make-parameter #f))
(define mi-style (make-parameter '()))
(define (mi-display) (mi-element-display (mi-el)))

(define (mi-el-by-id id) (hashtable-ref element-table id #f))
(define mi-id
  (case-lambda 
    [() (mi-element-id (mi-el))]
    [(id)
     (let ([x (if id 
		  (symbol->string id)
		  (format "~d" (length (mi-element-children (mi-el)) )))]) 
       (string->symbol (string-append (symbol->string (mi-id)) "-" x)))]))
