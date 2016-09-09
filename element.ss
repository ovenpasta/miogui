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
	  (mutable layout-state) 
	  (mutable x) (mutable y) (mutable w) (mutable h) 
	  (mutable content-size)))
  
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

(define (create-element el id activable thunk)
  (unless id
    (set! id (mi-id id)))
  (let-values ([(last-x last-y last-w last-h) (get-last-coords id)])
    (let ([old-style (widget-old-style id)]
	  [element #f] 
	  [td #f] 
	  [style #f] 
	  [pseudo #f])
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
      (mi-element-style-set! element style)
      
      (set! td (mi-style-query element 'transition-duration 0 #f))

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

      (mi-element-x-set! element (mi-element-left element))
      (mi-element-y-set! element (mi-element-top element))
      (mi-element-w-set! element (mi-element-width element))
      (mi-element-h-set! element (mi-element-height element))
    
      (case  (mi-element-display element)
	[block
	 (let-values ([(x y w h) (layout-element element)])
	   (mi-element-x-set! element x)
	   (mi-element-y-set! element y)
	   (mi-element-w-set! element w)
	   (mi-element-h-set! element h))]
	[flex
	 #t
	 ]
	[else (printf "create-element: error wrong value for display: ~d~n" display)])

      (cond [(and (mi-element-parent element) 
		  (eq? (mi-element-display (mi-element-parent element)) 
		       'flex))
	     (mi-element-x-set! element last-x)
	     (mi-element-y-set! element last-y)
	     (mi-element-w-set! element last-w)
	     (mi-element-h-set! element last-h)
	     (mi-element-add-child (mi-element-parent element) element)])

					;(define border (style-query style 'border 'none))
					;
      ;; (define-values (border-type border-width border-color)
      ;;   (match border 
      ;; 	   ['none (values 'none 0 #f)] 
      ;; 	   [(solid ,width ,color) (values 'solid width color)] ))


      (p10e ([mi-el element])
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
(define (mi-bg-color) (mi-element-background-color (mi-el)))
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
    [(sub-id)
     (let ([x (if sub-id 
		  (symbol->string sub-id)
		  (format "~d" (length (mi-element-children (mi-el)) )))]) 
       (string->symbol (string-append (symbol->string (mi-id)) "-" x)))]))


(define-syntax define-css-element
  (lambda (x)
    (syntax-case x()
      [(_ name default inherited transformer validator ...)
	(with-syntax 
	 ([function-name 
	   (datum->syntax #'name 
			  (string->symbol 
			   (string-append 
			     "mi-element-"
			    (symbol->string 
			     (syntax->datum #'name)))))] )
	 #`(begin 
	     (define (function-name element)
	       (let ([v (mi-style-query element 'name default inherited)])
		 (if (or (validator v) ...)
		     (transformer element v)
		     (errorf 'function-name "invalid attribute value ~d for element ~d" v (mi-element-id element)))))))])))

(define (i-t element x) x)

(define (in-list-validator . values)
  (lambda (v)
    (memq v values)))

(define-css-element position 'static #f i-t
  (in-list-validator 'static 'relative 'absolute))
(define-css-element padding 0 #f i-t number?)
(define-css-element margin 0 #f i-t number?)

(define (color-validator v)
  (or (color? v) (symbol? v) (and (list? v) (<= 4 (length v) 5)))) ;;TODO IMPROVE THIS
(define (color-transformer e v)
  (->color v))
(define-css-element border-color 'black #f color-transformer color-validator)

(define-css-element border-style 'none #f i-t
  ;;not yet supported: 'dotted 'dashed 'double 'groove 'ridge 'inset 'outset
  (in-list-validator 'none 'hidden 'solid)) 

(define (border-width-transformer element width)
  (if (number? width) 
      width
      (case width
	[thin 0.5]
	[medium 1]
	[thick 2])))

(define-css-element border-width 'medium #f
  border-width-transformer
  number? 
  (in-list-validator 'medium 'thin 'thick))

(define-css-element border-radius 0 #f i-t number?)

(define-css-element color 'black #t color-transformer color-validator)

(define-css-element background-color 'transparent #f color-transformer color-validator)

(define-css-element font-family "sans" #t i-t string?)

(define (font-size-transformer element sz)
  (if (number? sz) sz
      (case sz
	[medium 12]
	[large 14]
	[small 10]
	[smaller (- (mi-element-font-size (mi-element-parent element)) 2)]
	[larger  (+ (mi-element-font-size (mi-element-parent element)) 2)]
	[x-small 8]
	[x-large 16]
	[xx-small 7]
	[xx-large 18])))

(define-css-element font-size 'medium #t
  font-size-transformer 
  number? 
  (in-list-validator 'medium 'large 'small 'smaller 'larger 'x-small 'x-large 'xx-small 'xx-large))

;;;normal|bold ; these are not supported: bolder|lighter|number
(define-css-element font-weight 'normal #t i-t
  (in-list-validator 'normal 'bold))

(define-css-element font-style 'normal #t i-t
  (in-list-validator 'normal 'italic 'oblique))

(define (line-height-transformer e v)
  (if (number? v) v
      1.2))

(define (eq-validator s)
  (lambda (x)
    (eq? s x)))

(define-css-element line-height 'normal #t
  line-height-transformer
  number?
  (eq-validator 'normal))

(define-css-element display 'block #f i-t
  (in-list-validator 'block 'flex)) ;; TODO: ADD SUPPORT FOR 'none

(define-css-element justify-content 'flex-start #f i-t
  ;; WARNING: STRETCH IS A MIOGUI EXTENSION
  (in-list-validator 'flex-start 'flex-end 'space-around 'space-between 'center 'stretch))

(define-css-element align-items 'stretch #f i-t
  (in-list-validator 'flex-start 'flex-end 'center 'stretch))

(define-css-element align-self 'auto #f i-t
  (in-list-validator 'auto 'flex-start 'flex-end 'center 'stretch))

(define-css-element flex-direction 'row #f i-t
  (in-list-validator 'row 'column 'row-reverse 'column-reverse))

(define-css-element flex 1 #f i-t number?)

(define-css-element min-width 0 #f i-t number? list?)

(define-css-element min-height 0 #f i-t number? list?)

(define-css-element text-align 0 #f i-t number? list?)

(define-css-element box-sizing 'border-box #f i-t
  (eq-transformer 'border-box))

(define-css-element order 0 #f i-t
  number?)

(define (z-index-transformer element v)
  (if (number? v) v
      (let ([parent (mi-element-parent element)])
	(if parent (mi-element-z-index parent) 0))))

(define-css-element z-index 'auto #f 
  z-index-transformer
  number?
  (eq-validator 'auto))

(define-css-element left 0 #f i-t number?)
(define-css-element top 0 #f i-t number?)
(define-css-element width 'auto #f i-t
  number? list? (in-list-validator 'auto 'expand))
(define-css-element height 'auto #f i-t
  number? list? (in-list-validator 'auto 'expand))
