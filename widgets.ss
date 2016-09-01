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

(define (panel id children-thunk)
  ;(define-values (x y w h) (get-last-coords id))
  (create-element 
   'panel id #f 
   (lambda ()
     ;(printf "panel element: ~d ~d ~d ~d ~d~n" (mi-el) (mi-x) (mi-y) (mi-w) (mi-h))
     (draw-rect (mi-x) (mi-y) (mi-w) (mi-h))
     (start-layout (mi-el))
     (children-thunk)
     (end-layout (mi-el)))))

(define (symbol-append sym s)
  (string->symbol (string-append (symbol->string sym) s)))

(define (toggle-panel id state children-thunk)
  ;(define-values (x y w h) (get-last-coords id))
  (create-element 
   'toggle-panel id #f
   (lambda ()
     ;(printf "toggle-panel element: ~d ~d ~d ~d ~d~n" (mi-el) (mi-x) (mi-y) (mi-w) (mi-h)) 
     (draw-rect (mi-x) (mi-y) (mi-w) (mi-h))
     (start-layout (mi-el))
     (if (button (symbol-append id "::button") "HI")
	 (state (not (state))))
     (when (state)
	   (mi-force-break id)
	   (panel (symbol-append id "::panel") 
		  children-thunk)
	   (end-layout (mi-el)))))
  (state))

(define (button id text)
  (create-element 'button id #t
   (lambda ()
     (define-values (x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h)))
     
     (draw-rect x y w h)
     (draw-text/centered text (+ 0 x (/ w 2)) (+ 0 y (/ h 2)))
     
     ;;return
     (and (not (mi-mouse-down?))
	  (eq? (mi-hot-item) id)
	  (eq? (mi-active-item) id)))))

(define (label id text)
  (create-element 'label id #f
		  (lambda ()
		    (define-values (x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h)))
		    (draw-rect x y w h)
		    (let ([extents (draw-text/centered text (+ 0 x (/ w 2)) (+ 0 y (/ h 2)))])
		      (mi-element-content-size-set! (mi-el) extents))
		    #f)))

(define (box id text)
  (create-element 'box id #f
		  (lambda ()
		    (define-values (x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h)))
		    (draw-rect x y w h))))

(define (debug-tooltip)
  (define id (mi-hot-item))
  (when id
	(let-values ([(x y w h) (get-last-coords id)])
	  (if (region-hit? x y w h)
	      (parameterize ([mi-style `((position absolute) 
					 (left ,(mi-mouse-x)) 
					 (top ,(mi-mouse-y)) )])
			    (label (symbol-append id "::debug") (symbol->string id)))))))


(define (hslider id state)
  (create-element 'hslider id #t
   (lambda ()
     (let-values ([(x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h))])
       (draw-rect x y w h)
       (start-layout (mi-el))
       (let ([l (* (- w (* 2 (mi-padding)) 20) (state))])
	 (p10e ([mi-style `((width 20) (height expand) (position relative) (left ,l))])
	       (create-element 
		'slider-box (symbol-append id "::box") #t
		(lambda ()
		  (let-values ([(x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h))])
		    (draw-rect x y w h))))))
       (end-layout (mi-el))
       (let ([extents (draw-text/centered (format "~,3F" (state)) (+ 0 x (/ w 2)) (+ 0 y (/ h 2)))])
	 (mi-element-content-size-set! (mi-el) extents))
       
       (when (and (> w 0) (eq? (mi-active-item) id))
	     (let ([val (/ (- (mi-mouse-x) x) w)])
	       (if (< val 0) (set! val 0))
	       (if (> val 1) (set! val 1))
	     (cond [(not (= (state) val))
		    (state val)
		    #t]
		   [else #f])))))))
  

(define (vslider id state)
  (create-element 'vslider id #t
   (lambda ()
     (let-values ([(x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h))])
       (draw-rect x y w h)
       (start-layout (mi-el))
       (let ([l (* (- h (* 2 (mi-padding)) 20) (state))])
	 (p10e ([mi-style `((width expand) (height 20) (position relative) (top ,l))])
	       (create-element 
		'slider-box (symbol-append id "::box") #t
		(lambda ()
		  (let-values ([(x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h))])
		    (draw-rect x y w h))))))
       (end-layout (mi-el))
       ;(let ([extents (draw-text/centered (format "~,3F" (state)) (+ 0 x (/ w 2)) (+ 0 y (/ h 2)))])
       ;(mi-element-content-size-set! (mi-el) extents))
       
       (when (and (> h 0) (eq? (mi-active-item) id))
	     (let ([val (/ (- (mi-mouse-y) y) h)])
	       (if (< val 0) (set! val 0))
	       (if (> val 1) (set! val 1))
	     (cond [(not (= (state) val))
		    (state val)
		    #t]
		   [else #f])))))))
  
