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
     (let ([extents (draw-text/padding text x y w h)])
       (mi-element-content-size-set! (mi-el) extents))
     
     (and (not (mi-mouse-down?))
	  (eq? (mi-hot-item) id)
	  (eq? (mi-active-item) id)))))

(define (label id text)
  (import (only (srfi s14 char-sets) char-set)
	  (only (thunder-utils) string-split))
  (create-element 'label id #f
		  (lambda ()
		    (define-values (x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h)))
		    (draw-rect x y w h)
		    (let ([lines (string-split text (char-set #\newline))]
			  [x* (+ (mi-padding) x)] 
			  [y* (+ (mi-padding) y)] 
			  [w* 0]
			  [h* (* (mi-line-height) (mi-font-size))])
		      (if (= 1 (length lines))
			  (let ([extents (draw-text/padding (car lines) x y w h)])
			    (set! w* (car extents)))
			  (let loop ([l lines])
			    (unless (null? l)
			      (let ([extents (draw-text (car l) x* y* 
							(- w (* 2 (mi-padding) ))
							h*)])
				(set! y* (+ y* (* (mi-line-height) (mi-font-size))))
				(set! w* (max w* (car extents)))
				(loop (cdr l))))))
		      (set! h* (* (length lines) (* (mi-line-height) (mi-font-size))))
		      (mi-element-content-size-set! (mi-el) (list w* h*)))
		    #f)))

  
(define (debug-tooltip)
  (define id (mi-hot-item))
  (when id
	(let-values ([(x y w h) (get-last-coords id)])
	  (if (region-hit? x y w h)
	      (p10e ([mi-style `((z-index 1)
					 (position absolute) 
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
       (let ([extents (draw-text/padding (format "~,3F" (state)) x y w h)])
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


(define (textline id text)
  (import (only (srfi s14 char-sets) char-set)
	  (only (thunder-utils) string-split string-replace))
  (create-element 
   'textline id #t
   (lambda ()
     (define-values (x y w h) (values (mi-x) (mi-y) (mi-w) (mi-h)))
     (define (cursor-pos) (mi-wget id 'cursor-pos 0))
     (define (cursor-pos-move dir)
       (let ([cp (cursor-pos)])
	 (cond
	  [(and (< dir 0) (> cp 0))
	   (mi-wset id 'cursor-pos (- cp 1))]
	  [(and (> dir 0) (< cp (string-length (text))))
	   (mi-wset id 'cursor-pos (+ cp 1))])))
     (draw-rect x y w h)

     (if (> (cursor-pos) (string-length (my-text)))
	 (mi-wset id 'cursor-pos (string-length (my-text))))

     (when (eq? (mi-kbd-item) id)
       (let ([txt (text)]
	     [txt-len (string-length (text))])
	 (case (mi-key)
	   [backspace
	    (when (and (> txt-len 0) (> (cursor-pos) 0))
	      (text (string-append
		     (substring txt 0 (- (cursor-pos) 1) )
		     (substring txt (cursor-pos) txt-len)))
	      (cursor-pos-move -1))
	    (mi-key #f)]
	   [delete
	    (when (and (> txt-len 0) (>= (cursor-pos) 0)
		       (< (cursor-pos) txt-len))
	      (text (string-append
		     (substring txt 0 (cursor-pos))
		     (substring txt (+ (cursor-pos) 1) txt-len))))
	    (mi-key #f)]
	   [left  (cursor-pos-move -1)  (mi-key #f)]
	   [right (cursor-pos-move 1)  (mi-key #f)]
	   [home  (mi-wset id 'cursor-pos 0)
		  (mi-key #f)]
	   [end   (mi-wset id 'cursor-pos txt-len)  
		  (mi-key #f)]
	   [else
	    (when (mi-txt)
	      (text (string-append (substring txt 0 (cursor-pos)) (mi-txt)
				   (substring txt (cursor-pos) txt-len)))
	      (mi-wset id 'cursor-pos (+ (string-length (mi-txt))
					 (mi-wget id 'cursor-pos 0)))
	      (mi-txt #f))])))

     (let* ([extents (draw-text/padding (text) x y w h)]
	    [w* (car extents)]
	    [h* (mi-font-size)])
       (mi-element-content-size-set! (mi-el) (list w* h*))
       (when (and (eq? (mi-kbd-item) id)
		  (not (= 0 (logand (bitwise-arithmetic-shift-right (sdl-get-ticks) 9) 1))))
	 (let* ([cursor-pos (mi-wget id 'cursor-pos 0)]
		[size (text-extents (string-replace (substring (text) 0 cursor-pos) #\space #\-))]
		[padding (mi-padding)]
		[text-align (mi-text-align)])
	   (draw! 
	    (lambda ()
	      (define x1
		(case text-align
		  [left   (+ x (car size) padding) ]
		  [center (- (+ x (/ w 2)) (- (/ w* 2) (car size)) )]
		  [right  (- (+ x w) (- w* (car size)) padding)]))
	      (with-cairo (mi-cr)
			  (set-source-color (mi-color))
			  (move-to x1 (+ y padding))
			  (line-to x1 (- (+ y h ) padding))
			  (set-line-width 1)
			  (stroke)))))))
     #f)))
