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

(define draw-pool '())
(define (round-rect x y width height br)
  (define-values (tlrx tlry trrx trry brrx brry blrx blry)
    (match br
	   [((tlrx tlry) (trrx trry) (brrx brry) (blrx blry))
	    (values tlrx tlry trrx trry brrx brry blrx blry)]
	   [else
	    (values 0 0 0 0 0 0 0 0)]))
  
  ;(define (h x) (car x)) (define (y x) (cadr x))
  ;(printf "br: ~d~n" br)
  (let* ([cr (mi-cr)]
	 [pi 3.1415926536] 
	 [aspect 1.0] ;;     /* aspect ratio */
	 [degrees (/ pi 180.0)])

    (cairo-new-sub-path cr)
    (cairo-save cr)
    (cairo-translate cr 
     		     (- (+ x  width) trrx)
     		     (+ y trry))    
    (if (and (< 0 trrx) (< 0 trry))
	(cairo-scale cr trrx trry))
    (cairo-arc cr 0 0 1 (* -90 degrees) (* 0 degrees))
    (cairo-restore cr)

    ;; (cairo-arc cr (- (+ x  width) (car top-left-r)) (+ y (cadr top-left-r)) 
    ;; 	       radius (* -90 degrees) (* 0 degrees))
    ;; (cairo-arc cr (- (+ x width) radius) (- (+ y height) radius) radius (* 0 degrees) (* 90 degrees))
    ;; (cairo-arc cr (+ x radius) (- (+ y height) radius) radius (* 90 degrees) (* 180 degrees))
    ;; (cairo-arc cr (+ x radius) (+ y radius) radius (* 180 degrees) (* 270 degrees))
    (cairo-close-path cr)))


(define (round-rect* x y width height border corner-radius )
  (define (setup-border i)
    (let ([x (list-ref border i)])
      (cairo-set-line-width (mi-cr) (car x))
      (cairo-set-source-color (mi-cr) (caddr x))))
  ;(set! corner-radius (min corner-radius (/ width 2) (/ height 2)))
  (let* ([cr (mi-cr)]
	 [pi 3.1415926536] 
	 [aspect 1.0] ;;     /* aspect ratio */
;	 [radius (/ corner-radius aspect)]

	 [radius 0]
	 [degrees (/ pi 180.0)])

    (cairo-move-to cr (+ x radius) y)
    (cairo-line-to cr (- (+ x width) radius) y )
    (setup-border 0)
    (cairo-stroke cr)

    (cairo-new-sub-path cr)
    (cairo-arc cr (- (+ x  width) radius) (+ y radius) radius (* -90 degrees) (* 0 degrees))
    (cairo-arc cr (- (+ x width) radius) (- (+ y height) radius) radius (* 0 degrees) (* 90 degrees))
    (setup-border 1)
    (cairo-stroke cr)

    (cairo-move-to cr (+ x radius) (+ y height))
    (cairo-line-to cr (- (+ x width) radius) (+ y height))
    (setup-border 2)
    (cairo-stroke cr)

    (cairo-new-sub-path cr)  
    (cairo-arc cr (+ x radius) (- (+ y height) radius) radius (* 90 degrees) (* 180 degrees))
    (cairo-arc cr (+ x radius) (+ y radius) radius (* 180 degrees) (* 270 degrees))
    (setup-border 3)
    (cairo-stroke cr)
    #;(cairo-close-path cr)))


(define (draw! thunk)
  (define z-index (mi-z-index))
  (when (layout-ready?)
	(let ([x (assq z-index draw-pool)])
	  (if x
	      (set-cdr! x (cons thunk (cdr x)))
	      (set! draw-pool (cons (cons z-index (list thunk)) 
			      draw-pool))))))
  
(define (draw-all)
  (let ([z-ordered-draw (sort (lambda (x y) (< (car x) (car y))) draw-pool)])
    (for-each (lambda (x) 
		(for-each (lambda (y) (y))
			    (reverse (cdr x))))
		z-ordered-draw))
 (set! draw-pool '()))

(define (mi-draw-border)
  (draw-rect* (mi-x) (mi-y) (mi-w) (mi-h) (mi-bg-color) (mi-border)
	      (mi-border-radius)))

(define (mi-draw-outline)
  (define width (mi-outline-width))
  (draw-rect (- (mi-x) width) (- (mi-y) width)
	     (+ (mi-w) width) (+ (mi-h) width)
	     (mi-outline-style) (mi-outline-color) 
	     width
	     (->color 'transparent)
	     (mi-border-radius)))

(define (draw-rect x y w h border-style border-color bw bg-color border-radius)
  (define (draw-path)
    (if (equal? border-radius '((0 0) (0 0) (0 0) (0 0)))
	(cairo-rectangle (mi-cr) (+ x bw) (+ y bw) (- w bw) (- h bw)))
	(round-rect (+ x bw) (+ y bw) (- w bw) (- h bw) border-radius))
  
  (draw! 
   (lambda ()
     (when bg-color
       (draw-path)
       (with-cairo (mi-cr)
		   (set-source-color bg-color)
		   (fill)))
     (when (not (memq border-style '(none hidden)))
       (draw-path)
       (with-cairo (mi-cr)
		   (set-line-width bw)
		   (set-source-color border-color)
		   (stroke))))))

(define (draw-rect* x y w h bg-color border  border-radius)

  (define (draw-path)
    (round-rect* x y w h border border-radius)) 
  (define (draw-bg)
    (round-rect x y w h border-radius))
  (draw! 
   (lambda ()
     (when bg-color
	   (draw-bg)
	   (with-cairo (mi-cr)
		       (set-source-color bg-color)
		       (fill)))
     
	   (draw-path))))

(define (text-extents text)
  (define font-size (mi-font-size))
  (define font-family (mi-font-family))
  (define font-style (mi-font-style))
  (define font-weight (mi-font-weight))
  (define text-align (mi-text-align))
  (define color (mi-color))

  (check-arg string? text draw-text)

  (let ([extents (cairo-text-extents-create)])
    (cairo-set-font-size (mi-cr) font-size)
    (cairo-select-font-face  (mi-cr) (string-append font-family (string #\nul))
					    (cairo-font-slant font-style) ;; normal|italic|oblique
					    (cairo-font-weight font-weight)) ;; normal|bold
    (cairo-text-extents (mi-cr) text extents)
    (let-struct extents cairo-text-extents-t 
		(width height x-bearing y-bearing x-advance y-advance)
		(list width height x-bearing y-bearing x-advance y-advance))))

(define-syntax cast
  (syntax-rules ()
    [(_ ftype fptr)
     (make-ftype-pointer ftype
			 (ftype-pointer-address fptr))]))

(define (*int ptr) (ftype-ref int () ptr))

(define (show-text cr text x y font-size)
  (define glyphs* (cairo-glyph*-create))
  (define glyph-count (cairo-int-create))
  (define clusters* (cairo-text-cluster*-create))
  (define cluster-count (cairo-int-create))
  (define clusterflags (cairo-text-cluster-flags-create))
  (define scaled-face (cairo-get-scaled-font cr))
  (define clusters #f)
  (define glyphs #f)
  
  (ftype-set! void* () (cast void* clusters*) 0)
  (ftype-set! void* () (cast void* glyphs*) 0)

  ;; THIS COULD BE CACHED SOMEWHERE?
  (unless (eq? 'success
	       (cairo-scaled-font-text-to-glyphs
		scaled-face x y text (string-length text)
		glyphs* glyph-count
		clusters* cluster-count
		clusterflags))
	  (raise (error 'show-text "stat error" stat)))

  (set! clusters (cairo-guard-pointer (ftype-&ref cairo-text-cluster-t* (*) clusters*)))
  (set! glyphs (cairo-guard-pointer (ftype-&ref cairo-glyph-t* (*) glyphs*)))

  ;; WE COULD USE cairo-show-glyphs instead?
  (let loop ([glyph-index 0] [byte-index 0] [i 0])
    (when (< i (*int cluster-count))
	  (let* ([cluster       (ftype-&ref cairo-text-cluster-t () clusters i)]
		 [clusterglyphs (ftype-&ref cairo-glyph-t () glyphs glyph-index)]
		 [extents (cairo-text-extents-create)])
	    (let-struct
	     cluster cairo-text-cluster-t (num-glyphs num-bytes)
	     (cairo-scaled-font-glyph-extents scaled-face clusterglyphs (*int glyph-count) extents)
	     ;;(printf "extents: status: ~d num-glyphs: ~d num-bytes: ~d~n" (cairo-status cr) num-glyphs num-bytes)
	     (with-cairo cr
			 (glyph-path clusterglyphs num-glyphs)
			 (set-line-width 0)
			 (fill-preserve)
			 (stroke))
	     (loop (+ glyph-index num-glyphs)
		   (+ byte-index num-bytes)
		   (+ i 1)))))))


(define (draw-text text x y w h)
  (define font-size (mi-font-size))
  (define font-family (mi-font-family))
  (define font-style (mi-font-style))
  (define font-weight (mi-font-weight))
  (define text-align (mi-text-align))
  (define color (mi-color))

  (check-arg string? text draw-text)
  (check-arg number? x draw-text)
  (check-arg number? y draw-text)
  
  (let ([extents (cairo-text-extents-create)])
       (cairo-set-font-size (mi-cr) font-size)
       (cairo-select-font-face  (mi-cr) (string-append font-family (string #\nul))
				(cairo-font-slant font-style) ;; normal|italic|oblique
				(cairo-font-weight font-weight)) ;; normal|bold
       (cairo-text-extents (mi-cr) text extents)
       (let-struct extents cairo-text-extents-t 
		   (width height x-bearing y-bearing x-advance y-advance)
		   (draw!
		    (lambda ()
		      ;(printf "font-family: ~d~n" font-family)
		      (cairo-set-font-size (mi-cr) font-size)
		      (cairo-select-font-face  (mi-cr) (string-append font-family (string #\nul))
					       (cairo-font-slant font-style) ;; normal|italic|oblique
					       (cairo-font-weight font-weight)) ;; normal|bold
		      (cairo-set-source-color (mi-cr) color)
		      (cairo-identity-matrix (mi-cr))
		      (case text-align
			[left
			 (cairo-translate (mi-cr) x (+ y height))]
			[center
			 (cairo-translate (mi-cr) (- (+ x (/ w 2)) (/ width 2)) (+ y height))]
			[right
			 (cairo-translate (mi-cr) (- (+ x w) width x-bearing) (+ y height))]
			[else
			 (cairo-translate (mi-cr) x (+ y height))])
		      
		      (show-text (mi-cr) text 0 0 font-size)
		      
		      (cairo-identity-matrix (mi-cr))))
		   
		   (list x-advance height))))

(define (draw-text/padding text x y w h)
  (draw-text text 
	     (+ (mi-padding) x)
	     (+ (mi-padding) y)
	     (- w (* 2 (mi-padding)))
	     (- h (* 2 (mi-padding)))))

(define (draw-box id class style)
  #t
  )
