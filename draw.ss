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

(define (round-rect x y width height corner-radius)
  (let* ([cr (mi-cr)]
	 [pi 3.1415926536]
	 [aspect 1.0] ;;     /* aspect ratio */
	 [radius (/ corner-radius aspect)]
	 [degrees (/ pi 180.0)])

    (cairo-new-sub-path cr)
    (cairo-arc cr (- (+ x  width) radius) (+ y radius) radius (* -90 degrees) (* 0 degrees))
    (cairo-arc cr (- (+ x width) radius) (- (+ y height) radius) radius (* 0 degrees) (* 90 degrees))
    (cairo-arc cr (+ x radius) (- (+ y height) radius) radius (* 90 degrees) (* 180 degrees))
    (cairo-arc cr (+ x radius) (+ y radius) radius (* 180 degrees) (* 270 degrees))
    (cairo-close-path cr)))

(define (draw-rect x y w h)
  (define bw (mi-border-width))
  (define border-color (mi-border-color))

  (when (mi-bg-color)
	(with-cairo (mi-cr)
	      ;(cairo-scale 640 480)
	      (set-source-color (mi-bg-color))
	      (if (> (mi-border-radius) 0)
		  (round-rect (+ x bw) (+ y bw) (- w bw) (- h bw) (mi-border-radius))
		  (cairo-rectangle (mi-cr) (+ x bw) (+ y bw) (- w bw) (- h bw)))
	      (fill-preserve)))
  (with-cairo (mi-cr)
	      ;(cairo-scale 640 480)
	      (set-line-width bw)
	      (set-source-color border-color)
	      (stroke)))

(define (draw-text/centered text x y)
  (check-arg string? text draw-text/centered)
  (check-arg number? x draw-text/centered)
  (check-arg number? y draw-text/centered)

  (cairo-set-font-size (mi-cr) (mi-font-size))
  (cairo-select-font-face  (mi-cr) (string-append (mi-font-family) (string #\nul))
			   (cairo-font-slant (mi-font-style)) ;; normal|italic|oblique
			   (cairo-font-weight (mi-font-weight))) ;; normal|bold
 
  (let ([extents (cairo-text-extents-create)])
    (cairo-text-extents (mi-cr) text extents)
    (let-struct extents cairo-text-extents-t (width height x-bearing y-bearing)
;		(printf "x ~d y ~d~n" x y)
		(cairo-set-source-color (mi-cr) (mi-color))
		(cairo-move-to (mi-cr) 
			       (- x (/ width 2) x-bearing)
			       (- y (/ height 2) y-bearing))
		(cairo-show-text (mi-cr) text)
		(list width height))))

(define (draw-box id class style)
  #t
  )
