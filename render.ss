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

(define (render-prepare)
  (define tex (mi-sdl-texture))
  (define r0 (make-ftype-pointer sdl-rect-t 0))

  (sdl-set-render-draw-color (mi-renderer) 0 0 0 1)
  (sdl-render-clear (mi-renderer))
  (mi-cairo-surface 
   (sdl-let-ref-call sdl-lock-texture 
		     (tex r0 (pixels void*) (pitch int)) 
		     return
		     (cairo-image-surface-create-for-data 
		      (make-ftype-pointer unsigned-8 pixels)
		      (cairo-format 'argb-32) (mi-window-width) (mi-window-height) pitch)))
  
  (mi-cr (cairo-create (mi-cairo-surface)))
  (with-cairo (mi-cr)
	      (set-source-rgb 0 0 0) ; blank scrren
	      (rectangle 0 0 (mi-window-width) (mi-window-height))
	      (fill))
  (mi-hot-item #f))

(define (render-finish)
  (draw-all)

  (if (not (mi-mouse-down?))
      (mi-active-item #f)
      (if (not (mi-active-item))
	  (mi-active-item '())))
  (sdl-unlock-texture (mi-sdl-texture))
  (sdl-render-copy (mi-renderer) (mi-sdl-texture)
		   (make-ftype-pointer sdl-rect-t 0) 
		   (make-ftype-pointer sdl-rect-t 0))
  
  (sdl-render-present (mi-renderer))
  (collect)
  (sdl-free-garbage))

(define last-frame (current-time))
(define stat-fps 0)
(define toggle (make-parameter #f))
(define tg1-state (make-parameter #f))

(define slider-state 
  (make-parameter 0.25 
		  (lambda (x) 
		    (fps (+ 1 (* 100 x)))
		    x)))

(define (render-stuff&)
  (render-prepare)
  (panel 'panel-1
	 (lambda () 
	   (if (button 'button1 "CIAO")
	       (printf "BUTTON CLICKED!\n"))
	   (if (button 'button2 "NAMAST66E")
	       (printf "BUTTON CLICKED NAMASTE!\n"))
	   (mi-force-break 'panel-1)
	   (when (button 'button3 (format "FPS: ~,2F" stat-fps))
		 (printf "BUTTON3 CLICKED!\n")
		 (toggle (not (toggle))))
	   (p10e ([mi-style '((width 200) (height 20))])
		 (label 'lbl-active (format "~d" (mi-active-item)))
		 (label 'lbl-hot (format "~d" (mi-hot-item)))
		 (label 'lbl-md (format "~d" (mi-mouse-down?))))
	   (mi-force-break 'panel-1)
		 
	   (if (toggle) 
	       (panel 'panel-2 
		      (lambda ()
			(label 'lbl1 "lalalala"))))
	   
	   (toggle-panel 'tg1 tg1-state
			 (lambda ()
			   (vslider 'slider2 slider-state)
			   (panel 'panel3 (lambda ()
					    (label 'lbl3 "123455")
					    (mi-force-break 'panel3)
					    (label 'lbl4 "67890")))))
	   
	   (hslider 'slider1 slider-state)))

  (debug-tooltip)
  
  (render-finish))

(define (render-stuff )
  (render-stuff&)
  (let ([d (time-difference (current-time) last-frame)])
    ;(printf "frame-duration: ~d~n" (time-float d))
    ;(printf "fps: ~d~n" (/ 1. (time-float d)))
    (set! stat-fps (/ 1. (time-float d)))
    (set! last-frame (current-time))))

