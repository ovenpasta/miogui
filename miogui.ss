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

#!chezscheme
(import (chezscheme)
	(sdl2)
	(cairo))

(debug-level 3)
(optimize-level 0)
(sdl-library-init)
(cairo-library-init)

(define mi-window (make-parameter #f))
(define mi-renderer (make-parameter #f))
(define mi-window-width (make-parameter 640))
(define mi-window-height (make-parameter 480))
(define mi-sdl-texture (make-parameter #f))

(define (init-sdl window-title)
  (assert (= 0 (sdl-init (sdl-initialization 'video))))
  
  (mi-window (sdl-create-window window-title 100 100 
				(mi-window-width) (mi-window-height) 
				(sdl-window-flags 'shown 'resizable)))
  (assert (not (ftype-pointer-null? (mi-window))))
  
  (mi-renderer (sdl-create-renderer (mi-window) -1 
				    (sdl-renderer-flags 'accelerated 'presentvsync)))
  (assert (not (ftype-pointer-null? (mi-renderer))))

  (mi-sdl-texture (sdl-create-texture (mi-renderer) (sdl-pixelformat 'argb-8888) 
				      (sdl-texture-access 'streaming) 
				      (mi-window-width) (mi-window-height))))

(define (fini-sdl)
  (sdl-destroy-window (mi-window)))

(define mi-mouse-x (make-parameter 0))
(define mi-mouse-y (make-parameter 0))
(define mi-mouse-down? (make-parameter #f))
(define mi-hot-item (make-parameter #f))
(define mi-active-item (make-parameter #f))
(define mi-active-window 'none)
(define mi-last-activable (make-parameter #f))

(define mi-kbd-item (make-parameter #f))

;; (define mi-keys (make-parameter '()))

;; (define (mi-keys-add k)
;;   (mi-keys (append (mi-keys) (list k))))

;; (define (mi-keys-rm k)
;;   (mi-keys (remove k (mi-keys))))

;; (define (mi-keys-available)
;;   (pair? (mi-keys)))

;; (define (mi-keys-pop)
;;   (if (pair? (mi-keys))
;;       (let ([k (car (mi-keys))])
;; 	(mi-keys (cdr (mi-keys)))
;; 	k)
;;       #f))

(define mi-key (make-parameter #f))
(define mi-keymod (make-parameter '()))
(define mi-txt (make-parameter #f))

(define mi-cr (make-parameter #f))
(define mi-cairo-surface (make-parameter #f))

(define fps (make-parameter 25))
(define mi-frame-number (make-parameter 0))

(import (srfi s26 cut)) 
(import (matchable))

(include "utils.ss")
(include "css-color.ss")
(include "draw.ss")

(include "css.ss")

(include "layout.ss")

(include "transition.ss")

(include "element.ss")

(include "widgets.ss")

(include "render.ss")

(include "repl.ss")
      
(include "event-loop.ss")

  
