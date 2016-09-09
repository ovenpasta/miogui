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

;(trace make-mi-element)
(define miogui-user-render (make-parameter values))


(define (miogui-run)
  (define last-frame-time (current-time))
  (mi-frame-number 0)
  (printf "starting event loop..\n")
  (sdl-start-text-input)
  (call/cc 
   (lambda (quit)
     (let loop ()
       ;;(define (sdl-poll-event* . x)
       ;; (with-interrupts-disabled (apply sdl-poll-event x)))
       (let ([el (make-mi-element 'window 'window-1 #f #f
				  (make-mi-element 'null 'null #f #f #f))])
	 (mi-el el)
	 (mi-element-w-set! el (mi-window-width))
	 (mi-element-h-set! el (mi-window-height))
	 (mi-element-x-set! el 0)
	 (mi-element-y-set! el 0)
	 (mi-element-style-set! el (alist->hashtable 
				    `((z-index 0)
				      (width ,(mi-window-width)) 
				      (height ,(mi-window-height))
				      (position absolute)))))

       (guard (x [else (printf "ERROR IN RENDER ") (display-condition x)(newline) #;(sleep-s 1) #f])
	      (render-stuff (miogui-user-render)))
       
       (let poll-event-loop ()
	 (sdl-let-ref-call 
	  sdl-poll-event ((e sdl-event-t &)) result
	  ;(printf "~d ~d\n" e result)
	  (when (not (zero? result))
		(let-struct 
		 e sdl-event-t (type)
		 (case (sdl-event-type-ref type)
		   [quit (printf "quit\n") (quit)]
		   [keydown (let* ([sym (event-keyboard-keysym-sym e)]
				   [sym-name (sdl-keycode-ref sym)])
			      (printf "keydown ~x ~d\n" sym (sdl-keycode-ref sym))
			      (if (eq? sym-name 'q) (quit)))]
		   [textinput (let* ([ti (ftype-&ref sdl-event-t (text) e)]
				     [text (char*-array->string
					    (ftype-&ref sdl-text-input-event-t (text) ti) 32)])
				(printf "text input \"~d\"\n" text )
				(if (string=? text "q") (quit)))]
		   [mousemotion (let* ([mousemotion (ftype-&ref sdl-event-t (motion) e)])
				  (let-struct mousemotion sdl-mouse-motion-event-t
					      (x y xrel yrel state)
					      (mi-mouse-x x) (mi-mouse-y y)
					      (if (region-hit? 0 0 (mi-window-width) (mi-window-height))
						  (sdl-capture-mouse #t)
						  (sdl-capture-mouse #f))
					      ;(printf "mouse moved ~d ~d ~d ~d ~d" x y xrel yrel state)
					      ))]
		   [mousewheel (let* ([wheel (ftype-&ref sdl-event-t (wheel) e)])
				 (let-struct wheel sdl-mouse-wheel-event-t (x y window-id)
					     (printf "mouse wheel ~d ~d\n" x y)))]
		   
		   [mousebuttondown (let* ([button (event-mouse-button e)]
					   [button-name (sdl-button-ref button)])
				      (printf "mouse down ~d ~d\n" button button-name)
				      (when (eq? button-name 'left)
					    (mi-mouse-down? #t)))]
		   [mousebuttonup (let* ([button (event-mouse-button e)]
					 [button-name (sdl-button-ref button)])
				    (printf "mouse up ~d ~d\n" button button-name)
				    (when (eq? button-name 'left)
					  (mi-mouse-down? #f)))]
		   [windowevent 
		    (let-struct (ftype-&ref sdl-event-t (window) e) sdl-window-event-t (event)
				(let ([we (sdl-window-event-enum-ref event)])
				  #t))]
				  ;(printf "windowevent ~d\n" we)))]
				  ;(case we
				  ;  [enter (sdl-capture-mouse #t)])))]
				    ;[leave (sdl-capture-mouse #f)]))]
				    
		   [mousebuttondown (printf "mousebuttondown\n")]))
		(poll-event-loop))))
       ;;(sleep (make-time 'time-duration (exact (truncate (* 10e6  (/ 60. (fps))))) 0))
              
       ;;FIXME, compute the sleep time from the difference of last frame and fps
       (sleep-s (/ 1. (fps)))
       ;(let ([t (time-nanosecond (time-difference (current-time) last-frame-time ))])
       ;  (printf "t: ~d ms ~d fps~n" (exact->inexact (/ t 1000000)) (exact->inexact (/ 1000000000 t))))
       (set! last-frame-time (current-time))
       (mi-frame-number (+ (mi-frame-number) 1))
       ;(sdl-delay (exact (truncate (/ 1000. (fps)))))
       (my-local-repl)
       (loop)
       )))
       (sdl-capture-mouse #f)
       (printf "exiting event loop\n"))
