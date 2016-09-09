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

(define-syntax check-arg
  (lambda (stx)
    (syntax-case stx ()
      [(_ pred val caller)
       (and (identifier? #'val) (identifier? #'caller))
       #'(unless (pred val)
		 (assertion-violationf 'caller "check-arg failed (~d ~d:~d) " 'pred 'val val))])))


(define (print-condition e)
  (printf "~d: ~d with irritants ~d~n" 
	  (if (who-condition? e) (condition-who e) 'unknown)
	  (if (message-condition? e) (condition-message e) "")
	  (if (irritants-condition? e) (condition-irritants e) "")))

(define (region-hit? x y w h)
  (check-arg number? x region-hit?)
  (check-arg number? y region-hit?)
  (check-arg number? w region-hit?)
  (check-arg number? h region-hit?)

  (not (or (< (mi-mouse-x) x) (< (mi-mouse-y) y)
	   (>= (mi-mouse-x) (+ x w))
	   (>= (mi-mouse-y) (+ y h)))))

(define (float-sec->time-duration d)
  (check-arg number? d float-sec->time-duration)
  (if (>= d 1) 
      (let ([trunc (exact (truncate d))])
	(make-time 'time-duration (exact (truncate (* 1000000000 (- d trunc)))) trunc))
      (make-time 'time-duration (exact (truncate (* 1000000000 d))) 0)))

(define (sleep-s s)
  (check-arg number? s sleep-s)
  (sleep (float-sec->time-duration s)))

(define-ftype-allocator new-uint32 uint32)
(define-ftype-allocator new-int int)

;; (let ([format (new-uint32)] [access (new-int)] [w (new-int)] [h (new-int)])
;;   (sdl-query-texture tex format access w h)
;;   (printf "~x ~d ~d ~d\n" format access w h))

;; (sdl-let-ref-call sdl-query-texture 
;; 		  (tex (format uint32) (access int) (w int) ( h int)) 
;; 		  result
;; 		  (printf "~x ~d ~d ~d -> ~d\n" format access w h result) )

;; (define r (new-struct sdl-rect-t (x 0) (y 0) (w 10) (h 10)))

;; (let-struct r sdl-rect-t (x y w h) 
;; 	    (printf "~d ~d ~d ~d\n" x y w h)

;; 	    ;(sdl-delay 1000)
;; 	    )

(define (time-float x)
  (check-arg time? x time-float)
  (+ (time-second x) (/ (time-nanosecond x) 10e8)))

;; FIXME FIND A WAY TO CREATE A MACRO THAT GENERATES A UNIQUE ID EACH TIME IT IS EXPANDED
;; USE make-compile-time-value perhaps
 
(alias p10e parameterize)

(define (compare-hashes a b)
  (check-arg hashtable? a compare-hashes)
  (check-arg hashtable? b compare-hashes)

  (letrec ([cmp (lambda (x y) 
		  (< (symbol-hash (car x)) (symbol-hash (car y))))]
	   [sort-hash (lambda (x)
			(sort cmp (hashtable->alist x)))])
    (equal? (sort-hash a) (sort-hash b))))

(define (none? x)
  (eq? x 'none))

(define (not-none? x)
  (not (none? x)))
