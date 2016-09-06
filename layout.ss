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

(define layout-state (make-eq-hashtable))

(define (get-last-coords id)
  (check-arg symbol? id get-last-coords)

  (let ([e (mi-el-by-id id)])
    (if e 
	(parameterize ([mi-el e])
		      (values (mi-x) (mi-y) (mi-w) (mi-h)))
	(values 0 0 0 0))))

(define (layout-element element)  

  (check-arg mi-element? element layout-element)

  (let* ([position (mi-element-position element)]
	 [x (if (eq? position 'static) 0 (mi-element-x element))]
	 [y (if (eq? position 'static) 0 (mi-element-y element))]
	 [w (mi-element-w element)]
	 [h (mi-element-h element)]
	 [margin (mi-element-margin element)]
	 [padding (mi-element-padding element)]
	 [parent (mi-element-parent element)]
	 [p-padding (mi-element-padding parent)]
	 [csz (mi-element-content-size element)]
	 [w* (case w 
	       [expand 0]
	       [none (+ (* 2 padding) (if (list? csz) (car csz) 0))]
	       [else w])]
	 [h* (case h
	       [expand 0]
	       [none  (+ (* 2 padding) (if (list? csz) (cadr csz) 0))]
	       [else h])]
	 )
   ; (printf "h ~d w ~d h* ~d w* ~d position ~d csz ~d~n" h w h* w* position csz)
    (cond [(eq? position 'absolute)
 	   (values x y w* h*)] ;; FIXME absolute should positioned relative to the nearest positioned ancestor (e.g. not static)
	  [else 
	   (let* ([p-id (mi-element-id parent)]
		  [state (hashtable-ref layout-state (mi-element-id parent) #f)]
		  [p-x (mi-element-x parent)]
		  [p-y (mi-element-y parent)]
		  [p-w (mi-element-w parent)]
		  [p-h (mi-element-h parent)])
		  ;; (printf "state: ~d x ~d y ~d w ~d h ~d p-x ~d p-y ~d p-w ~d p-h ~d upd ~d~n" state x y w h p-x p-y p-w p-h update)
	     (match state
		    [(s-x s-y s-w s-h) 
		     ;(printf "w: ~d~n" w)
		     
		     (let-values 
			 ([(new-state ret)
			  (cond 
			   #;[(equal? `(s-x s-y s-w s-h) '(0 0 0 0))
			   (values (list (+ p-x margin p-padding)))]
			   [(> (+ w* s-w s-x (* 2 p-padding)) p-w)
			    ;; LINE BREAK
			    (if (eq? w 'expand) (set! w* (- p-w (* 2 p-padding) (* 2 margin) )))
			    (if (eq? h 'expand) (set! h* (- p-h (* 2 p-padding) (* 2 margin) )))
			    (values
			     (list (+ x p-x p-padding margin) 
				   (+ y s-y s-h) 
				   (+ w*  2 margin)
				   (+ h* (* 2 margin)))
			     (list (+ x p-x p-padding margin) 
				   (+ y margin s-y s-h)
				   w* 
				   h*))]
			   [else 
			    ;; SAME LINE
			    (if (eq? w 'expand) (set! w* (- p-w s-w (* 2 p-padding) (* 2 margin) )))
			    (if (eq? h 'expand) (set! h* (- p-h (* 2 p-padding) (* 2 margin) )))
			    (values
			     (list (+ x s-x s-w margin) 
				   (+ y s-y)
				   (+ x w* margin)
				   (max (+ h* margin)  s-h))
			     (list (+ x s-x s-w margin)
				   (+ y margin s-y)
				   w* 
				   h*))])])
		       (hashtable-set! layout-state p-id new-state)
		       (apply values ret))]
		    [else (values 0 0 0 0)]))])))

(define (mi-force-break id)
  (check-arg symbol? id mi-force-break)
  (let ([coord (hashtable-ref layout-state id #f)])
    (if coord
	(hashtable-set! layout-state id (match coord [(x y w h) (list 99999999 0 0 (+ h y))])))))
  
(define (start-layout element)
  (check-arg mi-element? element start-layout)
  (hashtable-set! layout-state (mi-element-id element) 
		  (list (+ (mi-element-padding element) (mi-element-x element)) 
			(+ (mi-element-padding element) (mi-element-y element)) 0 0)))
(define (end-layout element)
  (check-arg mi-element? element end-layout)
  (hashtable-set! layout-state (mi-element-id element) #f))
