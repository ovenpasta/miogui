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

(define transitions (make-eq-hashtable))


(define (widget-old-style id)
  (check-arg symbol? id widget-old-style)
  (hashtable-ref style-table id (make-eq-hashtable)))

(define (get-transition-ratio trans duration)
  (cond 
   [(zero? duration) 1.0]
   [trans
    (let* ([t (current-time)]
	   [d (time-float (time-difference t (car trans)))])
      (/ d duration))]
   [else 0]))

(define (color->list c)
  (list (color-r c) (color-g c) (color-b c) (color-a c)))

(define (list->color c)
  (apply make-color c))

(define (number-transition a b r)
  (check-arg number? a number-transition)
  (check-arg number? b number-transition)
  (check-arg number? r number-transition)

  (+ a (* (- b a) r)))

(define (color-transition color1 color2 ratio)
  (check-arg color? color1 color-transition)
  (check-arg color? color2 color-transition)
  (check-arg number? ratio color-transition)
  
  (apply make-color
	 (map (lambda (f)
		(number-transition (f color1) (f color2) ratio) )
	      (list color-r color-g color-b color-a))))

;  (list->color (map (lambda (a b) (+ a (* (- b a) ratio))) 
;		    (color->list color1) (color->list color2))))
(define style? hashtable?)

(define (start-transition element style-a style-b)
  (check-arg mi-element? element start-transition)
  (check-arg style? style-a start-transition)
  (check-arg style? style-b start-transition)

  ;(printf "starting transition of ~d from ~d to ~d" (mi-element-id element) (hashtable->alist style-a) (hashtable->alist style-b))
  (let ([time (current-time)])
    (hashtable-set! transitions (mi-element-id element) (list time style-a style-b))))

(define (end-transition element)
  (check-arg mi-element? element end-transition)
  (hashtable-delete! transitions (mi-element-id element)))

(define (eventually-end-transition element duration)
  (check-arg mi-element? element eventually-end-transition)
  (check-arg number? duration eventually-end-transition)

  (let ([t (hashtable-ref transitions (mi-element-id element) #f)])
    (if (and t (> (time-float (time-difference (current-time) (car t))) duration))
	(end-transition element)
	#f)))

(define (style-transition style-a style-b ratio)
  (check-arg style?  style-a style-transition)
  (check-arg style?  style-b style-transition)
  (check-arg number? ratio style-transition)

  (alist->hashtable
   (map (lambda (a)
	 (define name (car a))
	 (define val (value-or-list (cadr a)))
	 (define val2 (style-query style-b name val))
	 ;(printf "name val val2 ~d ~d ~d~n" name val val2)
	 (if (equal? val val2)
	     a
	     (case name
	       [(width height left top border-radius border-width font-size padding margin) 
		(if (and (number? val) (number? val2))
		    (list name (number-transition val val2 ratio))
		    (list name val2))]
	       [(color background-color border-color)
		(list name (guard (e [else (->color val2)])
			     (color-transition (->color val) (->color val2) ratio)))]
	       [else a])))
       (hashtable->alist style-a))))
