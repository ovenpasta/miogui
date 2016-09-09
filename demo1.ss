
(include "miogui.ss")

(stylesheet '([* ==> 
			(position static)
			(box-sizing border-box)
			(font-family "Sans")
			(font-size 12)
			(line-height 1.2)
			(z-index auto)
			(text-align left)]
		     [button ==>
			     (width 100) 
			     (height 50) 
			     (color red)
			     (background-color (rgbf 0 1 0 0.5))
			     (border-style solid)
			     (border-color red)
			     (border-width 1) 
			     (border-radius 7)
			     (text-align center)]
		     [(and button (: hover)) ==> (border-color green) (background-color blue)]
		     [(and (: hover) (id button2)) ==> (background-color (rgbf 0.5 0.5 0.5)) (color blue)]
		     [(and (: pressed) (id button2)) ==>
		      (background-color (rgb 200 200 200)) (color blue) (transition-duration 0)]
		     [(> * (id button1)) ==> (border-width 4)]
		     [(id button1) ==>
		      (font-weight bold)
		      (padding 7)]
		     [(id button2) ==> (left 200) (top 200) (width 150 !important)
		      (background-color black)
		      (transition-duration 1)]
		     [(id panel-1) ==> 
		      (width 100 %) (height 89 %) (top 0) (left 0) (position absolute) 
		      (background-color (rgb 125 125 125))]
		     [(> (id panel-1) button) ==> (width 27 %) (height 10 %)]
		     [panel ==> 
			    (padding 10) (width 100 %) (border-style solid)
			    (border-width 1) (border-color black)]
		     [label ==>
			     (color black) (padding 5) (height 25)
			     (border-width 1) (border-color blue)]
		     [hslider ==> (height 40) (color black)]
		     [(id lbl2) ==> (width 90 %) (margin 0)]
		     [(id tg1) ==> (border-width 1) (border-color red) (background-color red)
		      (width expand) (height 200) (padding 5)]
		     [(id panel-2) ==> (height 200) (width expand) (border-color red) (border-width 1)]
		     [(id tg1::panel) ==>  (height 100 ) (width expand)]
		     [(id tg1::button) ==> (height 50  ) (width expand)]
		     [(> (id panel-1) slider) ==> (width expand) (margin 5)]
		     [(> (id tg1::panel) panel)
			  ==> (width expand) (height expand) ]
		     [(> (id panel3) label) ==> (width expand) ]
		     [slider-box ==> (background-color blue) (border-style none)
				 (border-radius 4)]
		     [(id slider1) ==> (width expand) (padding 5)] 
		     [(id slider2) ==> (width 25) (height expand) (padding 2)]
		     ))

(define toggle (make-parameter #f))
(define tg1-state (make-parameter #f))

(define slider-state 
  (make-parameter 0.25 
		  (lambda (x) 
		    (fps (+ 1 (* 100 x)))
		    x)))

(miogui-user-render
 (lambda ()
    (panel 'panel-1
	 (lambda () 
	   (if (button 'button1 "CIAO")
	       (printf "BUTTON CLICKED!\n"))
	  
	   (if (button 'button2 "NAMASTE")
	       (printf "BUTTON CLICKED NAMASTE!\n"))
	   (mi-force-break 'panel-1)
	   (when (button 'button3 (format "FPS: ~,2F" mi-stat-fps))
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
					    (label 'lbl4 "67890")
					    (mi-force-break 'panel3)
					    (label 'lbl3 "123455\n54321\nabcde")
					    )))) 
	   
	   (hslider 'slider1 slider-state)))

  (debug-tooltip)))

(init-sdl "DEMO1")

(miogui-run)
