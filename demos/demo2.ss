
(include "miogui.ss")

(stylesheet '( [* ==> (font-family "Sans")]
	       [button ==>  
			(color red)
			(background-color (rgbf 0.1 0.1 0.8 1))
			(border 1 solid (rgbf 0 0 0.9 1))
			(border-radius 6)
			(padding 10)
			(font-size 15)
			(text-align center)
			(transition-duration 0.2)]
		[(and button (: hover)) ==> 
		 (border-color green) (background-color (rgbf 0.2 0.2 0.9 1))]
		[(> button label) ==> (color white)]
		[(and button (: pressed)) ==>
		 (background-color (rgb 200 200 200)) 
		 (color blue)
		 (transition-duration 0)]
		[(: focus) ==> 
		 (outline-style solid)
		 (outline-width 3) 
		 (outline-color (rgba 20 20 20 150))]
		[panel ==>  
		       (padding 10) (width 200 ) (height 200) 
		       (border 1 solid black) (background-color red)]
		[label ==> 
		       (color black) (padding 5) 
		       (background-color white)]
		[slider ==> (height 20) (color black) (padding 2)]

		[(id panel-1) ==> 
		 (width 600) (height 480) (top 0) (left 0) (position absolute) 
		 (background-color (rgb 125 125 125))
		 (display flex)
		 (justify-content space-around)
		 (align-items center)
		 (flex-direction column)]
		[(id label1) ==>
		 (align-self stretch)]
		[ button ==> (text-align center)]
		[ label ==> (text-align center)]
		[(or textline intline floline) 
		 ==> 
		 (border-left 15 solid red)
		 (color black) (border-style solid) 
		 (background-color white)
		 (padding 5)
		 (min-width 200)]
		[intline ==>  (text-align center) ]
		[floline ==> (text-align right)]
		))

(init-sdl "buttons")
(define my-text (make-parameter "some editable text!"))
(define my-int (make-parameter 543210))
(define my-flo (make-parameter 3.141592))
(miogui-user-render
 (lambda ()
  ;(fps 25)
  (panel 'panel-1
	 (lambda () 
	   (if (button 'button1 "BUTTON 1")
	       (printf "BUTTON 1 CLICKED!\n"))
	   
	   (if (button 'button2 (format "FRAME NUMBER: ~d" (mi-frame-number)))
	       (printf "BUTTON 2 CLICKED!\n"))
	   (when (button 'button3 (format "FPS: ~,2F" mi-stat-fps))
		 (printf "BUTTON3 CLICKED!\n"))
	   (label 'label1 "1\nGOOD MORNING!\nLine 2\nLine 3\nLine 4")
	   (textline 'text1 my-text)
	   (intline 'int1 my-int)
	   (floline 'flo1 my-flo 4)))
  (debug-tooltip)))

(miogui-run)
