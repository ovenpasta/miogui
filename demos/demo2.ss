
(include "miogui.ss")

(stylesheet '(  [button ==>
			(width 100) 
			(height 50) 
			(color red)
			(background-color (rgbf 0.1 0.1 0.8 1))
			(border-style solid)
			(border-color (rgbf 0 0 0.9 1))
			(border-width 1) 
			(border-radius 7)
			(padding 10)
			(text-align center)
			(transition-duration 0.2)]
		[(and button (: hover)) ==> 
		 (border-color green) (background-color (rgbf 0.2 0.2 0.9 1))]
		[(> button label) ==> (color white)]
		[(and button (: pressed)) ==>
		 (background-color (rgb 200 200 200)) 
		 (color blue) 
		 (transition-duration 0)]

		[panel ==> 
		       (padding 10) (width 200 ) (height 200) 
		       (border-style solid) (background-color red)
		       (border-width 1) (border-color black)]
		[label ==>
		       (color black) (padding 5) 
		       (border-width 1) (border-color blue)]
		[slider ==> (height 20) (color black) (padding 2)]

		[(id panel-1) ==> 
		 (width 100 %) (height 89 %) (top 0) (left 0) (position absolute) 
		 (background-color (rgb 125 125 125))
		 (display flex)
		 (justify-content space-around)
		 (align-items flex-center)
		 (flex-direction column)]
		[(id label1) ==>
		 (align-self stretch)
		 (height 50)]
		))


(init-sdl "buttons")

(miogui-user-render
 (lambda ()
  (fps 25)
  (panel 'panel-1
	 (lambda () 
	   (if (button 'button1 "BUTTON 1")
	       (printf "BUTTON 1 CLICKED!\n"))
	   
	   (if (button 'button2 (format "FRAME NUMBER: ~d" (mi-frame-number)))
	       (printf "BUTTON 2 CLICKED!\n"))
	   (when (button 'button3 (format "FPS: ~,2F" mi-stat-fps))
		 (printf "BUTTON3 CLICKED!\n"))
	   (label 'label1 "GOOD MORNING!")))
  (debug-tooltip)))

(miogui-run)
