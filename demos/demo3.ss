
(include "miogui.ss")

(stylesheet '(  [panel ==> 
		       (padding 10) 
		       (border-style solid) 
		       (background-color red)
		       (border-width 1) 
		       (border-color black)]
		[label ==>
		 (align-self flex-start)
		       (color red) 
		       (padding 5) 
		       (border-width 1) 
		       (border-color blue)]

		[(class first) ==>
		 (align-self flex-start)
		 (min-height 40) (min-width 160)]

		[(class second) ==>
		 (align-self flex-center) ]
		[(class third) ==>
		 (align-self flex-end)
		 (order 2) (color blue) ]
		[(class fourth) ==>
		 (align-self stretch) 
		 (align-self flex-end) ]

		[(class great) ==> 
		 (position absolute) (display flex)
		 (background-color (rgb 125 125 125))]

		[(id panel1) ==> 
		 (width 500) (height 100) (top 10)
		 (flex-direction row)
		 (justify-content flex-end)]

		[(id panel2) ==> 
		 (width 200) (height 220) (top 120)
		 (flex-direction column)
		 (justify-content flex-start)]

		[(id panel3) ==> 
		 (width 200) (height 220) (top 120) (left 220)
		 (flex-direction column-reverse)
		 (justify-content space-around)]
		[(id panel4) ==> 
		 (width 500) (height 100) (top 350) (left 30)
		 (flex-direction row-reverse)
		 (justify-content space-between)]))

(init-sdl "FLEXBOX1 align-items")

(fps 25)
(miogui-user-render
 (lambda ()
  (define some-labels (lambda ()
			(p10e ([mi-class 'first])
			      (label (mi-id 'l1) "l1: flex-start"))
			(p10e ([mi-class 'second])
			      (label (mi-id 'l2) "l2: flex-center"))
			(p10e ([mi-class 'third]) 
			      (label (mi-id 'l3) "l3: flex-end , order 1"))
			(p10e ([mi-class 'fourth])
			      (label (mi-id 'l4) "l4: stretch"))))

  (p10e ([mi-class 'great])
        (panel 'panel1 some-labels)
        (panel 'panel2 some-labels)
        (panel 'panel3 some-labels)
	(panel 'panel4 some-labels))
  (debug-tooltip)))

(miogui-run)
