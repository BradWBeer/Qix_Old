(proclaim '(optimize (speed 3)
	   (safety 0)
	   (debug 0)
	   (compilation-speed 0)))

(in-package #:qix)

(defmacro qix-win (&key (width 800) (height 600) (fullscreen nil) (title "") (caption "") (resize t)
		   on-init on-close
		   on-quit on-resize on-keyup on-keydown on-mousedown on-mouseup on-mousemove on-user-event on-idle
		   on-joymove on-joyup on-joydown on-joyhatmove on-joyballmove)
  `((lambda ()
      ;(proclaim '(optimize (speed 3) (space 0) (debug 0)))
      (sdl:with-init (sdl-cffi::sdl-init-joystick sdl-cffi::sdl-init-audio)
	(sdl-cffi::sdl-joystick-open 0)
	(sdl:window ,width ,height
		    :OPENGL t
		    :TITLE-CAPTION ,title
		    :ICON-CAPTION ,caption
		    :DOUBLE-BUFFER t
		    :RESIZABLE ,resize
		    :FULLSCREEN ,fullscreen
		    :opengl-attributes '(:SDL-GL-RED-SIZE 8
					 :SDL-GL-GREEN-SIZE 8
					 :SDL-GL-BLUE-SIZE 8
					 :SDL-GL-ALPHA-SIZE 8))
	(setf (sdl:frame-rate) nil)
	(gl:check-error)
	(sdl:enable-unicode)
	(sdl:resize-window ,width ,height)
	(gl:viewport 0 0 ,width ,height)
	(setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
	(setf *gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)


	;; (setf cl-opengl-bindings:*gl-get-proc-address*
	;;       #'sdl-cffi::sdl-gl-get-proc-address)
	,(if on-init
	     `(funcall ,on-init ,width ,height ,fullscreen ,resize))

	  (sdl:with-events ()
	    (:quit-event () t)
	    (:video-expose-event () (sdl:update-display))
	    (:VIDEO-RESIZE-EVENT (:W w :H h)
				 (sdl:resize-window w h)
				 ,(if on-resize
				      `(funcall ,on-resize w h)
				      `(progn
					 (format t "on-resize: w ~A h ~A~%" w h)
					 (sdl:resize-window w h)
					 (gl:viewport 0 0 w h))))
	    (:KEY-DOWN-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY :UNICODE UNICODE)
			     ,(if on-keydown
				 `(funcall ,on-keydown (code-char unicode) state scancode key mod mod-key unicode)
				 `(if (progn
					(format t "on-keydown: char ~A state ~A scancode ~A key ~A mod ~A mod-key ~A unicode-raw ~A~%"
						(code-char unicode) state scancode key mod mod-key unicode)
					(sdl:key= :SDL-KEY-ESCAPE KEY))
				      (SDL:PUSH-QUIT-EVENT))))
	    (:KEY-UP-EVENT
	     (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY)
	     ,(if on-keyup
		  `(funcall ,on-keyup state scancode key mod mod-key)
		  `(format t "on-keyup: state ~A scancode ~A key ~A mod ~A mod-key ~A~%"
			  state scancode key mod mod-key)))

	    (:MOUSE-BUTTON-DOWN-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y)
				      ,(if on-mousedown
					   `(funcall ,on-mousedown button state x y)
					   `(format t "on-mousedown: button ~A state ~A X ~A Y ~A~%"
						    button state x y)))


	    (:MOUSE-BUTTON-UP-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y)
				    ,(if on-mouseup
					 `(funcall ,on-mouseup button state x y)
					 `(format t "on-mouseup: button ~A state ~A X ~A Y ~A~%"
						  button state x y)))

	    (:MOUSE-MOTION-EVENT (:STATE STATE :X X :Y Y :X-REL X-REL :Y-REL Y-REL)
				 ,(if on-mousemove
					 `(funcall ,on-mousemove state x y x-rel y-rel)
					 `(format t "on-mousemove: state ~A X ~A Y ~A X-rel ~A Y-rel ~A~%"
						  state x y x-rel y-rel)))
	    (:JOY-AXIS-MOTION-EVENT (:WHICH WHICH :AXIS AXIS :VALUE VALUE)
				    ,(if on-joymove
					 `(funcall ,on-joymove which axis value)
					 `(format t "on-joymove: which ~A axis ~A value ~A~%"
						  which axis value)))
	    (:JOY-BUTTON-DOWN-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
				    ,(if on-joydown
					 `(funcall ,on-joydown which button state)
					 `(format t "on-joydown: which ~A Button ~A State ~A~%"
						  which button state)))
	    (:JOY-BUTTON-UP-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
				    ,(if on-joyup
					 `(funcall ,on-joyup which button state)
					 `(format t "on-joyup: which ~A Button ~A State ~A~%"
						  which button state)))
	    (:JOY-HAT-MOTION-EVENT (:WHICH WHICH :HAT HAT :VALUE VALUE)
				   ,(if on-joyhatmove
					`(funcall ,on-joyhatmove which hat value)
					`(format t "on-joyhatmove: which ~A hat ~A Value ~A~%"
						 which hat value)))
	    (:JOY-BALL-MOTION-EVENT (:WHICH WHICH :BALL BALL :X-REL X-REL :Y-REL Y-REL)
				    ,(if on-joyballmove
					 `(funcall ,on-joyballmove which ball x-rel y-rel)
					 `(format t "on-joyballmove: which ~A ball ~A x-rel ~A y-rel ~A~%"
						  which ball x-rel y-rel)))

	    (:USER-EVENT (:TYPE TYPE :CODE CODE :DATA1 DATA1 :DATA2 DATA2)
			 	 ,(if on-user-event
					 `(funcall ,on-user-event type code data1 data2)
					 `(format t "on-user-event: TYPE ~A CODE ~A DATA1 ~A DATA2 ~A~%"
						  type code data1 data2)))

	    (:IDLE ()
		   ,(if on-idle
			`(funcall ,on-idle)
			`(progn
			   (gl:enable :blend :smooth :depth-test)
			     (%gl:clear-color 1 1 1 0)
			     (gl:clear :color-buffer-bit :depth-buffer-bit)))
		   (sdl:update-display)))
	  (sdl-mixer:Close-Audio t)
	  ,(if on-close
	       `(funcall ,on-close))))))

