(asdf:defsystem #:qix
  :depends-on (lispbuilder-sdl lispbuilder-sdl-mixer cl-opengl cl-glu cl-cairo2 pango)
  :version "0.0.1"
  :components ((:file "package")
	       (:file "sdl-wrapper"
		      :depends-on ("package"))
	        (:file "buffers"
		       :depends-on ("package"))
	        (:file "texture"
		       :depends-on ("package" "buffers"))
	       (:file "material"
		      :depends-on ("package"))
	       (:file "light"
		      :depends-on ("package"))
	       (:file "mover"
		      :depends-on ("package"))
	       (:file "text-buffer"
		      :depends-on ("package"))
	       (:file "text-view"
		      :depends-on ("package"))
	       (:file "math"
		      :depends-on ("package"))))