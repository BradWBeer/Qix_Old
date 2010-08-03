(in-package #:qix)

(defclass text-buffer ()
  ((cairo-surface
    :initform nil)
   (cairo-context
    :initform nil)
   (pango-context
    :initform nil)
   (pango-layout
    :initform nil)
   (data
    :accessor data
    :initform ""
    :initarg :data)
   (width
    :accessor width
    :initform (error "you must have a width")
    :initarg :width)
   (height
    :accessor height
    :initform (error "you must have a height!")
    :initarg :height)
   (storage-type
    :reader storage-type
    :initform :ARGB32
    :initarg :storage-type)
   (word-wrap
    :accessor word-wrap
    :initform :PANGO_WRAP_WORD
    :initarg :word-wrap)
   (text-font
    :accessor text-font
    :initform "Mono 10"
    :initarg :text-font)
   (cursor-position
    :accessor cpos
    :initform nil
    :initarg :cpos)
   (page-x
    :accessor page-x
    :initform 0
    :initarg :page-x)
   (page-y
    :accessor page-y
    :initform 0
    :initarg :page-y)
   (text-color
    :accessor text-color
    :initform '(255 255 255)
    :initarg :text-color)
   (background-color
    :accessor background-color
    :initform '(0 0 0)
    :initarg :background-color)))

(defmethod initialize-instance :after ((this text-buffer) &key data)
  (initialize-cairo-surface this)
  (initialize-cairo-context this)
  (initialize-pango-context this)
  (initialize-pango-layout this)

  (reset-data this data))


(defmethod reset-data ((this text-buffer) &optional str)
  (setf (slot-value this 'data)
	(if str
	    (loop with l = 1
	       for n = 0 then (+ pos l)
	       for pos = (loop for i from n to (1- (length str))
			    if (char= #\newline (aref str i))
			    return i)
	       if pos collect (list (- pos n) nil nil (subseq str n pos))
	       else collect (list (- (length str) n) nil nil (subseq str n))
	       while pos)
	    nil)))


(defmethod resize ((this text-buffer) width height)
  (setf (width this) width) 
  (setf (height this) height) 
  
  (release-pango-layout this)
  (release-pango-context this)
  (release-cairo-context this)
  (release-cairo-surface this)

  (initialize-cairo-surface this)
  (initialize-cairo-context this)
  (initialize-pango-context this)
  (initialize-pango-layout this))
  

(defmethod initialize-cairo-surface ((this text-buffer) &key)
  (release-cairo-surface this)
  (setf (slot-value this 'cairo-surface)
	(cairo:create-image-surface (storage-type this)
				    (width this)
				    (height this))))


(defmethod release-cairo-surface ((this text-buffer) &key)
  (with-slots ((csurf cairo-surface)) this
    (if csurf (destroy csurf))))


(defmethod initialize-cairo-context ((this text-buffer) &key)
  (release-cairo-context this)
  (setf (slot-value this 'cairo-context) (cairo:create-context (slot-value this 'cairo-surface))))


(defmethod release-cairo-context ((this text-buffer) &key)
  (with-slots ((cont cairo-context)) this
    (if cont (destroy cont))))


(defmethod get-cairo-context-pointer ((this text-buffer) &key)
  (slot-value  (slot-value this 'cairo-context) 'pointer))


(defmethod initialize-pango-context ((this text-buffer) &key)
  (release-pango-context this)
  (setf (slot-value this 'pango-context)
  	(pango_cairo_create_context
  	 (get-cairo-context-pointer this))))


(defmethod release-pango-context ((this text-buffer) &key)
  (with-slots ((pcon pango-context)) this
    (if pcon (g_object_unref pcon))))



(defmethod initialize-pango-layout ((this text-buffer) &key)
  (release-pango-layout this)
  (setf (slot-value this 'pango-layout)
  	(pango_layout_new (slot-value this 'pango-context)))
  (with-slots ((layout pango-layout)
	       (width width)
	       (height height)) this
    (pango_layout_set_width layout (* width PANGO_SCALE))
    (pango_layout_set_height layout (* height PANGO_SCALE))
    (pango_layout_set_wrap layout (word-wrap this))))



(defmethod release-pango-layout ((this text-buffer) &key)
  (with-slots ((playout pango-layout)) this
    (if playout (g_object_unref playout))))


(defmethod release-text-buffer ((this text-buffer))
  (release-cairo-surface this)
  (release-cairo-context this)
  (release-pango-context this)
  (release-pango-layout this)
  (with-slots ((csurf cairo-surface)
	       (ccontext cairo-context)
	       (pcontext pango-context)
	       (layout pango-layout)
	       (data data)
	       (h height)
	       (w width)
	       (st storage-type)
	       (cp cursor-position)) this
    (setf data nil
	  w nil
	  h nil
	  st nil
	  cp nil
	  csurf nil
	  ccontext nil
	  pcontext nil
	  layout nil)))


(defmethod draw-text ((this text-buffer) &key)
  (with-slots ((csurf cairo-surface)
	       (context cairo-context)
	       (pcontext pango-context)
	       (layout pango-layout)
	       (data data)
	       (h height)
	       (w width)
	       (st storage-type)
	       (cursor-position cursor-position)) this

    (calculate-line-dimentions this)

    (cairo:set-source-rgb 0 0 0 context)
    (cairo:paint context)
    (cairo:set-source-rgb 1 1 1 context)

    (let* ((desc (pango_font_description_from_string (text-font this)))
	   (cursor cursor-position))
    
      (loop
	 with pos = 0
	 for i from 0
	 for line in data
	 while (< pos (+ h (page-y this)))
	 do (let ((attrs (pango_attr_list_new))
		  (text (fourth line))
		  (text-len (first line)))
	      
	      (cairo:move-to 0 (- pos (page-y this)) context)
	      
	      (if (and cursor
		       (= (first cursor) i))
		  (progn 
		    
		    (unless (first cursor)
		      (setf (first cursor) (1- (length data))))
		    
		    (if (second cursor)
			
			(let* ((p (second cursor))
			       (p2 (1+ p)))
			  
			  (format t "cursor = ~A~%cp = ~A : cp1 = ~A~%" cursor p p2)
			  
			  (add-pango-attribute attrs (pango_attr_foreground_new 0 0 0) p p2)
			  (add-pango-attribute attrs (pango_attr_background_new 
						      (ezcolor 255) (ezcolor 255) (ezcolor 255)) p p2))
			
			(let* ((len (first line))
			       (len++ (1+ len)))
			  (add-pango-attribute attrs (pango_attr_foreground_new 0 0 0) len len++)
			  (add-pango-attribute attrs (pango_attr_background_new 
						      (ezcolor 255) (ezcolor 255) (ezcolor 255)) len len++)
			  
			  (incf text-len)
			  (setf text (concatenate 'string (fourth line) " "))))))
	    	    
		    
		  
	      (pango_layout_set_text layout text text-len)
	      
	      (pango_layout_set_attributes layout attrs)
	      (pango_layout_set_font_description layout desc)
	      (pango_cairo_update_layout (get-cairo-context-pointer this) layout)
	      (pango_cairo_show_layout (get-cairo-context-pointer this) layout)
	      (multiple-value-bind (x y) (get-layout-size layout)
		(incf pos y))
	      (pango_attr_list_unref attrs)))
      
      (pango_font_description_free desc))
    
    csurf))


(defmethod get-line-extents ((this text-buffer) line)
  (cffi:with-foreign-objects ((ink :pointer)
			      (logical :pointer))
    (with-slots ((layout pango-layout)) this
      (pango_layout_line_get_extents (pango_layout_get_line_readonly layout line) ink logical)      
      (values (/ (cffi:foreign-slot-value ink 'PangoRectangle 'x) PANGO_SCALE)
	      (/ (cffi:foreign-slot-value ink 'PangoRectangle 'y) PANGO_SCALE)))))

(defmethod layout-get-size ((this text-buffer))
  (get-layout-size (slot-value this 'pango-layout)))

(defun delete-from-center (loc str)
  
  )

(defun insert-into-center (loc str new-string)
  )

(defmethod insert-text ((this text-buffer) new-text &key)
  )

(defmethod get-index-of-line ((this text-buffer) line-number &key)
  )


(defmethod get-cursor-line ((this text-buffer))
)

(defmethod get-line-start-end ((this text-buffer) pos &key)
  )

(defmethod arrow-left ((this text-buffer) &key (count 0))
  (let* ((tmp-para (first (cpos this)))
	 (paragraph (if tmp-para tmp-para (1- (length (slot-value this 'data)))))
	 (index (second (cpos this)))
	 (layout (setup-layout this paragraph)))
    ;(setf (cpos this)
	  
	  (format t "paragraph = ~A  : index = ~A~%" paragraph index)

	  ;; There are three possibilites for the index, first (0), just-past-last (nil), and normal...
	  (cond
	    ;; past-last?
   	    ((null index) (let ((len (first (nth paragraph (slot-value this 'data)))))
			    ;; if the length of this paragraph is zero, move down another paragrah
			    (if (zerop len)
				(unless (zerop paragraph) (decf paragraph))
				(setf index (1- len)))))
	    ;; are we at zero, then move to a previous paragraph...
   	    ((zerop index) (unless (zerop paragraph)
			     (setf index nil)
			     (decf paragraph)))
	    
 	    (t
	     (format t "index = ~A  for move cursor...~%" index) 
	     (setf index (move-cursor-visually layout index :forward nil))))
	    
	  (setf (cpos this) (list paragraph index))))

	  
(defmethod arrow-right ((this text-buffer) &key)
  (let* ((tmp-para (first (cpos this)))
	 (paragraph (if tmp-para tmp-para (1- (length (slot-value this 'data)))))
	 (index (second (cpos this)))
	 (layout (setup-layout this paragraph))
	 (len (first (nth paragraph (slot-value this 'data)))))

    (format t "para ~A   index ~A  len  ~A" paragraph index len)

    (cond
      ((null index) (unless (>= paragraph (1- (length (slot-value this 'data))))
		      (incf paragraph)
		      (if (zerop (first (nth paragraph (slot-value this 'data))))
			  (setf index nil)			  
			  (setf index 0))))
      ((>= index (1- len)) (setf index nil))
      (t (setf index (move-cursor-visually layout index))))
    (setf (cpos this) (list paragraph index))))
       


(defmethod arrow-up ((this text-buffer) &key (count 0))
  

  )

(defmethod arrow-down ((this text-buffer) &key (count 0))
  )    

(defmethod backspace-key ((this text-buffer) &key (count 0))
  )

(defmethod delete-key ((this text-buffer) &key (count 0))
  )

(defmethod page-up ((this text-buffer) &key (count 0))
  )

(defmethod page-down ((this text-buffer) &key (count 0))
  )

(defmethod home ((this text-buffer) &key (count 0))
  )

(defmethod end ((this text-buffer) &key (count 0))
  )

(defmethod home-key ((this text-buffer) &key)
  )

(defmethod end-key ((this text-buffer) &key)
  )
		  
(defmethod image-surface-data ((this text-buffer))
  (cairo:image-surface-get-data (slot-value this 'cairo-surface) :pointer-only t))

(defmethod write-to-png ((this text-buffer) path &key)
  (cairo:surface-write-to-png (slot-value this 'cairo-surface) path))
	
(defmethod calculate-line-dimentions ((this text-buffer) &key (force-all nil))
  (setf (slot-value this 'data)
	(with-slots ((layout pango-layout)
		     (data data)) this  
	  (let* ((desc (pango_font_description_from_string (text-font this)))
		 (attrs (pango_attr_list_new)))
	    
	    (loop for i in (slot-value this 'data)
	       unless (and (second i) (third i) (null force-all))
	       collect (progn 		  
			 (pango_layout_set_text layout
						(let ((str (fourth i)))
						  (if (string= str "")
						      " " str))
						(if (zerop (first i))
						    1 (first i)))
			 (pango_layout_set_attributes layout attrs)
			 (pango_layout_set_font_description layout desc)
			 (pango_cairo_update_layout (get-cairo-context-pointer this) layout)
			 (multiple-value-bind (w h) (get-layout-size layout)
			   (let ((line-data (get-layout-lines-data layout)))
			     (list (first i) 
				   w 
				   h
				   (fourth i)
				   (first line-data)
				   (second line-data)))))
	       else collect i)))))


(defmethod get-scroll-by-line-number ((this text-buffer) x &key) 
  (calculate-line-dimentions this)
  (loop
     with line-number = 0
     with y-pos = 0
     for i in (slot-value this 'data)
     while (< (+ line-number (nth 4 i)) x)
     do (progn 
	  (incf line-number (nth 4 i))
	  (incf y-pos (nth 2 i)))
     finally (return (loop for j in (nth 5 i)
			while (> x line-number)
			do (progn
			     (incf line-number)
			     (incf y-pos (second j)))
			finally (return (list y-pos line-number))))))

		     
(defmethod get-line-count ((this text-buffer) &key)
  (calculate-line-dimentions this)
  (loop for i in (slot-value this 'data)
     sum (nth 4 i)))
	 
(defmethod setup-layout ((this text-buffer) paragraph &key add-space)
  (with-slots ((layout pango-layout)
	       (data data)) this  
    (let* ((line (nth paragraph data))
	   (desc (pango_font_description_from_string (text-font this)))
	   (attrs (pango_attr_list_new)))

      (let ((str (fourth line)))

	(if (or add-space
		(string= str ""))
	    (pango_layout_set_text layout
				   (concatenate 'string str " ")
				   (1+ (first line)))

	    (pango_layout_set_text layout str (first line)))
				   (first line))
	    
      (pango_layout_set_attributes layout attrs)
      (pango_layout_set_font_description layout desc)
      (pango_cairo_update_layout (get-cairo-context-pointer this) layout))
    (setf (slot-value this 'pango-layout) layout)))
			 

(defmethod get-line-number-of-index ((this text-buffer) line-number index)
  (calculate-line-dimentions this)
  (let* ((lay (nth line-number (slot-value this 'data)))
	 (lines (nth 5 lay)))
    (loop
       for i from 0
       for (x y start len) in lines
       if (and (>= index start)
	       (< index (+ start len)))
       do (return i))))
       
	 
		 
      
(defmethod up1 ((this text-buffer)) 
  (setup-layout this (first (cpos this)) :add-space t)
  (let* ((lines (second (get-layout-lines-data (slot-value this 'pango-layout))))
	(paragraph (first (cpos this)))
	(index (if (second (cpos this))
		   (second (cpos this)) (1- (first (nth paragraph (slot-value this 'data)))))))
		   
    (unless (>= 0 -1) (setf index 0))
    (format t "paragraph and index are : ~A ~A ~%" paragraph index)

    (multiple-value-bind (old-line x-pos) 
	(loop 
	   for i in lines
	   for x from 0
	   if (and (>= index (third i))
		   (< index (+ (third i) (fourth i))))
	   do (return (values x
			      (layout-line-index-to-x (fifth i) index))))

      (format t "old-line: ~A  x-pos: ~A ~%" old-line x-pos)
      (print "what about here?")
      ;(print paragraph)

      (if (or (null old-line) (zerop old-line))
	  (unless (zerop paragraph)
	    (print "run")
	    (print (setup-layout this (decf paragraph) :add-space t))
	    (setf (cpos this)
		  (list paragraph
			(let ((max-y (second (car (last (sixth (nth 1 (slot-value buf 'data))))))))
			  (if (or (null x-pos) (> x-pos max-y))
			      nil
			      (layout-line-x-to-index 
			       (fifth 
				(car (last (second (get-layout-lines-data (slot-value this 'pango-layout))))))
			       (if x-pos x-pos 0)))))))
	  (progn
	    (print "gump")
	    (setf (cpos this)
		(list paragraph
		      (layout-line-x-to-index (fifth (nth (1- old-line) lines)) x-pos))))))))

	   

(defmethod get-line-info ((this text-buffer))
	     (loop 
		for x from 0
		for i in (slot-value this 'data)
	      append (loop for j in (sixth i)
			  collect (append (list x) j))))

