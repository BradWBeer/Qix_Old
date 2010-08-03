(in-package #:qix)

(defclass gl-buffer () 
  ((id
    :initform nil
    :reader id)
   (size
    :initform 0
    :initarg :size
    :reader size)
   (data-type
    :initform :float
    :initarg :data-type
    :reader data-type)
   (target 
    :initform :array-buffer
    :initarg :target
    :reader target)
   (number-of-fields
    :initform 3
    :initarg :number-of-fields
    :accessor number-of-fields)
   (usage 
    :initarg :usage
    :initform :stream-copy
    :accessor usage)
   (buffer 
    :initform nil
    :reader buffer)))


(defmethod initialize-instance :after ((this gl-buffer) &key (data nil))
  (if data
      (set-data this data)))

(defmethod release-id ((this gl-buffer))
  (if (id this)
      (gl:delete-buffers (list (id this)))))

(defmethod get-id ((this gl-buffer))
  (when (id this)
      (gl:delete-buffers (list (id this))))
  (setf (slot-value this 'id)
  	(car (gl:gen-buffers 1))))

(defmethod bind ((this gl-buffer))
  (if (not (id this))
      (get-id this))
  (%gl:bind-buffer (target this) (id this)))

; you don't really need this but....
(defmethod un-bind ((this gl-buffer))
  (%gl:bind-buffer (target this) (cffi:null-pointer)))


(defmethod set-data ((this gl-buffer) data &key (number-of-fields nil) (size nil) (data-type nil) (usage nil))
  (if size
      	(setf (slot-value this 'size) size)
	(setf size
	      (setf (slot-value this 'size) 
		    (length data))))

  (if data-type (setf (slot-value this 'data-type) data-type))
  (if (null (data-type this)) 
      (error "Please give this buffer a data-type!"))
  (if number-of-fields (setf (number-of-fields this) number-of-fields))
  (if (null (number-of-fields this)) 
      (error "Please give this buffer an number-of-fields!"))
  (if usage (setf (usage this) usage))
  (if (null (usage this)) 
      (error "Please give this buffer a usage!"))

  (bind this)
  (cffi:with-foreign-object (d (data-type this) (size this))
    (loop for i from 0
	 for index in data
	 do (setf (cffi:mem-aref d (data-type this) i) index))

    (%gl:buffer-data (target this) (* (size this) (cffi:foreign-type-size (data-type this))) d (usage this))))


(defmethod map-buffer ((this gl-buffer) &key (access :read-write))
  (bind this)
  (setf (slot-value this 'buffer)
	(%gl:map-buffer (target this) :read-write)))


(defmethod unmap-buffer ((this gl-buffer))
  (setf (slot-value this 'buffer)
	(%gl:unmap-buffer (target this))))	

(defmethod set-vertex-pointer ((this gl-buffer))
  (gl:enable-client-state :vertex-array)
  (bind this)
  (%gl:vertex-pointer (number-of-fields this)
		      (gl::cffi-type-to-gl (data-type this))
		      0
		      (cffi-sys:null-pointer)))

(defmethod set-normal-pointer ((this gl-buffer))
  (gl:enable-client-state :normal-array)
  (bind this)
  (%gl:normal-pointer (gl::cffi-type-to-gl (data-type this))
		      0
		      (cffi-sys:null-pointer)))

(defmethod set-color-pointer ((this gl-buffer))
  (gl:enable-client-state :color-array)
  (bind this)
  (%gl:color-pointer (number-of-fields this)
		     (gl::cffi-type-to-gl (data-type this))
		     0
		     (cffi-sys:null-pointer)))


(defmethod set-secondary-color-pointer ((this gl-buffer))
  (gl:enable-client-state :secondary-color-array)
  (bind this)
  (%gl:secondary-color-pointer (number-of-fields this)
			       (gl::cffi-type-to-gl (data-type this))
			       0
			       (cffi-sys:null-pointer)))


(defmethod set-edge-pointer ((this gl-buffer))
  (gl:enable-client-state :edge-flag-array)
  (bind this)
  (%gl:edge-flag-pointer 0 (cffi-sys:null-pointer)))

(defmethod set-tex-pointer ((this gl-buffer))
  (gl:enable-client-state :texture-coord-array)
  (bind this)
  (%gl:tex-coord-pointer (number-of-fields this)
			 (gl::cffi-type-to-gl (data-type this))
			 0
			 (cffi-sys:null-pointer)))

(defmethod set-fog-coordinate-pointer ((this gl-buffer))
  (gl:enable-client-state :fog-coordinate-array)
  (bind this)
  (%gl:fog-coord-pointer (gl::cffi-type-to-gl (data-type this))
			 0
			 (cffi-sys:null-pointer)))

(defmacro with-mapped-buffer (buffer var &rest body) 
  `(unwind-protect (let ((,var (map-buffer ,buffer)))
		     ,@body)
     (unmap-buffer ,buffer)))
	 

(defclass gl-mesh () 
  ((primitive
    :accessor primitive
    :initarg :primitive
    :initform :triangles)
   (vertexes
    :accessor vertexes
    :initarg :vertexes
    :initform nil)
   (normals
    :accessor normals
    :initarg :normals
    :initform nil)
   (colors
    :accessor colors
    :initarg :colors
    :initform nil)
   (secondary-colors
    :accessor secondary-colors
    :initarg :secondary-colors
    :initform nil)
   (edges
    :accessor edges
    :initarg :edges
    :initform nil)
   (texture-coodinates
    :accessor tex
    :initarg tex
    :initform nil)
   (fog-coodinates
    :accessor fog
    :initarg fog
    :initform nil)
   (index-array
    :accessor index-array
    :initarg :index-array
    :initform nil)))
	 

(defmethod initialize-instance :after ((this gl-mesh) &key ))


(defmethod render ((this gl-mesh))
  (with-accessors ((v vertexes)
		   (n normals)
		   (c colors)
		   (texture tex)
		   (i index-array)
		   (e edges)
		   (s secondary-colors)
		   (f fog)
		   (p primitive))  this
    (if v (set-vertex-pointer v))
    (if n (set-normal-pointer n))
    (if c (set-color-pointer c))
    (if s (set-secondary-color-pointer s))
    (if texture (set-tex-pointer texture))
    (if f (set-fog-coordinate-pointer f))
    (if e (set-edge-pointer e))
    (if i
	(progn
	  (bind i)
	  (%gl:draw-elements p 
			     (size i)
			     (gl::cffi-type-to-gl (data-type i)) 
			     (cffi-sys:null-pointer)))
	(%gl:draw-arrays p 0 (/ (size v) (number-of-fields v))))
    
    (if v (%gl:disable-client-state :vertex-array))
    (if n (%gl:disable-client-state :normal-array))
    (if c (%gl:disable-client-state :color-array))
    (if texture (%gl:disable-client-state :texture-coord-array))
    (if e (%gl:disable-client-state :edge-flag-array))))



(defmethod release-buffers ((this gl-mesh))
  (with-accessors ((v vertexes)
		   (n normals)
		   (c colors)
		   (texture tex)
		   (i index-array)
		   (e edges)
		   (s secondary-colors)
		   (f fog)
		   (p primitive))  this
    (if v (release-id v))
    (if n (release-id n))
    (if c (release-id c))
    (if s (release-id s))
    (if texture (release-id texture))
    (if f (release-id f))
    (if e (release-id e))
    (if i (release-id i))))
