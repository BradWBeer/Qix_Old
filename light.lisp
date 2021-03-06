(in-package #:qix)

(defvar *lights* nil)
(defvar *global-ambient-light* (vector .2 .2 .2 0))
(defconstant *light0* #x4000)

(defun lights-reset ()
  (setf *lights* nil))

(defun lights-init ()
  (unless *lights*
    (setf *lights* (make-array (gl:get-integer :max-lights)))
    (loop for i from 0 to (1- (array-total-size *lights*))
	 do (setf (aref *lights* i) nil))))

(defun set-global-ambient-light (r g b &optional (a 0))
  (let ((v (vector r g b a)))
    (setf *global-ambient-light* v)
    (gl:light-model :light-model-ambient v)))

(defun get-enabled-lights ()
  (cons :color-material
	(cons :lighting 
	      (loop for l across *lights*
		 if l collect (get-id l)))))
  

(defclass gl-light ()
  ((id
    :initarg :id
    :initform nil)
   (location
    :initform (vector 0 0 0 0))
   (ambient
    :initform (vector 0 0 0 1))
   (diffuse
    :initform (vector 0 0 0 1))
   (specular    
    :initform (vector 0 0 0 1))
   (enable
    :accessor enable
    :initform t
    :initarg :enable)))
    
    

(defmethod initialize-instance :after ((this gl-light) &key)
  (lights-init)
  (setf (slot-value this 'id)
	(loop for i from 0
	   for l across *lights*
	   if (null l)
	   return (progn
		    (setf (aref *lights* i) this)
		    i)
	     finally (error "out of lights to give you!"))))


(defmethod release-light ((this gl-light))
  (setf (aref *lights* (slot-value this 'id)) nil))
  
(defmethod get-id ((this gl-light))
  (let ((index (slot-value this 'id)))
    (if index 
	(+ (slot-value this 'id) *light0*)
	nil)))


(defmethod set-location ((this gl-light) x y z &optional (a 0))
  (lights-init)
  (let ((v (vector x y z a)))
    (gl:light (get-id this) :position v)
    (setf (slot-value this 'location) v)))

(defmethod get-location ((this gl-light))
  (coerce (slot-value this 'location) 'list))

(defmethod set-ambient ((this gl-light) r g b &optional (a 1))
  (lights-init)
  (let ((v (vector r g b a)))
    (gl:light (get-id this) :ambient v)
    (setf (slot-value this 'ambient) v)))

(defmethod get-ambient ((this gl-light))
  (coerce (slot-value this 'ambient) 'list))

(defmethod set-diffuse ((this gl-light) r g b &optional (a 1))
  (lights-init)
  (let ((v (vector r g b a)))
    (gl:light (get-id this) :diffuse v)
    (setf (slot-value this 'diffuse) v)))

(defmethod get-diffuse ((this gl-light))
  (coerce (slot-value this 'diffuse) 'list))

(defmethod set-specular ((this gl-light) r g b &optional (a 1))
  (lights-init)
  (let ((v (vector r g b a)))
    (gl:light (get-id this) :specular v)
    (setf (slot-value this 'specular) v)))

(defmethod get-specular ((this gl-light))
  (coerce (slot-value this 'specular) 'list))

(defmethod set-constant-attenuation ((this gl-light) val)
  (gl:light (get-id this) :constant-attenuation val))

(defmethod set-linear-attenuation ((this gl-light) val)
  (gl:light (get-id this) :linear-attenuation val))

(defmethod set-quadratic-attenuation ((this gl-light) val)
  (gl:light (get-id this) :quadratic-attenuation val))


