(in-package #:qix)

(defclass gl-material ()
  ((face
    :accessor face
    :initform :front
    :initarg :face)
   (ambient
    :initform nil)
   (diffuse
    :initform nil)
   (specular    
    :initform nil)
   (emission
    :initform nil)))

(defmethod initialize-instance :after ((this gl-material) &key ambient diffuse specular emission)
  (if ambient (apply #'set-ambient this ambient))
  (if diffuse (apply #'set-diffuse this diffuse))
  (if specular (apply #'set-specular this specular))
  (if emission (apply #'set-emission this emission)))


(defmethod set-ambient ((this gl-material) r g b &optional (a 1))
    (setf (slot-value this 'ambient) (vector r g b a)))

(defmethod unset-ambient ((this gl-material))
  (setf (slot-value this 'ambient) nil))

(defmethod get-ambient ((this gl-material))
  (coerce (slot-value this 'ambient) 'list))

(defmethod set-diffuse ((this gl-material) r g b &optional (a 1))
    (setf (slot-value this 'diffuse) (vector r g b a)))

(defmethod unset-diffuse ((this gl-material))
    (setf (slot-value this 'diffuse) nil))

(defmethod get-diffuse ((this gl-material))
  (coerce (slot-value this 'diffuse) 'list))

(defmethod set-specular ((this gl-material) r g b &optional (a 1))
  (setf (slot-value this 'specular) (vector r g b a)))

(defmethod unset-specular ((this gl-material))
  (setf (slot-value this 'specular) nil))

(defmethod get-specular ((this gl-material))
  (coerce (slot-value this 'specular) 'list))

(defmethod set-emission ((this gl-material) r g b &optional (a 1))
  (setf (slot-value this 'emission) (vector r g b a)))

(defmethod unset-emission ((this gl-material))
  (setf (slot-value this 'emission) nil))

(defmethod get-emission ((this gl-material))
  (coerce (slot-value this 'emission) 'list))


(defmethod use ((this gl-material))
  (with-slots ((a ambient)
	       (d diffuse)
	       (s specular)
	       (e emission)) this
    (if a
	(gl:material (face this) :ambient a))
;	(gl:material (face this) :ambient (vector 0 0 0 1)))
    (if d
	(gl:material (face this) :diffuse d))
;	(gl:material (face this) :diffuse (vector 0 0 0 1)))
    (if s 
	(gl:material (face this) :specular s))
;	(gl:material (face this) :specular (vector 0 0 0 1)))
    (if e
	(gl:material (face this) :emission e))))
;	(gl:material (face this) :emission (vector 0 0 0 1)))))


(defun get-current-material-property (face name)
    (cffi:with-foreign-object (p :float 4)
      (%gl:get-material-fv face name p)
      (loop for i from 0 to 3
	 collect (cffi:mem-aref p :float i))))
