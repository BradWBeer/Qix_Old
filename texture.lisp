(in-package #:qix)

;; USe PBO's Pixel Buffer Objects...
(defclass gl-texture ()
  ((id)
   (tex-type
    :reader tex-type
    :initform :texture-2d
    :initarg :tex-type)))

(defmethod get-id ((this gl-texture))
  (slot-value this 'id))

(defmethod bind ((this gl-texture))
;  (gl:enable :blend :texture-2d :depth-test)
;  (gl:blend-func :src-alpha :one-minus-src-alpha)
;  (gl:color-material :front :ambient-and-diffuse)
  (gl:bind-texture (tex-type this) (get-id this)))

(defmethod release-texture ((this gl-texture))
  (if (get-id this)
      (progn
	(gl:delete-textures (list (get-id this)))
	(setf (slot-value this 'id) nil))))

(defmethod initialize-instance :after ((this gl-texture) &key data)
  (unless (slot-boundp this 'id)
    (setf (slot-value this 'id) (car (gl:gen-textures 1)))))

(defmethod load-2d-image ((this gl-texture)
			  data
			  width
			  height &key
			  (format :bgra)
			  (data-type :unsigned-byte)
			  (wrap-s :repeat)
			  (wrap-t :repeat)
			  (mag-filter :nearest)
			  (min-filter :nearest)
			  (perspective-correction-hint :nicest)
			  (stride))
  (if stride (gl:pixel-store :unpack-row-length stride))
  (bind this)
  (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
  (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
  (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
  (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
  (gl:hint :perspective-correction-hint perspective-correction-hint)

  (gl:tex-image-2d :texture-2d 0 :rgba width height 0 format data-type data))