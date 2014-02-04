(in-package #:qix)

(defclass mover ()
  ((period
    :accessor period
    :initform 1
    :initarg :period)
   (time-offset
    :accessor time-offset
    :initform 0
    :initarg :time-offset)
   (start-value
    :accessor start-value
    :initform 0
    :initarg :start-value)
   (end-value
    :accessor end-value
    :initform 1
    :initarg :end-value)
   (repeat
    :accessor repeat
    :initform nil
    :initarg :repeat)
   (functor
    :accessor functor
    :initform nil
    :initarg :functor)
   (start-time
    :accessor start-time
    :initform nil)
   (slope)
   (last-value
    :initform nil)))

(defmethod initialize-instance :after ((this mover) &key )
	   (with-slots (slope end-value start-value period) this
	     (setf slope (/ (- end-value start-value) period))))
	     ;(if start-now (start-mover this delay))))


(defmethod start-mover ((this mover) &key delay)
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (if delay
	(setf (start-time this) (+ now delay))
	      (setf (start-time this) now))))


(defmethod get-current-value ((this mover))
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)) (ret))
    (setf ret
	  (if (> (start-time this) now)
	      (start-value this)
	      (if (and (not (repeat this))
		       (< (+ (start-time this) (period this))
			  now))
		  (end-value this)
		  (rem (* (slot-value this 'slope)
			  (- now (start-time this)))
		       (end-value this)))))
    (let ((retval (if (functor this)
		      (funcall (functor this) ret)
		      ret))
	  (old-value (slot-value this 'last-value)))
      (values (setf (slot-value this 'last-value) retval)
	      (not (equal old-value retval))))))

