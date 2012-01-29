(in-package #:qix)


(defclass qix-pipe (trivial-gray-streams:fundamental-character-input-stream
		    trivial-gray-streams:fundamental-character-output-stream
		    trivial-gray-streams:trivial-gray-stream-mixin)
  ((buffer
    :initform nil)
   (read-semaphore
    :initform (sb-thread:make-semaphore))
   (buffer-mutex
    :initform (sb-thread:make-mutex))))



(defmethod stream-read-char ((this qix-pipe))
  (let* ((empty)
	 (char (sb-thread:with-mutex ((slot-value this 'buffer-mutex) :wait-p t)
		 (if (slot-value this 'buffer)
		     (pop (slot-value this 'buffer))
		     (setf empty t)))))
    (if empty
	(progn
	  (sb-thread:wait-on-semaphore (slot-value this 'read-semaphore))
	  (stream-read-char this))
	char)))


(defmethod stream-write-char ((this qix-pipe) c)
  (sb-thread:with-mutex ((slot-value this 'buffer-mutex) :wait-p t)
    (if (slot-value this 'buffer)
	(setf (slot-value this 'buffer) (nconc (slot-value this 'buffer) (list c)))
	(progn
	  (setf (slot-value this 'buffer) (nconc (slot-value this 'buffer) (list c)))
	  (sb-thread:signal-semaphore (slot-value this 'read-semaphore))))))
