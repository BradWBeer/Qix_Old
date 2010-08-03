;(in-package #:qix)


(setq *handler-hash* (make-hash-table :test 'eq))

(defun register-handler (obj handler)
  (setf (gethash obj *handler-hash*)
	(append  (gethash obj *handler-hash*) (list handler)))
  handler)


(defun unregister-handler (obj handler)
  (unless (setf (gethash obj *handler-hash*)
		(remove-if (lambda (x)
			     (eq x handler))
			   (gethash obj *handler-hash*)))
    (remhash obj *handler-hash*)))


(defun fire (object &rest e)
  (let ((handlers (gethash object *handler-hash*)))
    (loop 
       for i in handlers
       do (setf e (apply i e))
       while e)))
