(defun read-whole-string-or-nothing (str)
  (let ((chars-read 0)
	(strlen (length str)))
    (handler-case
	(loop while (< chars-read strlen)
	   collect (multiple-value-bind (exp len) (read-from-string (subseq str chars-read))
		     (incf chars-read len)
		     exp))
      (end-of-file () nil))))


(defmacro with-retry (() &rest body)
  (let ((f-name (gensym)))
    `(labels ((,f-name ()
		(restart-case (progn ,@body)
		  (retry ()
		    :report (lambda (s)
			      (format s "RETRY:"))
		    (,f-name))
		  (abort ()
		    :report (lambda (s)
			      (format s "ABORT:"))
		    nil))))
       (,f-name))))


(defun eval-string (str)
  (let ((sexp (read-whole-string-or-nothing str)))
    (when sexp
      (loop
	 with *standard-output* = (make-string-output-stream)
	 with *error-output*    = (make-string-output-stream)
	 for i in sexp
	 for ret = (with-retry () (eval i))
	 finally (return (values ret
				 (get-output-stream-string *standard-output*)
				 (get-output-stream-string *error-output*)))))))

(defclass qix-stream (trivial-gray-streams:fundamental-character-input-stream
		      trivial-gray-streams:trivial-gray-stream-mixin) ())

(define-condition  out-of-data (error)  ())

(defmethod stream-read-char ((this qix-stream))
  (restart-case (error 'out-of-data)
    (return-a-char (x) (lambda (x) x))))


;; (with-retry ()
;;   (let ((*standard-output* (make-string-output-stream))
;; 	(*error-output* (make-string-output-stream)))
;;     (list (eval (read))
;; 	  (get-output-stream-string *standard-output*)
;; 	  (get-output-stream-string *debug-io*))))


;; (with-retry ()
;;   (loop
;;      with last-call
;;      for cmd in
;;        (with-input-from-string (s "(print \"hello\") nil")
;; 	 (loop
;; 	    with end-of-file = (gensym)
;; 	    for i = (read s nil end-of-file)
;; 	    while (not (eql i end-of-file))
;; 	    collect i))
;;      do (setf last-call (eval cmd))
;;      finally (return last-call)))



;; (defun eval-string (str)
;;   (with-retry ()
;;     (let ((*standard-output* (make-string-output-stream))
;; 	  (*error-output* (make-string-output-stream)))
;;       (list
;;        (loop
;; 	  with last-call
;; 	  for cmd in
;; 	    (with-input-from-string (s str)
;; 	      (loop
;; 		 with end-of-file = (gensym)
;; 		 for i = (read s nil end-of-file)
;; 		 while (not (eql i end-of-file))
;; 		 collect i))
;; 	  do (setf last-call (eval cmd))
;; 	  finally (return last-call))
;;        (get-output-stream-string *standard-output*)
;;        (get-output-stream-string *error-output*)))))


;; (handler-bind
;;     ((warning (lambda (x) (print x) nil)))
;;   (eval '(progn (warn tmp) (print "helo"))))
