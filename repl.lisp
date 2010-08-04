


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


(with-retry () 
  (let ((*standard-output* (make-string-output-stream))
	(*error-output* (make-string-output-stream)))
    (list (eval (read))
	  (get-output-stream-string *standard-output*)
	  (get-output-stream-string *debug-io*))))


(with-retry ()
  (loop 
     with last-call 
     for cmd in  
       (with-input-from-string (s "(print \"hello\") nil")
	 (loop 
	    with end-of-file = (gensym)
	    for i = (read s nil end-of-file)
	    while (not (eql i end-of-file))
	    collect i))
     do (setf last-call (eval cmd))
     finally (return last-call)))



(defun eval-string (str)
  (with-retry ()
    (let ((*standard-output* (make-string-output-stream))
	  (*error-output* (make-string-output-stream)))
      (list 
       (loop 
	  with last-call 
	  for cmd in  
	    (with-input-from-string (s str)
	      (loop 
		 with end-of-file = (gensym)
		 for i = (read s nil end-of-file)
		 while (not (eql i end-of-file))
		 collect i))
	  do (setf last-call (eval cmd))
	  finally (return last-call))
       (get-output-stream-string *standard-output*)
       (get-output-stream-string *error-output*)))))


(handler-bind 
    ((warning (lambda (x) (print x) nil)))
  (eval '(progn (warn tmp) (print "helo"))))
