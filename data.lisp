;(in-package #:qix)

(defmacro string-concat (&rest args)
  `(concatenate 'string ,@args))


(defun split-on-newline (string)
	   "Returns a list of substrings of string
divided by ONE newline each.
Note: Two consecutive spaces will be seen as
if there weak-pointer-value an empty string between them."
	   (loop for i = 0 then (1+ j)
	      as j = (position #\newline string :start i)
	      collect (subseq string i j)
	      while j))


(defun string-make (count)
  (make-array count :element-type 'character :adjustable t))

(defun string-resize (array newlen)
  (adjust-array array newlen))

(defun string-copy (source target)
  (loop for i from 0 to (1- (length source))
     do (setf (aref target i)
	      (aref source i)))
  target)

(defun string-insert (string text &optional (pos (1- (length string))))
  (concatenate 'string
	       (subseq string 0 pos)
	       text
	       (subseq string pos (length string))))



(defun string-list-insert (data str cursor)
  (let* ((newlist (split-on-newline str))
	 (length-newlist (length newlist)))
    (cond ((= length-newlist 1)
	   (setf (nth (first cursor) data) (string-insert (nth (first cursor) data) str (second cursor))))

	  ((= length-newlist 2)
	   (let ((broken-string (break-string (nth (first cursor) data) (second cursor))))
	     (setf (nth (first cursor) data)
		   (string-concat (first broken-string)
				  (first newlist)))
	     (list-insert-item data
			       (string-concat (second newlist)
					      (second broken-string))
			       (1+ (first cursor)))))

	  ((> length-newlist 2)
	   (let ((broken-string (break-string (nth (first cursor) data) (second cursor))))
	     (setf (nth (first cursor) data)
		   (string-concat (first broken-string)
				  (first newlist)))
	     (list-insert-list data (cdr newlist) (1+ (first cursor)))
	     (setf (nth (+ (first cursor)
			   (1- (length newlist))) data)
		   (string-concat (nth (+ (first cursor)
					  (1- (length newlist))) data)
				  (second broken-string))))))
    data))



(defun string-remove (string pos &optional (len 1))
  (let ((str-len (length string)))
    (cond ((>= pos str-len)  string)
	  ((>= (+ pos len) str-len) (subseq string 0 pos))
	  ((zerop pos) (subseq string len))
	  (t (concatenate 'string
			  (subseq string 0 pos)
			  (subseq string (+ pos len)))))))

(defun break-string (string pos)
	   (list (subseq string 0 pos)
		 (subseq string pos)))


(defun get-substring-list (data start &optional (end))
  (let ((start-line (first start))
	(start-index (second start)))
    (if end
	(let ((end-line (first end))
	      (end-index (second end)))
	  ;; simplest case, on the same line...
	  (cond ((= start-line end-line) (subseq (nth start-line data) start-index end-index))
		((= 1 (- end-line start-line))
		 (format nil "~A~%~A"
			 (subseq (nth start-line data) start-index)
			 (subseq (nth end-line data) 0 end-index)))
		(t (format nil "~A~%~A~A"
			   (subseq (nth start-line data) start-index)
			   (format nil "~{~A~%~}" (loop for i from (1+ start-line) to (1- end-line)
						     collect (nth i data)))
			   (subseq (nth end-line data) 0 end-index))))))))


(defun list-insert-item (l new-item &optional (pos 0))
  (if (>= pos (length l))
      (nconc l (list new-item))
      (let* ((cur (nthcdr pos l))
	     (old (car cur)))
	(setf (car cur) new-item)
	(setf (cdr cur) (cons old (cdr cur)))
	l)))


(defun list-insert-list (lst new-list &optional (pos 0))
  (if (>= pos (length lst))
          (nconc lst new-list)
	  (let* ((l (nthcdr pos lst))
		 (old-car (car l))
		 (old-cdr (cdr l)))

	    (setf (car l) (car new-list))
	    (setf (cdr l) (cdr (copy-list new-list)))
	    (setf (cdr (last l)) (cons old-car old-cdr))
	    lst)))

(defun list-remove (lst pos)
  (let* ((l (nthcdr pos lst)))
    (if (cdr l)
	(progn
	  (setf (car l) (cadr l))
	  (setf (cdr l) (cddr l)))
	(let ((l (nthcdr (1- pos) lst)))
	  (when l
	    (setf (cdr l) nil))))
    lst))

