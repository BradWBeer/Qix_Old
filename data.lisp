;(in-package #:qix)


(defun string-insert (string text &optional (pos (1- (length string))))
  (concatenate 'string
	       (subseq string 0 pos)
	       text
	       (subseq string pos (1- (length string)))))

(defun string-remove (string pos &optional (len 1))
  (let ((str-len (length string)))
    (cond ((>= pos str-len)  string)
	  ((>= (+ pos len) str-len) (subseq string 0 pos))
	  ((zerop pos) (subseq string len))
	  (t (concatenate 'string 
			  (subseq string 0 pos)
			  (subseq string (+ pos len)))))))
		    

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

	   