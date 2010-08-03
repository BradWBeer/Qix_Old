(in-package #:qix)

(defun rad->deg (x) 
  (* 57.2957795 x))

(defun deg->rad (x) 
  (/ x 57.2957795))

(defun law-of-cosines (a b c)
  (let ((aa (* (- (* a a) 
		  (* b b)
		  (* c c))))
	(bb (- (* 2 b c))))
    (if (zerop bb)
	90
	(rad->deg (acos (/ aa bb))))))			     


(defun transpose (matrix)
  (loop for row from 0 to 3
     append (loop for col from 0 to 3
	       collect (nth (+ row (* 4 col)) matrix))))


(defun cross-product (v1 v2)
  (list (- (* (nth 1 v1) (nth 2 v2))
	   (* (nth 2 v1) (nth 1 v2)))
	(- (* (nth 2 v1) (nth 0 v2))
	   (* (nth 0 v1) (nth 2 v2)))
	(- (* (nth 0 v1) (nth 1 v2))
	   (* (nth 1 v1) (nth 0 v2)))))


(defun normalize-vector (x y z)
  (let ((d (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (zerop d)
	'(0 0 0)
	(list (/ x d)
	      (/ y d)
	      (/ z d)))))

(defun get-length (&rest args)
  (sqrt (reduce #'+ (map 'list (lambda (x) (* x x))
			 (car args)))))

(defun make-identity-matrix ()
  '(1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))

(defun make-translation-matrix (x y z)
  (list 1 0 0 0 0 1 0 0 0 0 1 0 x y z 1))  

(defun make-rotation-matrix (angle x y z)
  (let* ((s (sin (deg->rad angle)))
	 (c (cos (deg->rad angle)))
	 (c1 (- 1 c)))

    (list (+ (* x x c1) c)
	  (+ (* x y c1) (* z s))
	  (- (* x z c1) (* y s))
	  0
		   
	  (- (* x y c1) (* z s))
	  (+ (* y y c1) c)
	  (+ (* y z c1) (* x s))
	  0
		   
	  (+ (* x z c1) (* y s))
	  (- (* y z c1) (* x s))
	  (+ (* z z c1) c)
	  0

	  0 0 0 1)))


(defun make-scaling-matrix (x y z)
  (list X 0 0 0 0 Y 0 0 0 0 Z 0 0 0 0 1))


(defun billboard-m (view-matrix m)
(list (nth 0 view-matrix)
      (nth 1 view-matrix)
      (nth 2 view-matrix)
      0
      (nth 4 view-matrix)
      (nth 5 view-matrix)
      (nth 6 view-matrix)
      0 
      (nth 8 view-matrix)
      (nth 9 view-matrix)
      (nth 10 view-matrix)
      0
      (nth 12 m)
      (nth 13 m)
      (nth 14 m)
      1))

(defun billboard-pos (view-matrix x y z)
(list (nth 0 view-matrix)
      (nth 1 view-matrix)
      (nth 2 view-matrix)
      0
      (nth 4 view-matrix)
      (nth 5 view-matrix)
      (nth 6 view-matrix)
      0 
      (nth 8 view-matrix)
      (nth 9 view-matrix)
      (nth 10 view-matrix)
      0
      x y z 1))

(defun make-random-matrix (&optional (seed 10))
  (loop for i from 0 to 15
     collect (random seed)))

(defun addmm (m n)
  (map 'list #'+ m n))

(defun addvv (m n)
  (map 'list #'+ m n))

(defun multvv (v1 v2)
  (let ((last1 (nth 3 v1))
	(last2 (nth 3 v2)))
    (list (* (nth 0 v1) (nth 0 v2))
	  (* (nth 1 v1) (nth 1 v2))
	  (* (nth 2 v1) (nth 2 v2))
	  (* (if last1 last1 0)
	     (if last2 last2 0)))))

(defun multmv (m v)
  (proclaim '(optimize (speed 3) (space 0) (debug 0)))
  (let ((m0 (nth 0 m))
	(m1 (nth 1 m))
	(m2 (nth 2 m))
	(m3 (nth 3 m))
	(m4 (nth 4 m))
	(m5 (nth 5 m))
	(m6 (nth 6 m))
	(m7 (nth 7 m))
	(m8 (nth 8 m))
	(m9 (nth 9 m))
	(m10 (nth 10 m))
	(m11 (nth 11 m))
	(m12 (nth 12 m))
	(m13 (nth 13 m))
	(m14 (nth 14 m))
	(m15 (nth 15 m))
	(v0 (nth 0 v))
	(v1 (nth 1 v))
	(v2 (nth 2 v))
	(v3 (if (nth 3 v)
		(nth 3 v) 1)))

    (list (+ (* v0 m0) (* v1 m4) (* v2 m8) (* v3 m12))
	  (+ (* v0 m1) (* v1 m5) (* v2 m9) (* v3 m13))
	  (+ (* v0 m2) (* v1 m6) (* v2 m10) (* v3 m14))
	  (+ (* v0 m3) (* v1 m7) (* v2 m11) (* v3 m15)))))
		   
	     
(defun multmm (m n)
  (proclaim '(optimize (speed 3) (space 0) (debug 0)))
  (let ((m0 (NTH 0 M))
	(m1 (NTH 1 M))
	(m2 (NTH 2 M))
	(m3 (NTH 3 M))
	(m4 (NTH 4 M)) 
	(m5 (NTH 5 M))
	(m6 (NTH 6 M))
	(m7 (NTH 7 M))
	(m8 (NTH 8 M)) 
	(m9 (NTH 9 M)) 
	(m10 (NTH 10 M)) 
	(m11 (NTH 11 M))
	(m12 (NTH 12 M)) 
	(m13 (NTH 13 M)) 
	(m14 (NTH 14 M)) 
	(m15 (NTH 15 M))
	(n0 (NTH 0 N)) 
	(n1 (NTH 1 N)) 
	(n2 (NTH 2 N)) 
	(n3 (NTH 3 N))
	(n4 (NTH 4 N)) 
	(n5 (NTH 5 N))
	(n6 (NTH 6 N)) 
	(n7 (NTH 7 N))
	(n8 (NTH 8 N)) 
	(n9 (NTH 9 N)) 
	(n10 (NTH 10 N)) 
	(n11 (NTH 11 N))
	(n12 (NTH 12 N)) 
	(n13 (NTH 13 N)) 
	(n14 (NTH 14 N)) 
	(n15 (NTH 15 N)))

    (list (+ (* m0 n0) (* m4 n1) (* m8 n2) (* m12 n3))
	  (+ (* m1 n0) (* m5 n1) (* m9 n2) (* m13 n3))
	  (+ (* m2 n0) (* m6 n1) (* m10 n2) (* m14 n3))
	  (+ (* m3 n0) (* m7 n1) (* m11 n2) (* m15 n3))

	  (+ (* m0 n4) (* m4 n5) (* m8 n6) (* m12 n7))
	  (+ (* m1 n4) (* m5 n5) (* m9 n6) (* m13 n7))
	  (+ (* m2 n4) (* m6 n5) (* m10 n6) (* m14 n7))
	  (+ (* m3 n4) (* m7 n5) (* m11 n6) (* m15 n7))

	  (+ (* m0 n8) (* m4 n9) (* m8 n10) (* m12 n11))
	  (+ (* m1 n8) (* m5 n9) (* m9 n10) (* m13 n11))
	  (+ (* m2 n8) (* m6 n9) (* m10 n10) (* m14 n11))
	  (+ (* m3 n8) (* m7 n9) (* m11 n10) (* m15 n11))

	  (+ (* m0 n12) (* m4 n13) (* m8 n14) (* m12 n15))
	  (+ (* m1 n12) (* m5 n13) (* m9 n14) (* m13 n15))
	  (+ (* m2 n12) (* m6 n13) (* m10 n14) (* m14 n15))
	  (+ (* m3 n12) (* m7 n13) (* m11 n14) (* m15 n15)))))

