(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

(defun continued-fraction (n)
  (loop for q = (floor n) then (truncate 1 r)
	for r = (- n q) then (- (/ 1 r) q)
	collect q
	until (= r 0)))

(defun get-continued-fraction (nums)
  (labels ((inner-func (q x1 x2)
	     (+ (* q x1) x2))
	   (inner-loop (rest n-prev n-next d-prev d-next)
	     (if rest
		 (inner-loop (cdr rest)
			     n-next
			     (inner-func (car rest) n-next n-prev)
			     d-next
			     (inner-func (car rest) d-next d-prev))
		 (/ n-next d-next))))
    (if nums
	(inner-loop nums 0 1 1 0)
	nums)))

(defun secret-key-p (n edg k)
  "If d is secret-key, return t"
  (if (/= k 0)
      (let* ((phi (truncate edg k))
	     (s (/ (1+ (- n phi)) 2)))
	(when (and (integerp s) (fast-perfect-square-p (- (expt s 2) n)))
	  t))
      nil))

(defun guess-f (fracs)
  (loop repeat (length fracs)
	for e from 1
	for i = (subseq fracs 0 e) then (subseq fracs 0 e)
	if (oddp e)
	  collect (let ((rev-i (reverse i)))
		    (get-continued-fraction (reverse (cons (1+ (car rev-i)) (cdr rev-i)))))
	else
	  collect (get-continued-fraction i)))

@export
(defun wiener-attack (n e)
  "This function attacks publick-key(n, e).
If private exponent d is small enouth, attack will be success."
  (dolist (i-kdg (guess-f (continued-fraction (/ e n))))
    (when (/= i-kdg 0)
      (let* ((dg (denominator i-kdg))
	     (k (numerator i-kdg))
	     (edg (* dg e))
	     (g (mod edg k)))
	(when (and (/= g 0) (secret-key-p n edg k))
	  (return (/ dg g)))))))
