(in-package :cl-user)
(defpackage hackrsa
  (:use :cl))
(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defun extend-gcd (a b)
  "(gcd(a,b), x, y) | ax + by = gcd(a, b)

gcd(a, b) = gcd(b, a mod b)
ax + by = gcd(a, b), x and y exists
gcd(b, a mod b) = bx' + (a mod b)y'
then
bx' + (a mod b)y' = gcd(a, b) ... (1)
a mod b = a - (truncate a b) * b ... (2)
then (2) into (1)
bx' + ay' - (truncate a b) * b y' = gcd(a, b)
ay' + b(x' - (truncate a b)y') = gcd(a, b)
compare ax + by = gcd(a, b),
x = y', y = x' - (truncato a b)y'
"
  (if (= b 0)
      (list a 1 0)
      (let* ((egcd (extend-gcd b (mod a b)))
	     (g (first egcd))
	     (x (second egcd))
	     (y (third egcd)))
	(list g y (- x (* y (truncate a b)))))))

@export
(defun mod-expt (base exp modulus)
  "more effective expotential and modulus.
calculate mod at every step of exp."
  (assert (integerp exp))
  (labels ((inner-mod-expt (base exp modulus)
	     (if (< exp 0) ;; if exp < 0, cannot calc mod so simply return base^exp
		 (expt base exp)
		 (loop for acc = 1 then (if (evenp e) acc (mod (* acc b) modulus))
		       for b = (mod base modulus) then (if (evenp e) (mod (expt b 2) modulus) b)
		       for e = exp then (if (evenp e) (/ e 2) (1- e))
		       when (= e 0)
			 do (return acc)))))
    (if (>= exp 0)
	(inner-mod-expt base exp modulus)
	(/ 1 (inner-mod-expt base (- exp) modulus)))))

@export
(defun mod-inv (a m)
  (let ((egcd (extend-gcd a m)))
    (mod (second egcd) m)))

@export
(defun decode-string (encoded-num)
  (labels ((inner-loop (num acc)
	     (if (> num 0)
		 (inner-loop (truncate num (expt 2 8)) (cons (mod num (expt 2 8)) acc))
		 acc)))
    (coerce (mapcar (lambda (x) (code-char x))
		    (inner-loop encoded-num nil))
	    'string)))

@export
(defun private-key (e p q)
  (mod-inv e (* (1- p) (1- q))))

@export
(defun decrypto (c d n)
  (mod-expt c d n))

;;; common-modulus-attack
;; two-cipher texts from tha same plain-text and two different e
@export
(defun common-modulus-attack (c1 c2 e1 e2 n)
  "If the same plain-texts are encrypted another e, we can attack by common-modulus-attack"
  (flet ((inner-solve (c1 c2 s1 s2)
	   (mod (* (mod-expt c1 s1 n)
		   (mod-expt c2 s2 n))
		n)))
    (let* ((s (extend-gcd e1 e2))
	   (s1 (second s))
	   (s2 (third s)))
      (cond ((< s1 0) (inner-solve (mod-inv c1 n) c2 (- s1) s2))
	    ((< s2 0) (inner-solve c1 (mod-inv c2 n) s1 (- s2)))
	    (t (inner-solve c1 c2 s1 s2))))))

#+sbcl
(defun sbcl-n-root (x n &optional (prec 1024))
  (require 'sb-mpfr)
  (sb-mpfr:coerce
   (sb-mpfr:with-precision prec
     (sb-mpfr:k-root (sb-mpfr:coerce x 'sb-mpfr:mpfr-float)
		     n))
   'double-float))

(defun n-root (x n)
  "Return nth root of x"
  #-sbcl
  (expt x (/ 1d0 n))
  #+sbcl  
  (sbcl-n-root x n))

;;; low public exponent attack
@export
(defun low-public-exponent-attack (c e &optional (limit 1000000))
  "If e is too small and m^e < n, this is success"
  (let ((estimate (round (n-root c e))))
    (loop for i from 0 to limit
	  if (= c (expt (- estimate i) e))
	    do (return (- estimate i))
	  else
	    if (= c (expt (+ estimate i) e))
	      do (return (+ estimate i)))))

;;; directly calculate p * q = n
;; fermat-rules
@export
(defun fermat-rules-attack (n)
  (loop for x = (1+ (isqrt n)) then (if (< w 0) (1+ x) x)
	for y = (isqrt (- (expt x 2) n)) then (if (> w 0) (1+ y) y)
	for w = (- (expt x 2) (expt y 2) n)
	when (= w 0)
	  do (return (cons (+ x y) (- x y)))))

;; Two n are shared prime is contained, we can calculate other prime
;; GCD
@export
(defun gcd-attack (n1 n2)
  (gcd n1 n2))

;;; low private exponent attack
;; wiener-attack
(defun continued-fraction (n)
  (loop for q = (floor n) then (truncate 1 r)
	for r = (- n q) then (- (/ 1 r) q)
	collect q
	until (= r 0)))

(defun const-continued-fraction (nums)
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

(defun guess-f (fracs)
  (loop repeat (length fracs)
	for e from 1
	for i = (subseq fracs 0 e) then (subseq fracs 0 e)
	if (oddp e)
	  collect (let ((rev-i (reverse i)))
		    (const-continued-fraction (reverse (cons (1+ (car rev-i)) (cdr rev-i)))))
	else
	  collect (const-continued-fraction i)))

(defun perfect-square-p (n)
  "if n's sqrt is integer, return t"
  (and (>= n 0)
       (= (expt (isqrt n) 2) n)))

(defparameter *table-sq-mod256* #(1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0
				  0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0
				  0 0 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0
				  1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0
				  0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0
				  0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1
				  0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0
				  0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0
				  0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0
				  0 0 0 0 0 0 1 0 0 0 0 0 0))

(defun fast-perfect-square-p (n)
  "Filtering n by mod 256, 5, 7, 9, 13 and 17, this works more fast."
  (let ((sq-mod256 *table-sq-mod256*)
	(mt (list (cons 9 #(1 1 0 0 1 0 0 1 0))
		  (cons 5 #(1 1 0 0 1))
		  (cons 7 #(1 1 1 0 1 0 0))
		  (cons 13 #(1 1 0 1 1 0 0 0 0 1 1 0 1))
		  (cons 17 #(1 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1))))
	(a (mod n (* 5 7 9 13 17))))
    (if (/= (aref sq-mod256 (logand n #xff)) 0)
	(dolist (mt-val mt (perfect-square-p n))
	  (when (= (aref (cdr mt-val) (mod a (car mt-val))) 0)
	    (return nil)))
	nil)))

(defun secret-key-p (n edg k)
  "If d is secret-key, return t"
  (if (/= k 0)
      (let* ((phi (truncate edg k))
	     (s (/ (1+ (- n phi)) 2)))
	(when (and (integerp s) (fast-perfect-square-p (- (expt s 2) n)))
	  t))
      nil))

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
