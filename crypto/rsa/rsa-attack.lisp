;; more generalize
(defun binary-search (func trg bottom top &optional (error-range 100000))
  (let* ((mid (truncate (+ top bottom) 2))
	 (res (funcall func mid)))
    (cond ((<= (abs (- res trg)) error-range) mid)
	  ((< res trg) (binary-search func trg mid top error-range))
	  ((> res trg) (binary-search func trg bottom mid error-range))
	  (t (format t "error: ~A~%" mid)))))

(defun mod-expt (base exp modulus)
  "more effective expotential and modulus.
calculate mod at every step of exp."
  (if (< exp 0) ;; if exp < 0, cannot calc mod so simply return base^exp
      (expt base exp)
      (loop for acc = 1 then (if (evenp e) acc (mod (* acc b) modulus))
	    for b = (mod base modulus) then (if (evenp e) (mod (expt b 2) modulus) b)
	    for e = exp then (if (evenp e) (/ e 2) (1- e))
	    when (= e 0)
	      do (return acc))))

(defun extended-gcd (a b)
  "return (x . y) | ax + by = 1"
  (loop for q = (/ (- a (mod a b)) b) then (/ (- z1 (mod z1 z2)) z2)
	for ztmp = (- a (* q b)) then (- z1 (* q z2))
	for z1 = a then z2
	for z2 = b then ztmp
	for xtmp = (- 1 (* q 0)) then (- x1 (* q x2))
	for x1 = 1 then x2
	for x2 = 0 then xtmp
	for ytmp = (- 0 (* q 1)) then (- y1 (* q y2))
	for y1 = 0 then y2
	for y2 = 1 then ytmp
	when (= z2 1)
	  do (if (< x2 0)
		 (return (cons (+ x2 b) (- y2 a)))
		 (return (cons x2 y2)))))

(defun common-modulus-attack (c1 c2 e1 e2 n)
  "if same plain-text is encrypted another e, we can attack by common-modulus-attack"
  (let ((s (extended-gcd e1 e2)))
    (mod (* (expt c1 (car s))
	    (expt c2 (cdr s)))
	 n)))

;; how to calculate n-rt
(defun low-public-exponent-attack (c e &optional (limit 1000000))
  "if e is too small and m^e < n, this is success"
  (let ((estimate (round (expt c (/ 1d0 e)))))
    (loop for i from 0 to limit
	  if (= c (expt (- estimate i) e))
	    do (return (- estimate i))
	  else
	    if (= c (expt (+ estimate i) e))
	      do (return (+ estimate i)))))

;;
(defun search-approximate-sqrt (num)
  "return approximate integer.
inner algorithm is binary-search"
  (binary-search ))

(defun fermat-rules-attack)
