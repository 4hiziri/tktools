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
  (flet ((next-val (x1 x2 q)
	   (- x1 (* x2 q))))
    (loop for q = (/ (- a (mod a b)) b) then (/ (- z1 (mod z1 z2)) z2)
	  for ztmp = (next-val a b q) then (next-val z1 z2 q)
	  for z1 = a then z2
	  for z2 = b then ztmp
	  for xtmp = (next-val 1 0 q) then (next-val x1 x2 q)
	  for x1 = 1 then x2
	  for x2 = 0 then xtmp
	  for ytmp = (next-val 0 1 q) then (next-val y1 y2 q)
	  for y1 = 0 then y2
	  for y2 = 1 then ytmp
	  when (= z2 1)
	    do (if (< x2 0)
		   (return (cons (+ x2 b) (- y2 a)))
		   (return (cons x2 y2))))))

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

(defun newton-sqrt (n)
  (flet ((func (x)
	   (truncate (+ x (truncate n x)) 2)))
    (loop for x1 = (expt 2 (truncate (1+ (integer-length n)) 2)) then x2
	  for x2 = (func x1) then (func x1)
	  when (<= x1 x2)
	    do (return x1))))

(defun fermat-rules-attack (n)
  (loop for x = (1+ (newton-sqrt n)) then (if (< w 0) (1+ x) x)
	for y = (newton-sqrt (- (expt x 2) n)) then (if (> w 0) (1+ y) y)
	for w = (- (expt x 2) (expt y 2) n) then (- (expt x 2) (expt y 2) n)
	when (= w 0)
	  do (return (cons (+ x y) (- x y)))))

(defun gcd-attack (n1 n2)
  (gcd n1 n2))

(defun cont-frac (n)
  (loop for q = (floor n) then (truncate 1 r)
	for r = (- n q) then (- (/ 1 r) q)
	collect q
	until (= r 0)))

(defun const-cont-frac (nums)
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
	nil)))

(defun potential-f (fracs)
  (let ((even t)
	(arg nil)
	(ret nil))
    (dolist (f fracs (reverse ret))
      (push f arg)
      (push (const-cont-frac
	     (reverse arg)
	     ;; WTF it does work?
	     ;; paper is wrong?
	     ;; :REFER https://www.cits.ruhr-uni-bochum.de/imperia/md/content/may/krypto2ss08/shortsecretexponents.pdf
	     ;; (if even
	     ;; 	 (reverse (cons (1+ f) (cdr arg)))
	     ;; 	 (reverse arg))
	     )
	    ret)      
      (setf ever (not even)))))

(defun perfect-square-p (n)
  (let ((sq-mod256  #(1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0))
	(mt (list (cons 9 #(1 1 0 0 1 0 0 1 0))
		  (cons 5 #(1 1 0 0 1))
		  (cons 7 #(1 1 1 0 1 0 0))
		  (cons 13 #(1 1 0 1 1 0 0 0 0 1 1 0 1))
		  (cons 17 #(1 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1))))
	(a (mod n (* 5 7 9 13 17))))
    (if (/= (aref sq-mod256 (logand n #xff)) 0)
	(dolist (mt-val mt (= (expt (newton-sqrt n) 2) n))
	  (when (= (aref (cdr mt-val) (mod a (car mt-val))) 0)
	    (return nil)))
	nil)))

(defun secret-key-p (n e k d)
  (if (and (/= k 0)
	   (= (mod (1- (* e d)) k) 0))
      (let* ((phi (truncate (1- (* e d)) k))
	     (s (1+ (- n phi)))
	     (discr (- (expt s 2) (* 4 n))))
	(when (and (>= discr 0) (perfect-square-p discr))
	  (print (list s n discr))
	  d))
      nil))

(defun wiener-attack (n e)
  (let ((kd (potential-f (cont-frac (/ e n)))))
    (dolist (kd-val kd)
      (when (secret-key-p n e (numerator kd-val) (denominator kd-val))
	(return (denominator kd-val))))))

(setf n #x9C2F6505899120906E5AFBD755C92FEC429FBA194466F06AAE484FA33CABA720205E94CE9BF5AA527224916D1852AE07915FBC6A3A52045857E0A1224C72A360C01C0CEF388F1693A746D5AFBF318C0ABF027661ACAB54E0290DFA21C3616A498210E2578121D7C23877429331D428D756B957EB41ECAB1EAAD87018C6EA3445)
(setf e #x466a169e8c14ac89f39b5b0357effc3e2139f9b19e28c1e299f18b54952a07a932ba5ca9f4b93b3eaa5a12c4856981ee1a31a5b47a0068ff081fa3c8c2c546feaa3619fd6ec7dd71c9a2e75f1301ec935f7a5b744a73df34d21c47592e149074a3ccef749ece475e3b6b0c8eecac7c55290ff148e9a29db8480cfe2a57801275)
