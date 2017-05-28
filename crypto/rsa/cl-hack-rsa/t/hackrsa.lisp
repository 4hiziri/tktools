(in-package :cl-user)
(defpackage hackrsa-test
  (:use :cl :hackrsa :prove))
(in-package :hackrsa-test)

;; NOTE: To run this test file, execute `(asdf:test-system :hackrsa)' in your Lisp.

(plan nil)
(setf prove:*enable-colors* t)
(diag "run test")

;; wiener-attack
(diag "Test: winer-attack")
(subtest "continued-fraction"
  (is (hackrsa::continued-fraction 4/11)
      '(0 2 1 3))
  (is (hackrsa::continued-fraction 4)
      '(4)))

(subtest "const-continued-fraction"
  (is (hackrsa::const-continued-fraction '(4))
      4)
  (is (hackrsa::const-continued-fraction (hackrsa::continued-fraction 4/11))
      4/11)
  (is (hackrsa::const-continued-fraction nil)
      nil)
  (loop repeat 10 ;; check by randomness
	with range = 10000 ;; meaningless this value, only need some integers
	for d = (random range) then (random range)
	for n = (random range) then (random range) do
	  (is (hackrsa::const-continued-fraction (hackrsa::continued-fraction (/ d n)))
	      (/ d n))))

(subtest "guess-f"
  (let ((list (list 0 2 1 3)))    
    (is (hackrsa::guess-f list)
	'(1 1/2 2/5 4/11))))

(subtest "perfect-square-p"
  (ok (hackrsa::perfect-square-p 4))
  (ok (not (hackrsa::perfect-square-p 5)))
  (loop repeat 10
	with range = 1000000
	for i = (random range) then (random range)
	do (progn (diag (format nil "Test: ~A" (expt i 2)))
		  (ok (hackrsa::perfect-square-p (expt i 2))))))

(subtest "fast-perfect-square-p"
  (loop repeat 10
	with range = 1000000
	for i = (random range) then (random range)
	do (progn (diag (format nil "Test: ~A" i))
		  (is (hackrsa::perfect-square-p i)
		      (hackrsa::fast-perfect-square-p i)))))

;; TODO implement rsa parameter
;; (subtest "secret-key-p"
;;   (ok ))

(subtest "wiener-attack"
  (let ((e #x466a169e8c14ac89f39b5b0357effc3e2139f9b19e28c1e299f18b54952a07a932ba5ca9f4b93b3eaa5a12c4856981ee1a31a5b47a0068ff081fa3c8c2c546feaa3619fd6ec7dd71c9a2e75f1301ec935f7a5b744a73df34d21c47592e149074a3ccef749ece475e3b6b0c8eecac7c55290ff148e9a29db8480cfe2a57801275)
	(n #x9C2F6505899120906E5AFBD755C92FEC429FBA194466F06AAE484FA33CABA720205E94CE9BF5AA527224916D1852AE07915FBC6A3A52045857E0A1224C72A360C01C0CEF388F1693A746D5AFBF318C0ABF027661ACAB54E0290DFA21C3616A498210E2578121D7C23877429331D428D756B957EB41ECAB1EAAD87018C6EA3445)
	(d #x30273E11CBE5AE0CF9054376C76452F5EF9642C4A0D485FBE6AE6E808FF0E011))    
    (is (wiener-attack n e)
	d)))

(finalize)
