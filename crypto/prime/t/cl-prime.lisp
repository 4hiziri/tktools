(in-package :cl-user)
(defpackage cl-prime-test
  (:use :cl
        :cl-prime
        :prove))
(in-package :cl-prime-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-prime)' in your Lisp.

(plan nil)

(diag "Run Test")
(subtest "mod-expt"
  (let ((b 10)
	(e 11)
	(m 12))    
    (is (hackrsa::mod-expt b e m)
	(mod (expt b e) m)))
  (is (hackrsa::mod-expt 10 -2 10)
      1/100)
  (is (hackrsa::mod-expt -1 10 20)
      1)
  (is (hackrsa::mod-expt -1 9 20)
      19))

(subtest "coprime"
  (is (gcd (coprime 10) 10)      
      1)
  (is (gcd (coprime 101) 101)
      1))

(subtest "fermat-test"
  (ok (fermat-test 561))
  (ok (fermat-test 2))
  (ok (fermat-test 997))
  (isnt (fermat-test 144)
	t))

(finalize)
