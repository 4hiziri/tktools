(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defstruct rsa-key
  version
  n
  e
  d
  p
  q
  e1 ;; exponent1: d mod (p-1)
  e2 ;; exponent2: d mod (q-1)
  c ;; coefficient: q^-1 mod p
  )

(defun list2rsa-key (list)
  (make-rsa-key :version (nth 0 list)
		:n (nth 1 list)
		:e (nth 2 list)
		:d (nth 3 list)
		:p (nth 4 list)
		:q (nth 5 list)
		:e1 (nth 6 list)
		:e2 (nth 7 list)
		:c (nth 8 list)))

;; TODO: to util
(defun read-string (stream)
  (do ((ch (read-char stream nil nil) (read-char stream nil nil))
       (ch-list nil (cons ch ch-list)))
      ((null ch)
       (coerce (reverse ch-list) 'string))))

(defun trim-left-subseq (prefix sequence)  
  (if (alexandria:starts-with-subseq prefix sequence)
      (subseq sequence (length prefix))
      sequence))

(defun trim-right-subseq (suffix sequence)  
  (if (alexandria:ends-with-subseq suffix sequence)
      (subseq sequence 0 (- (length sequence) (length suffix)))
      sequence))

(defun validate-pem (pem) 
  (let ((header "-----BEGIN RSA PRIVATE KEY-----")
	(footer "-----END RSA PRIVATE KEY-----")
	(pem (string-trim '(#\newline) pem)))
    (if (and (alexandria:starts-with-subseq header pem)
	     (alexandria:ends-with-subseq footer pem))
	(string-trim '(#\newline)
		     (trim-right-subseq footer
					(trim-left-subseq header pem)))
	nil)))

;; TODO: need ASP.1 reader
(defun is-asp1-sequence (type)
  (= type 0x30))

(defun is-short-form (length)
  (<= length #x7f))

(defun bytes2integer (bytes)
  "Byte-array to integer, big-endian"
  (reduce (lambda (acc x) (+ (ash acc 8) x))
	  bytes))

(defun get-length-field-len (length)
  "Return length-field length by definite."
  (if (is-short-form length)
      0
      (logand #x7f length)))

(defun get-length (bytes)
  "Receive bytes as type-length-value. Return length as long-definite."
  (let* ((length (nth 1 bytes))
	 (length-len (get-length-field-len length)))
    (cond
      ((is-short-form length) length)
      ((< length-len 127) (bytes2integer (subseq (cddr bytes) 0 length-len)))
      ;; TODO: not impl now, more length field exists
      (t nil))))

(defun get-type (bytes)
  "Receive bytes as type-length-value, and return type as lisp type (if possible)."
  ;; TODO: make more better impl later
  (car bytes))

(defun type-coerce (type value)
  "Coerce value depends on value.
Only INTEGER: 0x02, SEQUENCE: 0x30"
  (let ((integer #x02)
	(sequence #x30))
    (cond
      ((= type integer) (bytes2integer value))
      ((= type sequence) (parse-der value)) ;; recur?
      ;; if not impl, return nil
      (t nil))))

(defun get-value (bytes)
  "Receive bytes as type-length-value, return (value, rest) depends on length.
Only short-definite and long-definite, not infinite."
  (let ((type-length-len (+ 2 (get-length-field-len (nth 1 bytes))))
	(length (get-length bytes)))
    (values (type-coerce (get-type bytes)
			 (subseq bytes type-length-len (+ type-length-len length)))
	    (subseq bytes (+ type-length-len length)))))

(defun parse-der (bytes)
  "WARN: only RSA private key now"
  (labels ((inner-parse-field (acc bytes)
	     (multiple-value-bind (val rest) (get-value bytes)
	       (if rest
		   (inner-parse-field (cons val acc) rest)
		   (reverse (cons val acc))))))
    (inner-parse-field nil bytes)))

@export
(defun parse-pem (pem)
  (let ((pem-base64 (validate-pem pem)))    
    (if pem-base64
	(cl-base64:base64-string-to-usb8-array pem-base64))))

@export
(defun read-pem (filepath)
  (with-open-file (in filepath)
    (let ((der (coerce (parse-pem (read-string in)) 'list)))
      (list2rsa-key (car (parse-der der))))))
