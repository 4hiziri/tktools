(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defstruct rsa-key
  "RSA key pem fields"
  (version nil)
  (n nil)
  (e nil)
  (d nil)
  (p nil)
  (q nil)
  (e1 nil) ;; exponent1: d mod (p-1)
  (e2 nil) ;; exponent2: d mod (q-1)
  (c nil) ;; coefficient: q^-1 mod p
  )

(defun asp1-rsa-private (asp1-obj)
  "convert ASP.1 object of RSA private key pem to RSA key"
  (let ((vals (asp1-object-value asp1-obj)))
    (make-rsa-key :version (asp1-object-value (nth 0 vals))
		  :n (asp1-object-value (nth 1 vals))
		  :e (asp1-object-value (nth 2 vals))
		  :d (asp1-object-value (nth 3 vals))
		  :p (asp1-object-value (nth 4 vals))
		  :q (asp1-object-value (nth 5 vals))
		  :e1 (asp1-object-value (nth 6 vals))
		  :e2 (asp1-object-value (nth 7 vals))
		  :c (asp1-object-value (nth 8 vals)))))

(defun asp1-rsa-public (asp1-obj)
  "Make RSA-KEY from asp1-obj from public-key's pem."
  ;; RSA public key pem is represented by (object-id bit-string)
  ;; object-id is just id, bit-string is real public-key value
  ;; but first byte is pading-bit-length, so just do cdr
  ;; rest part is DER format public key data
  (let ((pubkey-data (asp1-object-value
		      (car (parse-der
			    (cdr (asp1-object-value
				  (nth 1 (asp1-object-value asp1-obj)))))))))
    ;; public-key pem only contains N and e.
    (make-rsa-key :n (asp1-object-value (nth 0 pubkey-data))
		  :e (asp1-object-value (nth 1 pubkey-data)))))

;; TODO: to util
(defun read-string (stream)
  "Return all content in stream as String"
  (do ((ch (read-char stream nil nil) (read-char stream nil nil))
       (ch-list nil (cons ch ch-list)))
      ((null ch)
       (coerce (reverse ch-list) 'string))))

(defun trim-left-subseq (prefix sequence)
  "Trim `prefix` from left"
  (if (alexandria:starts-with-subseq prefix sequence)
      (subseq sequence (length prefix))
      sequence))

(defun trim-right-subseq (suffix sequence)
  "Trim `suffix' from right.
;; (trim-right-subseq \"abc\" \"xyzabc\") ;; => xyz"
  (if (alexandria:ends-with-subseq suffix sequence)
      (subseq sequence 0 (- (length sequence) (length suffix)))
      sequence))

(defun pem-p (pem)
  "predicate func for pem, depending on header and footer"
  (let ((lines (split-sequence:split-sequence #\newline pem)))
    (and (alexandria:starts-with-subseq "-----BEGIN" (car lines))
	 (alexandria:starts-with-subseq "-----END" (car (last lines))))))

(defun pem-type-p (pem type)
  "Predicate func for OpenSSL PEM, depending on header and footer"
  (let ((header (car (split-sequence:split-sequence #\newline pem)))
	(type-header (cond ((eq type 'rsa-private)
			    "-----BEGIN RSA PRIVATE KEY-----")
			   ((eq type 'public)
			    "-----BEGIN PUBLIC KEY-----"))))
    (string= header type-header)))

(defun rsa-private-pem-p (pem)
  "Predicate rsa private key pem"
  (pem-type-p pem 'rsa-private))

(defun rsa-public-pem-p (pem)
  "Predicate rsa public key pem"
  (pem-type-p pem 'public))

(defun extract-pem (pem)
  "Extract PEM content"
  (if (pem-p pem)
      (let* ((lines (split-sequence:split-sequence #\newline pem))
	     (header (car lines))
	     (footer (last lines))
	     (pem-payload (subseq lines 1 (1- (length lines)))))
	(format nil "~{~A~}" pem-payload))))

(defun parse-der (bytes)
  "Parse DER from byte sequence to ASP.1 Object"
  (labels ((inner-parse-field (acc bytes)
	     (multiple-value-bind (val rest) (get-value bytes)
	       (if rest
		   (inner-parse-field (cons val acc) rest)
		   (reverse (cons val acc))))))
    (inner-parse-field nil bytes)))

@export
(defun parse-pem (pem)
  "Parse PEM to RSA key"
  (let ((asp1-obj (parse-der (coerce (cl-base64:base64-string-to-usb8-array
				      (extract-pem (string-trim '(#\newline) pem)))
				     'list))))
    (cond ((rsa-private-pem-p pem) (asp1-rsa-private (car asp1-obj)))
	  ((rsa-public-pem-p pem) (asp1-rsa-public (car asp1-obj)))
	  (t asp1-obj))))

@export
(defun read-pem (filepath)
  "Read PEM file and parse it"
  (with-open-file (in filepath)
    (parse-pem (read-string in))))
