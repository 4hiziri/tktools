(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defstruct rsa-key
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

(defun list2rsa-key (list)
  (cond ((= (length list) 9) ;; private-key
	 (make-rsa-key :version (nth 0 list)
		       :n (nth 1 list)
		       :e (nth 2 list)
		       :d (nth 3 list)
		       :p (nth 4 list)
		       :q (nth 5 list)
		       :e1 (nth 6 list)
		       :e2 (nth 7 list)
		       :c (nth 8 list)))
	((= (length list) 2) ;; public-key
	 (make-rsa-key :n (nth 0 list)
		       :e (nth 1 list)))))

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

(defun is-pem (pem)
  (let ((lines (split-sequence:split-sequence #\newline pem)))    
    (and (alexandria:starts-with-subseq "-----BEGIN" (car lines))
	 (alexandria:starts-with-subseq "-----END" (car (last lines))))))

;; TODO: if private, work. but public doesn't. need fix.
(defun extract-pem (pem)
  (if (is-pem pem)
      (let* ((lines (split-sequence:split-sequence #\newline pem))
	     (header (car lines))
	     (footer (last lines))
	     (pem-payload (subseq lines 1 (1- (length lines)))))
	(format nil "~{~A~}" pem-payload))))

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
  (let ((pem-base64 (extract-pem (string-trim '(#\newline) pem))))
    (if pem-base64
	(cl-base64:base64-string-to-usb8-array pem-base64))))

@export
(defun read-pem (filepath)
  (with-open-file (in filepath)
    (let ((der (coerce (parse-pem (read-string in)) 'list)))
      (list2rsa-key (car (parse-der der))))))
