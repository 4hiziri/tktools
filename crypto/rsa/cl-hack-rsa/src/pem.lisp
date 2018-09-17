(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

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

@export
(defun parse-pem (pem)
  (let ((pem-base64 (validate-pem pem)))    
    (if pem-base64
	(cl-base64:base64-string-to-usb8-array pem-base64))))

@export
(defun read-pem (filepath)
  (with-open-file (in filepath)   
    (parse-pem (read-string in))))
