(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

(defparameter *asp1-type* (make-hash-table))
(setf (gethash #x02 *asp1-type*) 'integer)
(setf (gethash #x03 *asp1-type*) 'bit-string)
(setf (gethash #x05 *asp1-type*) 'null)
(setf (gethash #x06 *asp1-type*) 'object-id)
(setf (gethash #x30 *asp1-type*) 'sequence)

@export
(defstruct asp1-object
  type
  value)

;; TODO: need ASP.1 reader
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

;; TODO: when impl all asp.1 types, return type symbol
(defun get-type (bytes)
  "Receive bytes as type-length-value, and return type as lisp type (if possible).
Not return type symbol, just num." 
  (car bytes))

(defun type-coerce (type value)
  "Coerce value depends on value.
Only INTEGER: 0x02, SEQUENCE: 0x30"
  (flet ((ret (x) (values x t)))
    (cond
      ((eq type 'integer) (ret (bytes2integer value)))
      ((eq type 'bit-string) (ret value)) ;; not impl but return
      ((eq type 'null) (ret nil))
      ((eq type 'object-id) (ret value)) ;; not impl but return
      ((eq type 'sequence) (ret (parse-der value))) ;; recur
      (t (values nil nil))))) ;; if not implemented, returned second value become nil

(defun get-value (bytes)
  "Receive bytes as type-length-value, return (value, rest) depends on length.
Only short-definite and long-definite, not infinite."  
  (let ((type-length-len (+ 2 (get-length-field-len (nth 1 bytes))))
	(length (get-length bytes)))       
    (values (make-asp1-object
	     :type (gethash (get-type bytes) *asp1-type*)
	     :value (type-coerce (gethash (get-type bytes) *asp1-type*)
				 (subseq bytes type-length-len
					 (+ type-length-len length))))
	    (subseq bytes (+ type-length-len length)))))

(defun parse-asp1 (bytes)
  (labels ((inner-parse-field (acc bytes)
	     (multiple-value-bind (val rest) (get-value bytes)
	       (print val)
	       (if rest
		   (inner-parse-field (cons val acc) rest)
		   (reverse (cons val acc))))))
    (inner-parse-field nil bytes)))
