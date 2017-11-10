(in-package :cl-user)
(defpackage subst
  (:use :cl :cl-annot))
(in-package :subst)

(cl-annot:enable-annot-syntax)

@export
(defparameter *english-freq* '((#\e . 12.49)
			       (#\t . 9.28)
			       (#\a . 8.04)
			       (#\o . 7.64)
			       (#\i . 7.57)
			       (#\n . 7.23)
			       (#\s . 6.51)
			       (#\r . 6.28)
			       (#\h . 5.05)
			       (#\l . 4.07)
			       (#\d . 3.28)
			       (#\c . 3.34)
			       (#\u . 2.73)
			       (#\m . 2.51)
			       (#\f . 2.40)
			       (#\p . 2.14)
			       (#\g . 1.87)
			       (#\w . 1.68)
			       (#\y . 1.66)
			       (#\b . 1.48)
			       (#\v . 1.05)
			       (#\k . 0.54)
			       (#\x . 0.23)
			       (#\j . 0.16)
			       (#\q . 0.12)
			       (#\z . 0.09)))

@export
(defparameter *translation-table* nil)

@export
(defun str-char (s)
  "#\a -> #\a
\"a\" -> #\a"
  (if (stringp s)
      (char s 0)
      s))

@export
(defun add-table (cipher plain &optional (table *translation-table*))
  (let ((c (str-char cipher))
	(p (str-char plain)))
    (push (cons c p) table)))

@export
(defun get-table (c &optional (table *translation-table*))
  (cdr (assoc (str-char c) table)))

@export
(defun delete-table ()
  (setf *translation-table* nil))

@export
(defun decrypto (cipher &optional (table *translation-table*))
  (map 'string
       (lambda (c)
	 (let ((p (get-table c table)))
	   (if p (char-upcase p) c)))
       cipher))

@export
(defun count-char (cipher)
  "Only lowercase."
  (let ((cipher (string-downcase cipher)))
    (loop for char in (coerce "abcdefghijklmnopqrstuvwxyz" 'list)
	  collect (cons char (count char cipher)))))

@export
(defun analyze (cipher-text)
  (let ((cipher-freq (mapcar (lambda (x) (cons (car x)
					       (/ (cdr x) (length cipher-text))))
			     (sort (count-char cipher-text)
				   (lambda (x y) (> (cdr x) (cdr y)))))))
    (loop for c in cipher-freq
	  for p in *english-freq*
	  collect (cons (car c) (car p)))))
