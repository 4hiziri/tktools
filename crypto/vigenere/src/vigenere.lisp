(in-package :cl-user)
(defpackage vigenere
  (:use :cl :cl-annot))
(in-package :vigenere)

(cl-annot:enable-annot-syntax)

@export
(defun subseq-by-len (seq len)
  "This return take subseq by `len' from `seq'.
   (subseq-by-len '(1 2 3 4 5) 2) => '((1 2) (2 3) (3 4) (4 5))"
  (let ((ret nil))    
    (dotimes (n (- (length seq) (1- len)) (reverse ret))
      (push (subseq seq n (+ n len))
	    ret))))

@export
(defun position-seq-list (seq1 seq2)
  "This return position of `seq1' in `seq2'.
   (position-seq-list '(1 2) '(1 2 3 4 1 2 1 2 3 4 1 4 5 1)) => (0 4 6)"
  (labels ((inner-search-pos (pos acc)
	     (if (< pos (length seq2))
		 (let ((next-pos (search seq1 seq2 :start2 pos)))
		   (if next-pos
		       (inner-search-pos (1+ (+ next-pos (length seq1))) (cons next-pos acc))
		       (reverse acc)))
		 (reverse acc))))
    (inner-search-pos 0 nil)))

@export
(defun length-between-seq (seq1 seq2)
  "This return length between parts that is equal to `seq1' in `seq2'.
   (length-between-seq '(1 2) '(0 1 2 3 4 1 2 1 2 3 4 1 4 5 1)) => (4 2)"
  (let ((pos-list (position-seq-list seq1 seq2)))
    (mapcar (lambda (x) (- (second x) (first x)))
	    (subseq-by-len pos-list 2))))

@export
(defun group-seq (list)
  "This return tuple which has item and num of item in sequence. => (num-of-item item)

(1 1 1 2 2 3) => ((3 . 1) (2 . 2) (1 . 3))"
  (let ((uniq (remove-duplicates list :test #'equal)))
    (mapcar (lambda (x) (cons (count x list :test #'equal) x))
	    uniq)))

@export
(defun pattern-by-length (sequence pattern-length)
  "(pattern-freq '(1 2 3 1 2 3 1 2 3) 2) => ((3 1) (1 2) (2 3))"
  (remove-duplicates (subseq-by-len sequence pattern-length) :test #'equal))

@export
(defun possible-position (sequence pattern-length)
  (let ((possible-span-list nil))
    (dolist (pat (pattern-by-length sequence pattern-length) possible-span-list)
      (let ((pos (position-seq-list pat sequence)))
	(when (/= (length pos) 1)
	  (push pos possible-span-list))))))

@export
(defun estimate-key-length (sequence)
  (let ((max-pat-len (truncate (length sequence) 2)))
    (loop for i from max-pat-len downto 2
	  for pos = (possible-position sequence i)
	  when (funcall (complement #'null) pos)
	    collect (list (cons :pat-len i)
			  (cons :pos (possible-position sequence i))))))

@export
(defun analyze-vigenere (sequence &key (key-len-min 4))
  (loop for i from (truncate (length sequence) 2) downto key-len-min
	when (some (lambda (x) (/= (car x) 1))
		   (group-seq (subseq-by-len sequence i)))
	  collect (remove-if (lambda (x) (= (car x) 1))
			     (group-seq (subseq-by-len sequence i)))))
