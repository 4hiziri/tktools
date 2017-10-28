(in-package :cl-user)
(defpackage vigenere
  (:use :cl))
(in-package :vigenere)

(defun subseq-by-len (seq len)
  "This return take subseq by `len' from `seq'.
   (subseq-by-len '(1 2 3 4 5) 2) => '((1 2) (2 3) (3 4) (4 5))"
  (let ((ret nil))    
    (dotimes (n (- (length seq) (1- len)) (reverse ret))
      (push (subseq seq n (+ n len))
	    ret))))

(defun position-seq-list (seq1 seq2)
  "This return position of `seq1' in `seq2'.
   (position-seq-list '(1 2) '(1 2 3 4 1 2 1 2 3 4 1 4 5 1)) => (0 4 6)"
  (labels ((inner-search-pos (pos acc)
	     (let ((next-pos (search seq1 seq2 :start2 pos)))
	       (if next-pos
		   (inner-search-pos (1+ next-pos) (cons next-pos acc))
		   (reverse acc)))))
    (inner-search-pos 0 nil)))

(defun length-between-seq (seq1 seq2)
  "This return length between parts that is equal to `seq1' in `seq2'.

   (length-between-seq '(1 2) '(0 1 2 3 4 1 2 1 2 3 4 1 4 5 1)) => (4 2)"
  (let ((pos-list (position-seq-list seq1 seq2)))
    (mapcar (lambda (x) (- (second x) (first x)))
	    (subseq-by-len pos-list 2))))

(defun group-seq (list)
  "This return tuple which has item and num of item in sequence. => (num-of-item item)"
  (let ((uniq (remove-duplicates list :test #'equal)))
    (mapcar (lambda (x) (cons (count x list :test #'equal) x))
	    uniq)))

(defun estimate-key-length (sequence)
  )

(defun analyze-vigenere (sequence &key (key-len-min 4))
  (loop for i from (truncate (length sequence) 2) downto key-len-min
	when (some (lambda (x) (/= (car x) 1))
		   (group-seq (subseq-by-len sequence i)))
	  collect (remove-if (lambda (x) (= (car x) 1))
			     (group-seq (subseq-by-len sequence i)))))
