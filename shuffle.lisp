(defpackage :shuffle
  (:use :common-lisp)
  (:export :shuffle
	   :shuffle!))
(in-package :shuffle)

(defun array-shuffle! (arr)
  "Destructively shuffle an arr in-place by an algorithm of Fisher & Yates."
  (do ((i (length arr) (1- i)))
      ((= i 2) arr)
    (rotatef (aref arr (random i))
	     (aref arr (1- i)))))

(defun list-shuffle! (lst)
  "Destructively shuffle the list in-place copying from a shuffled vector copy"
  (let ((array (array-shuffle! (coerce lst 'vector))))
    (declare (dynamic-extent array))
    (map-into lst 'identity array)))

(defun list-shuffle (lst)
  "Return a shuffled copy of the given list LST."
  (let ((cp (copy-list lst)))
    (list-shuffle! cp)))

(defun shuffle! (seq)
  "Destructively shuffle a seq in-place."
  (etypecase seq
    (list (list-shuffle! seq))
    (array (array-shuffle! seq))))

(defun shuffle (seq)
  "Return a shuffled copy of the given sequence SEQ."
  (etypecase seq
    (list (list-shuffle seq))
    (array (let ((cp (coerce seq 'list)))
	     (array-shuffle! (coerce cp 'vector))))))
