;;;; SHUFFLE.LISP - Fisher-Yates sequence shuffling algorithm
;;;; Copyright 2017 Darren W. Ringer <dwringer@gmail.com>

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
(defpackage :shuffle
  (:use :common-lisp)
  (:export :shuffle
	   :shuffle!))
(in-package :shuffle)

(defun array-shuffle! (arr)
  "Destructively shuffle array ARR in-place by an algorithm of Fisher & Yates."
  (do ((i (length arr) (1- i)))
      ((= i 2) arr)
    (rotatef (aref arr (random i))
	     (aref arr (1- i)))))

(defun list-shuffle! (lst)
  "Destructively shuffle list LST in-place copying from a shuffled vector copy"
  (let ((array (array-shuffle! (coerce lst 'vector))))
    (declare (dynamic-extent array))
    (map-into lst 'identity array)))

(defun list-shuffle (lst)
  "Return a shuffled copy of the given list LST."
  (let ((cp (copy-list lst)))
    (list-shuffle! cp)))

(defun shuffle! (seq)
  "Destructively shuffle the given sequence SEQ in-place."
  (etypecase seq
    (list (list-shuffle! seq))
    (array (array-shuffle! seq))))

(defun shuffle (seq)
  "Return a shuffled copy of the given sequence SEQ."
  (etypecase seq
    (list (list-shuffle seq))
    (array (let ((cp (coerce seq 'list)))
	     (array-shuffle! (coerce cp 'vector))))))
