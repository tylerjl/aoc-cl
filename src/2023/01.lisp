;;;; 01.lisp

(in-package :2023)

(esrap:defrule skip-char character (:constant nil))

(esrap:defrule digit (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  (:lambda (digit)
    (parse-integer digit)))

(esrap:defrule one/p   (and (esrap:& "one")   skip-char) (:constant 1))
(esrap:defrule two/p   (and (esrap:& "two")   skip-char) (:constant 2))
(esrap:defrule three/p (and (esrap:& "three") skip-char) (:constant 3))
(esrap:defrule four/p  (and (esrap:& "four")  skip-char) (:constant 4))
(esrap:defrule five/p  (and (esrap:& "five")  skip-char) (:constant 5))
(esrap:defrule six/p   (and (esrap:& "six")   skip-char) (:constant 6))
(esrap:defrule seven/p (and (esrap:& "seven") skip-char) (:constant 7))
(esrap:defrule eight/p (and (esrap:& "eight") skip-char) (:constant 8))
(esrap:defrule nine/p  (and (esrap:& "nine")  skip-char) (:constant 9))

(esrap:defrule phonetic (or one/p two/p three/p four/p five/p six/p seven/p
			    eight/p nine/p))

(esrap:defrule total (+ (or phonetic digit skip-char)))

(defun char->ints (l c)
  (cond ((char= #\1 c) (cons 1 l))
	((char= #\2 c) (cons 2 l))
	((char= #\3 c) (cons 3 l))
	((char= #\4 c) (cons 4 l))
	((char= #\5 c) (cons 5 l))
	((char= #\6 c) (cons 6 l))
	((char= #\7 c) (cons 7 l))
	((char= #\8 c) (cons 8 l))
	((char= #\9 c) (cons 9 l))
	(t l)))

(defun string->ints (str)
  (reduce #'char->ints str :initial-value '()))

(defun line->num (f line)
  (let ((nums (funcall f line)))
    (+ (* 10 (first nums)) (first (last nums)))))

(defun lines->num (f lines)
  (reduce #'+
	  (map 'list (lambda (l) (line->num f l)) lines)
	  :initial-value 0))

(defun parse/line->num (parser line)
  (remove nil (esrap:parse parser line)))

(defun solve (parser fp)
  (lines->num (lambda (l) (parse/line->num parser l))
	      (uiop:read-file-lines fp)))

(defun 1-1 (fp) (solve '(+ (or digit skip-char)) fp))

(defun 1-2 (fp) (solve 'total fp))

(export '1-1)
(export '1-2)
