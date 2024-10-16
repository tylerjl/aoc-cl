(in-package :aoc-cl/tests)

(defmacro aoc-test (year part expect)
  `(test ,part
	 (is (= ,(funcall (symbol-function
			   (find-symbol (format nil "~a" part) year))
			  (format nil "tests/~a/~a.txt" year part))
		,expect))))
