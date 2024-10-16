;;;; packages.lisp

(defpackage :2023
  (:use :cl :iterate)
  (:import-from :transducers #:transduce)
  (:local-nicknames (#:t #:transducers)
		    (#:a #:alexandria))
  (:import-from :trivia #:match)
  (:import-from :serapeum #:@ #:dict #:maphash-into))

(defpackage :aoc-cl
  (:use :cl)
  (:import-from :trivia #:match)
  (:export :main))
