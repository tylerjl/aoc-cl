(require "asdf")

(load "aoc-cl.asd")

(asdf:load-system "aoc-cl/tests")

(in-package :aoc-cl/tests)

(uiop:quit (if (run-all-tests) 0 1))
