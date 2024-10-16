(require "asdf")

(load "aoc-cl.asd")

(asdf:load-system "aoc-cl/bin")

(aoc-cl:main)
