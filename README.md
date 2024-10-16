# aoc-cl

Advent of Code solutions in Common Lisp.

## Quickstart

Run a solution:

	$ just run 2023 1 1 ./resources/2023/day1.txt
	
Run the tests:

	$ just test
	
Run a solution and measure the execution time:

	$ just run --time 2023 1 1 ./resources/2023/day1.txt
	
Run a solution and benchmark the execution time more accurately with 100 iterations:

	$ just run --benchmarks 100 2023 1 1 ./resources/2023/day1.txt

Build an executable rather than running in script mode:

	$ just build
	$ ./aoc-cl 2023 1 1 ./resources/2023/day1.txt

## Development

Mostly done within emacs: open a file and

	M-x sly

Steps I need to automate:

	;; Within sly's REPL
	(require "asdf")
	(load "aoc-cl.asd")
	(asdf:load-system "aoc-cl")
