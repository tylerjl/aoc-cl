@run +args:
	sbcl --script run.lisp {{args}}

build:
	sbcl --script run.lisp build

test:
	sbcl --script test.lisp
