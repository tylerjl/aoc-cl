(asdf:defsystem "aoc-cl"
  :depends-on (:alexandria
	       :esrap
	       :iterate
	       :lparallel
	       :serapeum
	       :transducers
	       :trivia)
  :serial t
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:module "2023"
			      :serial t
			      :components ((:file "01")
					   (:file "02")))))))

(asdf:defsystem "aoc-cl/tests"
  :depends-on (:aoc-cl :fiveam)
  :serial t
  :components ((:module "tests"
		:serial t
		:components ((:file "packages")
			     (:file "util")
			     (:module "2023"
			      :serial t
			      :components ((:file "tests")))))))

(asdf:defsystem "aoc-cl/bin"
  :depends-on (:adopt :aoc-cl :trivial-benchmark)
  :components ((:module "src"
		:components ((:file "main")))))
