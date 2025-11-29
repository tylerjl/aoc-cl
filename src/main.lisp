(in-package :aoc-cl)

(import :2023)
(import :trivial-benchmark)

(defparameter *option-version*
  (adopt:make-option
   'version
   :long "version"
   :help "Display version and exit."
   :reduce (constantly t)))

(defparameter *option-help*
  (adopt:make-option
   'help
   :long "help"
   :short #\h
   :help "Display help and exit."
   :reduce (constantly t)))

(defparameter *option-timing*
  (adopt:make-option
   'timing
   :long "time"
   :short #\t
   :help "Include time taken to execute solution"
   :reduce (constantly t)))

(defparameter *option-benchmark*
  (adopt:make-option
   'benchmark
   :long "benchmarks"
   :parameter "ITERATIONS"
   :short #\b
   :help "Benchmark the solution with the given number of times"
   :initial-value 0
   :key #'parse-integer
   :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
   :name "aoc-cl"
   :summary "Advent of Code solutions in Common Lisp"
   :usage "[options] <year> <day> <part> [input]"
   :help "Run solution code for the given problem"
   :contents (list *option-version*
                   *option-help*
                   *option-timing*
                   *option-benchmark*)))

(defun build ()
  (sb-ext:save-lisp-and-die
   "aoc-cl"
   :executable t
   :toplevel #'aoc-cl:main))

(defmacro timed (enabled thunk)
  `(if ,enabled (time ,thunk) ,thunk))

(defun run (time-it benchmarks args)
  (match args
    ((list "build") (build))
    ((list year day part input)
     (let ((fn-sym (find-symbol (format nil "~a-~a" day part) year)))
       (if fn-sym
           (progn
             ;; Bootstrap our potential threads for lparallel use
             (setf lparallel:*kernel* (lparallel:make-kernel 16))
             (let ((fn (symbol-function fn-sym)))
             (if (> benchmarks 0)
                 (progn
                   (write-line
                    (format nil "Benchmarking ~a:~a:" year fn-sym))
                   (trivial-benchmark:with-timing
                       (benchmarks) (funcall fn input)))
                 (write-line
                  (format nil "~a" (timed time-it (funcall fn input)))))))
           (write-line (format nil "Unimplemented: year ~a day ~a part ~a"
                               year day part)))))
    ((list* _) (write-line "Unknown command."))))

(defun main ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'version options)
          (format t "1.0.0~%")
          (adopt:exit))
        (run (gethash 'timing options)
             (gethash 'benchmark options)
             arguments))
    (error (c)
      (adopt:print-error-and-exit c))))
