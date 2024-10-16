;;;; 02.lisp

(in-package :2023)

(defstruct game id rounds)

(deftype color () '(member :red :green :blue))

(esrap:defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (esrap:text list) :radix 10)))

(esrap:defrule color (or "red" "green" "blue")
  (:lambda (c) (case c ("red" :red) ("green" :green) ("blue" :blue))))

(esrap:defrule draw (and " " integer " " color (esrap:? ","))
  (:destructure (w1 n w2 c w3) (declare (ignore w1 w2 w3)) (cons c n)))

(esrap:defrule round (and (+ draw) (esrap:? ";"))
  (:destructure (draws w) (declare (ignore w)) draws
    (maphash-into (make-hash-table)
                  (lambda (draw)
                    (match draw ((cons c n) (values c n))))
                  draws)))

(esrap:defrule game (and "Game " integer ":" (+ round))
  (:destructure (w1 n w2 rounds)
    (declare (ignore w1 w2))
    (make-game :id n :rounds rounds)))

(defun parse (line) (esrap:parse 'game line))

(defparameter bags (dict :red 12 :green 13 :blue 14))

(defun valid-round? (round)
  (iter (for (k v) in-hashtable round) (always (<= v (@ bags k)))))

(defun valid-game? (game)
  (iter (for round in (game-rounds game)) (always (valid-round? round))))

(defun max-color (colors counts)
  (match counts
    ((cons color count)
     (let ((existing (@ colors color)))
       (if (and existing (> existing count))
           colors
           (progn (setf (@ colors color) count) colors))))))

(defun max-colors (so-far round)
  (iter
    (for (color count) in-hashtable round)
    (reducing (cons color count)
              by #'max-color
              initial-value so-far)))

(defun game->power (game)
  (apply #'* (a:hash-table-values
              (iter
                (for round in (game-rounds game))
                (reducing
                 round
                 by #'max-colors
                 initial-value (dict :green 0 :blue 0 :red 0))))))

(defun 2-1 (input)
  (transduce
   (t:comp (t:map #'parse)
           (t:filter #'valid-game?)
           (t:map #'game-id))
    #'+
    (pathname input)))

(defun 2-2 (input)
  (transduce
   (t:comp (t:map #'parse)
           (t:map #'game->power))
   #'+
   (pathname input)))

(export '2-1)
(export '2-2)
