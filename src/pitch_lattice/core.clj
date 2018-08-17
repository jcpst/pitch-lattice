(ns pitch-lattice.core
  (:require [clojure.pprint :as p]))

(defrecord Pitch [cent ratio])
(defrecord Set [pitches transposition limit])

(defn range-inc
  "Make range upper bound inclusive"
  [num]
  (range 1 (+ num 1)))

(defn factors
  "Get the factors of a number"
  [num]
  (filter #(zero? (rem num %)) (range-inc num)))

(defn prime?
  "Determine if a number is prime"
  [num]
  (.isProbablePrime (BigInteger/valueOf num) 5))

(defn ratio-to-list
  "Converts a ratio to a list"
  [num]
  (if (ratio? num)
    (list (numerator num) (denominator num))
    (list num)))

(defn ratio-to-cents
  "Converts a ratio to cents"
  [ratio]
  (* (Math/log10 ratio) (/ 1200 (Math/log10 2))))

(defn recip
  "Returns the reciprocal of a ratio"
  [num]
  (if (ratio? num)
    (/ (denominator num) (numerator num))
    (/ 1 num)))

(defn flatten-ratio
  "Brings the size of a ratio in between 1 and 2"
  [ratio]
  (cond
    (> ratio 2) (recur (/ ratio 2))
    (< ratio 1) (recur (* ratio 2))
    :else ratio))

(defn flip-ratio
  "Flips the numerator and denominator in a ratio"
  [ratio]
  (flatten-ratio (recip ratio)))

(defn limit
  "Calculates the partial limit of a ratio"
  [ratio]
  (apply max (filter prime? (mapcat factors (ratio-to-list ratio)))))

(defn sum-ratios
  "Add an arbitrary number of ratios"
  [& ratios]
  (flatten-ratio (apply * ratios)))

(defn diff-ratios
  "Get the difference between two ratios"
  [x y]
  (flatten-ratio (/ x y)))

(defn step
  "Interval size on the lattice based on direction and limit"
  [direction limit]
  (flatten-ratio
    (condp = direction
      :otonal limit
      :utonal (recip limit))))

(defn make-pitch
  "Constructs a Pitch record"
  [ratio]
  (->Pitch (ratio-to-cents ratio) ratio))

(defn lattice-walk
  "Creates a list of steps in one direction on the lattice"
  [step num]
  (map #(make-pitch (apply sum-ratios (replicate % step))) (range-inc num)))

(defn make-set
  ""
  [transposition limit steps]
  (->Set (lattice-walk (step transposition limit) steps) transposition limit))

;TODO - get-scale = sort a collection of "make-set"s by ratio size

; ----
; test
; ----
(p/pprint
  (list
    (make-set :otonal 3 3)
    (make-set :utonal 3 3)
    (make-set :otonal 5 3)
    (make-set :utonal 5 3)))