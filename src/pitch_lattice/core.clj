(ns pitch-lattice.core)

(defrecord Pitch [cent ratio alpha accidentals]) ;; TODO - use this
(defrecord LatticeSet [otonal utonal])
(defrecord Lattice [root vertical horizontal])

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

(defn recip
  "Returns the reciprocal of a ratio"
  [num]
  (if (ratio? num)
    (/ (denominator num) (numerator num))
    (/ 1 num)))

(defn flatten-ratio
  "Brings the size of a ratio in between 1 and 2"
  [ratio]
  (cond (> ratio 2) (recur (/ ratio 2))
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

(defn add-ratios
  "Add an arbitrary number of ratios"
  [& ratios]
  (flatten-ratio (apply * ratios)))

(defn step
  "Interval size on the lattice based on direction and limit"
  [direction limit]
  (flatten-ratio
    (condp = direction
      :otonal limit
      :utonal (recip limit))))

(defn lattice-walk
  "Creates a list of steps in one direction on the lattice"
  [step num]
  (map #(apply add-ratios (replicate % step)) (range-inc num)))

; ----
; test
; ----
(def partial-limit (limit 81/64))
(def ratio (step :otonal partial-limit))
(def rows 5)

(println
  (lattice-walk ratio rows))