(ns pitch-lattice.core
  (:require [clojure.pprint :as pprint]))

;; TODO
;; Have each pitch in the lattice be a record with a
;;  :cent         -- Decimal
;;  :ratio        -- Ratio
;;  :alpha        -- String
;;  :accidentals  -- String Vector
;;
;; Change this to generate loom graphs
(defrecord Pitch [cent ratio alpha accidentals])
(defrecord LatticeSet [otonal utonal])
(defrecord Lattice [root vertical horizontal])

(defn factors
  "Get the factors of a number"
  [num]
  (map #(/ num %) (filter #(zero? (rem num %)) (range 1 (+ num 1)))))

(defn prime?
  "Determine if a number is prime"
  [num]
  (.isProbablePrime (BigInteger/valueOf num) 5))

(defn ratio-to-list
  "Converts a ratio to a list"
  [ratio]
  (list (numerator ratio) (denominator ratio)))

(defn recip
  "Returns the reciprocal of a ratio"
  [ratio]
  (/ (denominator ratio) (numerator ratio)))

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

(defn add-ratios
  "Add an arbitrary number of ratios"
  [& ratios]
  (flatten-ratio (apply * ratios)))

(defn otonal-step
  "Takes one step up the lattice"
  [steps ratio]
  (apply add-ratios (replicate steps ratio)))

(defn utonal-step
  "Takes one step down the lattice"
  [steps ratio]
  (apply add-ratios (map flip-ratio (replicate steps ratio))))

(defn lattice-walk
  "Creates a list of steps in one direction on the lattice"
  [direction ratio steps]
  (map direction (range 1 (+ steps 1)) (replicate steps ratio)))

(defn gen-lattice-set
  [ratio steps]
  (LatticeSet.
    (lattice-walk otonal-step ratio steps)
    (lattice-walk utonal-step ratio steps)))







(defn generate-lattice
  "Builds an entire 2-D pitch lattice"
  [vratio vsteps hratio hsteps]
  (Lattice. 1
            (gen-lattice-set vratio vsteps)
            (gen-lattice-set hratio hsteps)))
