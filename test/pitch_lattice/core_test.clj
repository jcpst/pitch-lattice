(ns pitch-lattice.core-test
  (:require [clojure.test :refer :all]
            [pitch-lattice.core :as pl]))

(deftest test-factors
  (let [result (pl/factors 81)]
    (is (= (sort '(1 3 9 27 81)) (sort result)))))

(deftest test-prime?
  (let [result (pl/prime? 13)]
    (is (= true result))))

(deftest test-ratio-to-list
  (let [result (pl/ratio-to-list 31/8)]
    (is (= '(31 8) result))))

(deftest test-recip
  (let [result (pl/recip 31/8)]
    (is (= 8/31 result))))

(deftest test-flatten-ratio
  (let [result (pl/flatten-ratio 6/4)]
    (is (= 3/2 result))))

(deftest test-flip-ratio
  (let [result (pl/flip-ratio 2/6)]
    (is (= 3/2 result))))

(deftest test-limit
  (let [result (pl/limit 91/48)]
    (is (= 13 result))))

(deftest test-add-ratios
  (let [result (pl/add-ratios 3/2 3/2)]
    (is (= 9/8 result))))

(deftest test-otonal-step
  (let [result (pl/otonal-step 2 3/2)]
    (is (= 9/8 result))))

;; TODO: Not sure why utonal-step blows up...
;(deftest test-utonal-step
;  (let [result (pl/utonal-step 2 4/3)]
;    (is (= 16/9 result))))
