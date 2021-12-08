(ns aoc.day3 (:require [aoc.util :as util]))

(def data (->> "inputs/day3.txt" (util/read-file) 
   (map seq) (map (partial map #(Character/digit % 10)))))

(def width (->> (nth data 0) (count)))

(defn exp [x n]
  (reduce *(repeat n x)))

(defn transpose [data] 
  (apply mapv vector data))

(defn most-common [row] 
  (let [ones (reduce + row)]
    ;; who would want to use booleans as numbers??? \s
    (if (> ones (- (count row) ones)) 1 0))) 

(defn as-decimal [b]
  (let [s (count b)]
    (->> b (map-indexed #(if (zero? %2) 0 (exp 2 (- s %1 1)))) (reduce +))))

(defn invert [row]
  (->> row (map #(if (zero? %) 1 0))))

(defn gamma [data]
  (->> data (transpose) (map most-common) (as-decimal)))

(defn epsilon [data]
  (->> data (transpose) (map most-common) (invert) (as-decimal)))

(defn part1 [data] 
  (* (gamma data) (epsilon data)))

;; who cares about speed
(defn filter-unwanted-at [data at state]
  (if (= 1 (count data)) 
    data
    (->> data (filter #(= (nth % at) state)))))

(defn count-at [data at state]
  (->> (filter-unwanted-at data at state) (count)))

(defn oxygen-choose-filter-state [data at]
  (let [ones (count-at data at 1)]
    (if (> (- (count data) ones) ones) 0 1)))

(defn oxygen-filter-unwanted [data at]
  (let [state (oxygen-choose-filter-state data at)]
    (filter-unwanted-at data at state)))

(defn co2-choose-filter-state [data at]
  (let [ones (count-at data at 1)]
    (if (> (- (count data) ones) ones) 1 0)))

(defn co2-filter-unwanted [data at]
  (let [state (co2-choose-filter-state data at)]
    (filter-unwanted-at data at state)))

(defn oxygen [data]
  (->> (range width) 
       (map #(let [] [% nil])) 
       (reduce (fn [[_ p] [i _]] 
                 [i (oxygen-filter-unwanted p i)]) 
               [0 data]) 
       (last)
       (last)
       (as-decimal)))

(defn co2 [data]
  (->> (range width) 
       (map #(let [] [% nil])) 
       (reduce (fn [[_ p] [i _]] 
                 [i (co2-filter-unwanted p i)]) 
               [0 data]) 
       (last)
       (last)
       (as-decimal)))

(defn part2 [data]
  (* (oxygen data) (co2 data)))
