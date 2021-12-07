(ns aoc.day2 (:require [aoc.util :as util]
                       [clojure.string :as str]))

(def data (->> "inputs/day2.txt" (util/read-file)
  (map #(let [[cmd dist] (str/split % #" ")] 
           [(keyword cmd) (util/as-int dist)]))))

(defn pre-proc-cmd-part1 [[cmd dist]]
  (case cmd :forward [dist     0]
            :up      [0 (- dist)]
            :down    [0 (+ dist)]))

(defn cmd-reductor-part1 [[px py] [x y]] [(+ px x) (+ py y)])

(defn part1 [data]
  (->> data (map pre-proc-cmd-part1) (reduce cmd-reductor-part1)))

(defn pre-proc-cmd-part2 [[cmd dist]]
  (case cmd :forward [:forward [dist     0] 0]
            :up      [:up      [0 (- dist)] (- dist)]
            :down    [:down    [0 (+ dist)] (+ dist)]))
  
(defn cmd-reductor-part2 [[pcmd [px py] paim] [_ [x y] aim]]
  (case cmd :forward [pcmd [(+ px x)  (+ py (* paim x))] (+ paim aim)]
                     [pcmd [(+ px x)           (+ py y)] (+ paim aim)]))

(defn fix-result-part2 [[_ [x y] aim]] [x y])

(defn part2 [data]
  (->> data (map pre-proc-cmd-part2) (reduce cmd-reductor-part2) (fix-result-part2)))

(defn main [args]
  (->> data (part1) (reduce *) (println))
  (->> data (part2) (reduce *) (println)))

