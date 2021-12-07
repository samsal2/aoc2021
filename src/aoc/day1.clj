(ns aoc.day1 (:require [aoc.util :as util]))

(defn part1 [input]
  (->>  input
       (partition 2 1)
       (filter (partial apply <))
       (count)))

(defn part2 [input]
  (->> input
       (partition 3 1)
       (map (partial reduce +))
       (part1)))

(defn main [args]
  (->> args (map util/as-int) (part1) (println))
  (->> args (map util/as-int) (part2) (println)))

