(ns aoc.util (:require [clojure.string :as str]))

(defn read-file [f] (str/split-lines (slurp f)))

(defn as-int [s] (Integer/parseInt s))

(defn as-nums [lines] (->> lines (map util/as-int)))
