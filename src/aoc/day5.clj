(ns aoc.day5 (:require [aoc.util :as util]
                       [clojure.string :as str]))

(def line-pattern (re-pattern #"([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"))
             
(defn dbg-println [w] (println w) w)

(defn unpack-line [row]
  (->> (re-matcher line-pattern row) (re-find) (rest) (map util/as-int)))

(defn parse-line [row] 
  (let [n (unpack-line row)]
    {:from {:x (nth n 0) :y (nth n 1)} :to {:x (nth n 2) :y (nth n 3)}}))

(defn is-straight [{from :from to :to}]
  (or (= (:x from) (:x to)) (= (:y from) (:y to))))

(defn get-input [file]
  (->> file (util/read-file) (map parse-line)))

(defn add-point-to-board [board point]
    (if (contains? board point)
      (assoc board point (as-> board $ ($ point) (inc $)))
      (assoc board point 1)))

(defn get-close [{from :from to :to}]
  (if (= (:x from) (:x to))
    (if (< (:y from) (:y to)) {:from {:x (:x from) :y (->> from (:y) (inc))}
                               :to {:x (:x from) :y (->> to (:y) (inc))}})
    (if (< (:x from) (:x to)) {:from {:x (->> from (:x) (inc)) :y (:y from)}
                               :to {:x (->> to (:x) inc) :y (:y from)}})))

(defn fill-board-iterator [board {from :from to :to}]
  (if (and (= (:x from) (:x to)) (= (:y from) (:y to))) board
      #(fill-board-iterator (get-close {:from from :to to})
      

(defn straight-only [lines]
  (->> lines (filter is-straight)))


