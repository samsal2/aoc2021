(ns aoc.day4 (:require [aoc.util :as util]
                       [clojure.string :as str]))

(def whole-input (util/read-file "inputs/day4.txt"))
(def board-input (->> whole-input (rest)))

(defn parse-board-line [line]
  (as-> line $ (str/trim $) (str/split $ #"\s{1,2}") 
        (mapv #(let [] {:value (util/as-int %) :state false}) $)))

(defn boards-reducer [boards line]
  (if (empty? line) (conj boards [])
      (let [new-last (as-> boards $ 
                       (last $) 
                       (conj $ (parse-board-line line)))]
        (conj (pop boards) new-last))))

(defn get-boards [input]
  (->> input (reduce boards-reducer [])))

(defn get-steps [input]
  (as-> input $ (str/split $ #",") (map (partial util/as-int) $)))

(def steps (->> whole-input (first) (get-steps)))
(def boards (get-boards board-input))

(defn mark-line [line num]
  (->> line (map #(if (= (:value %) num) (assoc % :state true) %))))

(defn mark-board [board num]
  (->> board (map #(mark-line % num))))

(defn mark-all-boards [boards num]
  (->> boards (map #(mark-board % num))))

;; hardcoded for 5x5
(defn check-diag-bt [board]
  (and (as-> board $ (nth $ 0) (nth $ 0) (:state $))
       (as-> board $ (nth $ 1) (nth $ 1) (:state $))
       (as-> board $ (nth $ 2) (nth $ 2) (:state $))
       (as-> board $ (nth $ 3) (nth $ 3) (:state $))
       (as-> board $ (nth $ 4) (nth $ 4) (:state $))))

(defn check-diag-tb [board]
  (and (as-> board $ (nth $ 0) (nth $ 4) (:state $))
       (as-> board $ (nth $ 1) (nth $ 3) (:state $))
       (as-> board $ (nth $ 2) (nth $ 2) (:state $))
       (as-> board $ (nth $ 3) (nth $ 1) (:state $))
       (as-> board $ (nth $ 4) (nth $ 0) (:state $))))

(defn check-line [line]
  (->> line (every? #(:state %))))

(defn transpose [data] 
  (apply mapv vector data))

(defn check-rows [board]
  (->> board (map check-line) (some true?)))

(defn check-cols [board]
  (->> board (transpose) (check-rows)))

(defn wins? [board]
  (or (check-cols board) 
      (check-rows board)))
      ;;(check-diag-bt board) 
      ;;(check-diag-tb board)))

(defn gather-unmarked-reductor [unmarked v]
  (if (:state v) unmarked (conj unmarked v)))

(defn gather-unmarked [board]
  (->> board (flatten) (reduce gather-unmarked-reductor [])))

(defn add-unmarked [board]
  (->> board (gather-unmarked) (reduce (fn [s v] (+ s (:value v))) 0)))

(defn part1-reductor [boards n]
  (as-> boards $ (mark-all-boards $ n) (let [] [$ (filter wins? $)])
        (let [[next won] $] (if (= (count won) 0) next
                    (->> won (first) (add-unmarked) (* n) (reduced))))))
                  
 (defn part1 [boards steps] 
   (->> steps (reduce part1-reductor boards)))

(defn part2-reductor [boards n]
  (as-> boards $ (mark-all-boards $ n) (filter #(->> % (wins?) (not)) $)
        (let [] (if (= (count $) 0) 
                  ;; HACK: using boards instead of $, as per the game
                  ;; it's imposible to have more than 1 last board,
                  ;; this means this board minus the mark is the score.
                  ;; Do this because well, if boards is empty after filtering,
                  ;; which one is the board?
                  (->> boards 
                       (first) 
                       (add-unmarked) 
                       (+ (- n)) 
                       (* n) 
                       (reduced))
                  $))))

 (defn part2 [boards steps] 
   (->> steps (reduce part2-reductor boards)))






