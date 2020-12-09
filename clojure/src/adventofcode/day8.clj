(ns adventofcode.day8
  (:require [clojure.string :as str]))

;; Parse input
(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(drop 1 (re-matches  #"([a-z]{3})\s\+?(\-?[0-9]+)" %)))
       (reduce (fn [acc instr]
                 (conj acc [(first instr) (Integer/parseInt (last instr))])) [])))

(def input (parse-input (slurp "src/adventofcode/day8input.txt")))

(def test-input (parse-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"))

;; Part 1
(defn solve [input]
  (loop [acc 0
         idx 0
         idxs #{}]
    (let [is (nth input idx)
          [instr step] is]
      (cond
        (contains? idxs idx) acc
        (= instr "nop") (recur acc (inc idx) (conj idxs idx))
        (= instr "jmp") (recur acc (+ idx step) (conj idxs idx))
        (= instr "acc") (recur (+ acc step) (inc idx) (conj idxs idx))))))

(solve input)


;; Part 2
(defn execute [input]
  (loop [acc 0
         idx 0
         idxs #{}]
    (let [is (nth input idx [])
          [instr step] is]
      (cond
        (nil? instr) [:terminate acc]
        (contains? idxs idx) [:infinite-loop acc]
        (= instr "nop") (recur acc (inc idx) (conj idxs idx))
        (= instr "jmp") (recur acc (+ idx step) (conj idxs idx))
        (= instr "acc") (recur (+ acc step) (inc idx) (conj idxs idx))))))

(defn problematic-indexes [input]
  (->> input
       (map-indexed (fn [index [instr count]]
                      (if (or (= instr "nop") (= instr "jmp"))
                        index)))
       (remove nil?)))

(defn flip-at-index [idx input]
  (let [[instr count] (nth input idx)
        new-input (cond
                    (= instr "nop") ["jmp" count]
                    (= instr "jmp") ["nop" count])]
    (assoc input idx new-input)))

(defn possible-solutions [input]
  (let [idxs (problematic-indexes input)
        try (map #(flip-at-index % input)  idxs)]
    try))


(defn solve2 [input]
  (some (fn [input]
          (let [[exit result] (execute input)]
            (if (= exit :terminate) result)))
        (possible-solutions input)))

(solve2 input)
