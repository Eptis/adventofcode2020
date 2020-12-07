(ns adventofcode.day5
  (:require [clojure.string :as str]))


(def input (slurp "src/adventofcode/day5input.txt"))

(def test-input "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL")

; Part 1
(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map char-array)))

(defn half-index [seq]
  (Math/round (float (/ (count seq) 2))))

(defn take-upper [seq]
  (last (split-at (half-index seq) seq)))

(defn take-lower [seq]
  (first (split-at (half-index seq) seq)))

(defn binary-search [instructions range take-upper-predicate]
  (loop [[instr & remaining] instructions
         rows range]
    (if  (nil? instr)
      (first rows)
      (if (take-upper-predicate instr)
        (recur remaining (take-upper rows))
        (recur remaining (take-lower rows))))))

(defn find-row [instructions]
  (binary-search instructions (range 128) (fn [instr] (= instr \B))))

(defn find-column [instructions]
  (binary-search instructions (range 8) (fn [instr] (= instr \R))))

(defn find-seat [identifier]
  (let [row (find-row (take 7 identifier))
        column (find-column (last (split-at 7 identifier)))]
    (+ (* row 8) column)))

(apply max (->> input
                parse-input
                (map find-seat)))

;; Part 2;
(defn seat-range []
  (let [seats (->> input
                   parse-input
                   (map find-seat))
        lowest (apply min seats)
        highest (apply max seats)]
    [lowest highest (sort seats)]))

(let [[lowest highest seats] (seat-range)
      seat-range (range lowest (+ highest 1))
      seat-comp (map vector seat-range seats)
      ]
  seat-comp
  (some (fn [[a b]] (if (not= a b) a)) seat-comp)
  )

(sort (->> input
           parse-input
           (map find-seat)))
