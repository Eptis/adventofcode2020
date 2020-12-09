(ns adventofcode.day9;
  (:require [clojure.string :as str]))

;; Parse input
(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(Long/valueOf %))
       vec))

(def input (parse-input (slurp "src/adventofcode/day9input.txt")))

(def test-input (parse-input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"))

;; Part 1
test-input

(defn preamble-set [input idx preamble]
  (subvec input (- idx preamble) idx))

(defn can-make-sum? [target set]
  (let [combinations (for [i set
                           j set]
                       [i j])]
    (some (fn [[a b]] (= (+ a b) target)) combinations)))

(defn solve [preamble ns]
  (loop [idx preamble]
    (let [target (nth ns idx :not-found)]
      (cond
        (= target :not-found) nil
        (can-make-sum? target (preamble-set ns idx preamble)) (recur (inc idx))
        :else target))))

(solve 5 test-input)
(solve 25 input)

;; Part 2
;; 127
;; 15690279
(defn solve2 [input target]
  (loop [idx 0 moving-idx 1]
    (let [input-size (count input)
          range (if (or (>= moving-idx input-size) (>= idx input-size))
                  []
                  (subvec input idx moving-idx))]
      (cond
        (and (>= moving-idx input-size) (>= idx input-size)) :not-found
        (empty? range) (recur (inc idx) (+ idx 2))
        (= (reduce + range) target) (+ (apply min range) (apply max range))
        :else (recur idx (inc moving-idx))))))

(solve2 test-input 127)
(solve2  input 15690279)
