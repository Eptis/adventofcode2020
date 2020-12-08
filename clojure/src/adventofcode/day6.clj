(ns adventofcode.day6
  (:require [clojure.string :as str]))


(def input (slurp "src/adventofcode/day6input.txt"))

(def test-input "abc

a
b
c

ab
ac

a
a
a
a

b")

;; Part 1
(->> (map str/split-lines (str/split input #"\R\R"))
     (map #(map char-array %))
     (map #(set (reduce concat [] %)))
     (map count)
     (reduce +))

;; Part 2
(defn answered-yes-by-all [group]
  (let [group-size (count group)
        answers (->> group
                     (map char-array)
                     (reduce concat [])
                     frequencies)]
    (reduce (fn [acc [k v]]
              (if (= v group-size)
                (cons k acc)
                acc))
            '() answers)))

(->> (map str/split-lines (str/split input #"\R\R"))
     (map answered-yes-by-all)
     (map count)
     (reduce +))
