(ns adventofcode.day7
  (:require [clojure.string :as str]))

;; Parse input
(defn parse-bag-color-amount [str]
  (let [[color count] (reverse (drop 1 (re-matches #"(\d)\s(.+)\sbags?\.?" str)))]
    [color (Integer/parseInt count)]))

(defn identify-color [string]
  (str/split string #" bags contain "))

(defn bag-rules [string]
  (if (= "no other bags." string)
    {}
    (into {} (map parse-bag-color-amount (str/split string #", ")))))

(defn parse-line [str]
  (let [[color rest] (identify-color str)
        rules (bag-rules rest)]
    [color rules]))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (into {})))

(def input (parse-input (slurp "src/adventofcode/day7input.txt")))

(def test-input (parse-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))



;; Part 1
(defn holds-shiny-gold? [holds]
  (contains? holds "shiny gold"))

(defn can-hold-shiny-gold? [[_color holds] bag-map]
  (if (holds-shiny-gold? holds)
    true
    (boolean (->> holds
                  (map (fn [[color _count]]  (can-hold-shiny-gold? [color (get bag-map color {})] bag-map)))
                  (some true?)))))

(defn solve [bag-map]
  (map (fn [[color holds]]
         [color (can-hold-shiny-gold? [color holds] bag-map)]) bag-map))

(->> input
     (solve)
     (filter (fn [[_color holds-gold?]] holds-gold?))
     (reduce #(conj %1 (first %2)) #{})
     (count))

;; Part 2
(defn bag-hold-count [holds bag-map]
  (reduce (fn [acc [color count]]
            (+ acc (+ count (* count (bag-hold-count (get bag-map color {}) bag-map))))) 0 holds))

(defn solve2 [bag-map]
  (bag-hold-count (get bag-map "shiny gold" {}) bag-map))

(->> input
     (str/split-lines)
     (map parse-line)
     (into {})
     (solve2))
