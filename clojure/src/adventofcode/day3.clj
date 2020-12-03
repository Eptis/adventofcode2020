(ns adventofcode.day3
  (:require [clojure.string :as str]))


(def input (slurp "src/adventofcode/day3input.txt"))

; Part 1
(defn line-to-seq [str]
  (cycle (vec (seq str))))

(defn landscape [input]
  (->> input
       str/split-lines
       (map line-to-seq)))

(def testinput "..##.......
..##.......")


(defn at-index [seq n]
  (nth seq n))

(defn count-trees [landscape]
  (loop [landscape landscape
         result '()
         index 0]
    (let [land (first landscape)
          scape (rest landscape)]
      (if (empty? scape)
        (reverse (cons (at-index land index) result))
        (recur scape (cons (at-index land index) result) (+ 3 index))))))


(get (->> input
          landscape
          count-trees
          frequencies)  \# 0)


;  Part 2
(defn count-trees2 [landscape {hor :hor vert :vert}]
  (loop [landscape landscape
         result '()
         index 0]
    (let [land (first landscape)
          scape (rest landscape)
          remaining (drop (- vert 1) scape)
          new-result (cons (at-index land index) result)]
      (if (empty? remaining)
        (reverse new-result)
        (recur remaining new-result (+ hor index))))))


(defn tree-count [input instr]
  (get (->> input
            landscape
            (#(count-trees2 % instr))
            frequencies)  \# 0))

(->> [{:hor 1, :vert 1}
      {:hor 3, :vert 1}
      {:hor 5, :vert 1}
      {:hor 7, :vert 1}
      {:hor 1, :vert 2}]
     (map #(tree-count input %))
     (reduce *))
