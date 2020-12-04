(ns adventofcode.day4
  (:require [clojure.string :as str]))


(def input (slurp "src/adventofcode/day4input.txt"))


(def test-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

; Part 1
(defn valid-passport? [passport]
  (cond
    (= (count passport) 8) true
    (and (= (count passport) 7) (every? #(not (str/starts-with? % "cid")) passport)) true;
    :else false))

(->>
 (str/split input #"\n\n")
 (map #(str/replace % #"\n" " "))
 (map #(str/split % #"\s"))
 (map valid-passport?)
 (frequencies)
 (#(get % true 0)))

; Part 2
(defn validate-ecl [ecl]
  (some #(= % ecl) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))


(defn in-range [year min max]
  (and (<=  (Integer/parseInt year) max)
       (>= (Integer/parseInt year) min)))

(defn validate-hgt [hgt]
  (cond
    (str/ends-with? hgt "in") (in-range (str/replace hgt #"in" "") 59 76)
    (str/ends-with? hgt "cm") (in-range (str/replace hgt #"cm" "") 150 193)
    :else false))

(defn validate-hcl [hcl]
  (some? (re-matches  #"^\#[0-9a-f]{6}$" hcl)))

(defn validate-pid [pid]
  (some? (re-matches  #"^[0-9]{9}$" pid)))

(defn valid-passport?2 [passport]
  (every? true? [(in-range (get passport "byr" "0") 1920 2002)
                 (in-range (get passport "iyr" "0") 2010 2020)
                 (in-range (get passport "eyr" "0") 2020 2030)
                 (validate-ecl (get passport "ecl" ""))
                 (validate-hgt (get passport "hgt" ""))
                 (validate-hcl (get passport "hcl" ""))
                 (validate-pid (get passport "pid" ""))]))


(defn parse-password [passport]
  (into {} (map (fn [x] (str/split x #"\:"))
                (vec passport))))

(->>
 (str/split input #"\n\n")
 (map #(-> %
           (str/replace #"\n" " ")
           (str/split #"\s")
           (parse-password)
           (valid-passport?2)))
 (frequencies)
 (get true 0))
