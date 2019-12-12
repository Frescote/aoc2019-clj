(ns advent-of-code-2019-clj.day01)
(require '[clojure.string :as str])

(defn fuel-for-mass-raw [mass]
  (-> mass (quot 3) (- 2) (max 0)))

(defn fuel-for-mass [mass]
  (loop [fuel (fuel-for-mass-raw mass)
         acc 0]
    (if (zero? fuel)
      acc
      (recur (fuel-for-mass-raw fuel) (+ acc fuel)))))

(def input (-> (slurp "resources/day01.txt") (str/split #"\n")))

(defn compute-fuel [fuel-function]
  (->> input (map read-string) (map fuel-function) (reduce +)))
(def part1 (compute-fuel fuel-for-mass-raw))
(def part2 (compute-fuel fuel-for-mass))

(assert (= 2 (fuel-for-mass-raw 12)))
(assert (= 50346 (fuel-for-mass 100756)))
