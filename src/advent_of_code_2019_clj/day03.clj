(ns advent-of-code-2019-clj.day03)
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input-string (slurp "resources/day03.txt"))
(defn split-movement [movement] [(first movement) (read-string (apply str (rest movement)))])
(defn process-input-path [input] (->> (str/split input #",") (map split-movement) (vec)))
(defn read-input [input] (->> (str/split input #"\n") (map process-input-path) (vec)))
(def input (read-input input-string))

(defn expand-path [current-path movement]
  (let [starting-point (last current-path)
        distance (range 1 (inc (last movement)))]
    (case (first movement)
      \D (->> distance (map (fn [d] [(first starting-point) (- (last starting-point) d)])))
      \L (->> distance (map (fn [d] [(- (first starting-point) d) (last starting-point)])))
      \U (->> distance (map (fn [d] [(first starting-point) (+ (last starting-point) d)])))
      \R (->> distance (map (fn [d] [(+ (first starting-point) d) (last starting-point)]))))))

(defn full-path [encoded-path]
  (rest (reduce (fn [acc element] (concat acc (expand-path acc element))) [[0 0]] encoded-path)))

(defn all-full-paths [encoded-paths] (->> encoded-paths (map full-path)))

(def part1 (->> input
                (all-full-paths)
                (map set)
                (apply set/intersection)
                (map (fn [p] (+ (Math/abs (first p)) (Math/abs (last p)))))
                (apply min)))

(defn distances-to [points path] 
  (->> path
       (map-indexed vector)
       (filter #(contains? points (second %1)))))


(def part2
  (let [all-paths (->> input all-full-paths)
        intersections (->> all-paths (map set) (apply set/intersection))
        distances (->> all-paths (map (partial distances-to intersections)))]
    (->> distances
         (apply concat)
         (group-by second)
         (map second)
         (map (fn [x] (+ (-> x first first inc) (-> x second first inc))))
         (sort)
         (first))))

part2

