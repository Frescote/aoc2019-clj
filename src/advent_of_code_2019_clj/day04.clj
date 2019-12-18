(ns advent-of-code-2019-clj.day04)

(defn always-increasing [number]
  (->> number
       (str) 
       (map #(Character/getNumericValue %))
       (vec)
       (apply <=)))

(defn has-two-digits-equal [number]
  (->> number
       (str)
       (map #(Character/getNumericValue %))
       (frequencies)
       (map second)
       (some (partial <= 2))))

(defn has-two-digits-equal-strict [number]
  (->> number
       (str)
       (map #(Character/getNumericValue %))
       (frequencies)
       (map second)
       (some (partial = 2))))

(def part1 (->> (range 109165 576723)
                (filter always-increasing)
                (filter has-two-digits-equal)
                (count)))


(def part2 (->> (range 109165 576723)
                (filter always-increasing)
                (filter has-two-digits-equal-strict)
                (count)))

part2