(ns advent-of-code-2019-clj.day02)
(require '[clojure.string :as str])

(def input-string (slurp "resources/day05.txt"))
(defn read-input [input] 
  (->> (str/split input #",") (map read-string) (vec)))

(defn zero-pad [opcode]
  (format "%05d" opcode))

(defn parameter-value [memory opcode param-number memory-position]
  (if (= (nth opcode (- 2 param-number)) "0")
    (nth memory (nth memory memory-position))
    (nth memory memory-position)))

(defn math-parameters
  ([memory pointer length]
   (math-parameters memory (subvec memory pointer (+ length pointer))))
  ([memory [_opcode p0 p1 p2]]
   (let [opcode (zero-pad _opcode)
         param-getter (partial parameter-value memory opcode)]
     [(param-getter 0 p0) (param-getter 1 p1) p2])))

(defn execute-math
  ([memory operation [p0 p1 p2]]
   (assoc memory p2 (operation p0 p1))))

(defn run-math-instruction [length operation memory pointer]
    [(execute-math memory operation (math-parameters memory pointer length))
     (+ length pointer)])

(defn do-math [operation memory pointer]
  (let [length 4]
    [(run-math-instruction length operation memory pointer) (+ length pointer)]))

(def do-sum (partial do-math +))
(def do-multiply (partial do-math *))

(defn save-input [memory pointer input]
  [(assoc memory (inc pointer) (first input)) 
   (+ 2 pointer) 
   (vec (rest input))])

(defn run-program [memory pointer input]
  (let [opcode (zero-pad (nth memory pointer))]
    (case (subs opcode 3)
      "01" (apply run-program (conj (do-sum memory pointer) input))
      "02" (apply run-program (conj (do-multiply memory pointer) input))
      "03" (apply run-program (save-input memory pointer input))
      "04" (nth memory (parameter-value memory opcode 0 (inc pointer)))
      "99" (nth memory 0))))

(def part1 (run-program (read-input input-string) 0 [1]))
part1
;(assert (= 3500 (run-program (read-input "1,9,10,3,2,3,11,0,99,30,40,50") 0 [])))