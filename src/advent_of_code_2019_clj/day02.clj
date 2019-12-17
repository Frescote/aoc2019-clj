(ns advent-of-code-2019-clj.day02)
(require '[clojure.string :as str])

(def input-string (slurp "resources/day02.txt"))
(defn read-input [input] (->> (-> input (str/split #",")) (map read-string) (vec)))
(defn setup-input [noun verb] (-> (read-input input-string) (assoc 1 noun) (assoc 2 verb)))
(def part1-input (setup-input 12 2))

(defn zero-pad [opcode]
  (format "%05d" opcode))

(defn parameter-value [memory opcode param-number value]
  (if (= (nth opcode (- 2 param-number)) "0") 
    (nth memory (nth memory value))
    (nth memory value)))

(defn parameters
  ([memory pointer length]
   (parameters memory (subvec memory pointer (+ length pointer))))
  ([memory [_opcode p0 p1 p2]]
   (let [opcode (zero-pad _opcode)
         param-getter (partial parameter-value memory opcode)]
      [(param-getter 0 p0) (param-getter 1 p1) p2])))

(defn execute-instruction 
  ([memory operation [p0 p1 p2]]
   (assoc memory p2 (operation p0 p1))))

(defn run-instruction [memory pointer input operation length]
  [(execute-instruction memory operation (parameters memory pointer length))
   (+ length pointer)
   input])

(defn run-program [memory pointer input]
  (print memory)
  (let [opcode (zero-pad (nth memory pointer))
        run (partial run-instruction memory pointer input)]
    (case (subs opcode 3)
      "01" (apply run-program (run + 4))
      "02" (apply run-program (run * 4))
      "99" (nth memory 0))))

(def part1 (run-program part1-input  0 []))

(assert (= 3500 (run-program (read-input "1,9,10,3,2,3,11,0,99,30,40,50") 0 [])))