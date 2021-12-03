(ns advent.core
  (:require [clojure.set :as set]))

(defn split-file [file]
  (clojure.string/split-lines (slurp file)))

(defn day1_1 [vals]
  (count (filter #(> 0 %1) (map #(- %1 %2) vals (rest vals)))))

(defn day1_2 [vals]
  (day1_1 (map #(reduce + %) (partition 3 1 vals))))

(defn get-new-pos [current-position move]
  (let [direction (first move)
        amount (Integer. (first (rest move)))
        current-pos (first current-position)
        current-depth (first (rest current-position))]
    (cond (= "forward" direction) (let [new-pos (+ current-pos amount)]
                                    (list new-pos current-depth))
          (= "up" direction) (let [new-depth (- current-depth amount)]
                               (list current-pos new-depth))
          (= "down" direction) (let [new-depth (+ current-depth amount)]
                                 (list current-pos new-depth)))))

(defn part2-get-new-pos [current-position move]
  (let [direction (first move)
        amount (Integer. (first (rest move)))
        current-pos (first current-position)
        current-depth (first (rest current-position))
        aim (first (rest (rest current-position)))]
    (cond (= "forward" direction) (let [new-horizontal (+ current-pos amount)
                                        new-depth (+ current-depth (* aim amount))]
                                    (list new-horizontal new-depth aim))
          (= "up" direction) (let [new-aim (- aim amount)]
                               (list current-pos current-depth new-aim))
          (= "down" direction) (let [new-aim (+ aim amount)]
                                 (list current-pos current-depth new-aim)))))

(defn day2_1 [moves]
  (let [final-coord (reduce (fn [acc move]
              (get-new-pos acc move)) (list 0 0) moves)]
    (* (first final-coord) (first (rest final-coord)))))

(defn day2_2 [moves]
  (let [final-coord (reduce (fn [acc move]
              (part2-get-new-pos acc move)) (list 0 0 0) moves)]
    (* (first final-coord) (first (rest final-coord)))))

(defn get-freq [input]
  (into [] (map #(frequencies %) (apply mapv vector input))))

(defn parse-to-decimal [binary-seq]
  (Integer/parseInt (apply str binary-seq) 2))

(defn invert-binary [binary]
  (map #(if (= \0 %) \1 \0) binary))

(defn gamma-binary [input]
  (map (fn [freq]
         (let [ones (get freq \1)
               zeros (get freq \0)]
           (if (> ones zeros) \1  \0))) (get-freq input)))

(defn day3_1 [input]
  (let [gamma-bin (gamma-binary input)
        epsilon-bin (invert-binary gamma-bin)]
    (* (parse-to-decimal gamma-bin) (parse-to-decimal epsilon-bin))))

(defn oxy-criteria [index input]
  (let [freq (get (get-freq input) index)
        ones (get freq \1)
        zeros (get freq \0)]
    (if (>= ones zeros) \1 \0)))

(defn co2-criteria [index input]
  (first (invert-binary (list (oxy-criteria index input)))))

(defn filter-on-criteria [criteria index input]
  (filter #(= criteria (get % index)) input))

(defn find-value [criteria-filter-fn input]
  (loop [index 0
         result (filter-on-criteria (criteria-filter-fn index input) index input)]
    (if (= 1 (count result)) result
        (recur (inc index) (filter-on-criteria (criteria-filter-fn (inc index) result) (inc index) result)))))

(defn day3_2 [input]
  (let [oxygen-rating (parse-to-decimal (first (find-value oxy-criteria input)))
        co2-rating (parse-to-decimal (first (find-value co2-criteria input)))]
    (* oxygen-rating co2-rating)))
