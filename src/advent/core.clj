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
