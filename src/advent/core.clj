(ns advent.core
  (:require [clojure.set :as set]))

(defn split-file [file]
  (clojure.string/split-lines (slurp file)))

(defn day1_1 [vals]
  (count (filter #(> 0 %1) (map #(- %1 %2) vals (rest vals)))))

(defn day1_2 [vals]
  (day1_1 (map #(reduce + %) (partition 3 1 vals))))
