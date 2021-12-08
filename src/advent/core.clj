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

(defn parse-bingo-block [block]
  (map
   (fn [row]
     (into [] (filter #(not (clojure.string/blank? %)) row)))
   (map #(clojure.string/split % #" ") block)))

(def geusses (clojure.string/split (first (split-file "resources/day4.input")) #","))

(defn get-next-n-geusses [n]
  (take n geusses))

(def bingo-blocks (map #(parse-bingo-block %) (partition 5 (filter #(not (= "" %)) (rest (split-file "resources/day4.input"))))))

(defn check-bingo [rows geusses]
  (not (empty? (filter #(= 5 (count %)) (map #(set/intersection geusses %) (map #(into #{} %) rows))))))

(defn is-matching-block? [block geusses]
  (let [block-t (apply mapv vector block)
        matching-rows (check-bingo block geusses)
        matching-cols (check-bingo block-t geusses)]
    (or matching-rows matching-cols)))

(defn day4_1 []
  (loop [cnt 5
         c-geuss (get-next-n-geusses cnt)]
    (let [geuss-as-set (into #{} c-geuss)
          bingo-block (filter #(is-matching-block? % geuss-as-set) bingo-blocks)]
      (if (not (empty? bingo-block))
        (let [sum-non-matching-numbers (reduce + (map #(Integer. %) (set/difference (apply set/union (map #(into #{} %) (first bingo-block))) geuss-as-set)))]
          (* sum-non-matching-numbers (Integer. (last c-geuss))))
        (recur (inc cnt) (get-next-n-geusses (inc cnt)))))))

(defn day4_2 []
  (loop [cnt 5
         c-geuss (get-next-n-geusses cnt)
         prev-match-block '()]
    (let [geuss-as-set (into #{} c-geuss)
          bingo-block (filter #(is-matching-block? % geuss-as-set) bingo-blocks)]
      (if (= (count bingo-block) (count bingo-blocks))
        (let [prev-match-block-as-set (into #{} prev-match-block)
              bingo-block-as-set (into #{} bingo-block)
              new-bingo-block (set/difference bingo-block-as-set prev-match-block-as-set)
              sum-non-matching-numbers
              (reduce +
                      (map #(Integer. %)
                           (set/difference (apply set/union (map #(into #{} %)
                                                                 (first new-bingo-block))) geuss-as-set)))]
          (* sum-non-matching-numbers (Integer. (last c-geuss))))
        (recur (inc cnt) (get-next-n-geusses (inc cnt)) bingo-block)))))

(defn create-line-segment [coords]
  (let [[[x1 y1] [x2 y2]] coords
        [a b] (sort (list (Integer/parseInt x1) (Integer/parseInt x2)))
        [c d] (sort (list (Integer/parseInt y1) (Integer/parseInt y2)))]
    (for [xs (range a (inc b))
          ys (range c (inc d))]
      [xs ys])))

(defn create-skew-line-segment [coords]
  (let [[[x1 y1] [x2 y2]] coords
        [a b] (sort (list (Integer/parseInt x1) (Integer/parseInt x2)))
        [c d] (sort (list (Integer/parseInt y1) (Integer/parseInt y2)))]
    (for [xs (range a (inc b))
          ys (range c (inc d))
          :when (= xs ys)]
      [xs ys])))

(defn parse-non-skew-lines [input-file]
  (for [[a b]
        (map #(clojure.string/split % #" -> ") (split-file input-file))
        :let [[x1 y1] (clojure.string/split a #",")
              [x2 y2] (clojure.string/split b #",")]
        :when (or (= x1 x2) (= y1 y2))]
    (list [x1 y1] [x2 y2])))

(defn parse-skew-lines [input-file]
  (for [[a b]
        (map #(clojure.string/split % #" -> ") (split-file input-file))
        :let [[x1 y1] (clojure.string/split a #",")
              [x2 y2] (clojure.string/split b #",")]
        :when (not (or (= x1 x2) (= y1 y2)))]
    (list [x1 y1] [x2 y2])))

(defn create-lines [lines]
  (map #(create-line-segment %) lines))

(defn day5_1 [input-file]
  (count (filter #(> (val %) 1)
                 (frequencies
                  (mapcat identity (create-lines (parse-non-skew-lines input-file)))))))
