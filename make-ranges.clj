(ns test1.core
  [:require
   [clojure.string :refer [join]]])


(defn merge-intervals [intervals interval]
  (if
    (= (inc (last (last intervals))) (first interval)) (conj (pop intervals) [(first (last intervals)) (last interval)])
    (conj intervals interval)))

(comment (merge-intervals [[1 1] [2 2]] [3 3]))
(comment (merge-intervals [[1 1] [2 2]] [4 4]))


(defn render [interval]
  (let [lo (first interval)
        hi (last interval)]
    (cond
      (= lo hi) (format "\\u%04x" lo)
      (= (inc lo) hi) (format "\\u%04x\\u%04x" lo hi)
      :else (format "\\u%04x-\\u%04x" lo hi))))

(comment (render [1 5]))


(defn fold [items]
  (let [intervals (mapv #(identity [% %]) items)]
    (->> (rest intervals)
         (reduce merge-intervals [(first intervals)])
         (map render)
         join)))

(comment (fold [1 2 3 8 9 13 30]))


(defn to-hex [s]
  (Integer/parseInt s 16))


(defn extract-num [s]
  (->> (re-find #"0x([0-9a-fA-F]{4})" s)
       last
       to-hex))

(comment (extract-num "hello 0x0010 world"))


(defn process [lines]
  (->> (mapv extract-num lines)
       sort
       fold))


(println
  (process (line-seq (java.io.BufferedReader. *in*))))
