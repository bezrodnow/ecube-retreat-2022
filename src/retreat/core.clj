(ns retreat.core)


(def array [
            [0 0 0 0 0 0 0 0 0 0 0 0]
            [1 1 1 0 0 0 1 1 1 0 0 0]
            [1 0 0 0 0 0 1 1 1 0 0 0]
            [0 0 0 0 0 0 0 0 0 0 0 0]
            ]
  )


(def row-length (count (nth array 0)))

(defn left-neighbor [index, row]
  (def left (- index 1))
  (if (> left -1) (nth row left) 0)
  )

(defn right-neighbor [index, row]
  (def right (+ index 1))
  (if (< right (count row)) (nth row right) 0)
  )

(defn row-neighbors [index, row]
  (def left (- index 1))
  (def right (+ index 1))

  (if (= row nil) [] [(nth row index) (left-neighbor index row) (right-neighbor index row)])

  )

(defn own-neighbors [index, row]
  (def left (- index 1))
  (def right (+ index 1))

  (if (= row nil) [] [(left-neighbor index row) (right-neighbor index row)])

  )

(defn get-neighbors [index main-array]
  (def current-row (int (/ index row-length)))
  (def prev-row-index (- current-row 1))
  (def next-row-index (+ current-row 1))
  (def row-index (mod index row-length))

  (def prev-row (if (> prev-row-index -1)
                  (if (< prev-row-index (count main-array)) (nth main-array prev-row-index))))
  (def next-row (if (> next-row-index -1)
                  (if (< next-row-index (count main-array)) (nth main-array next-row-index))))


  [(own-neighbors row-index (nth main-array current-row)) (row-neighbors row-index prev-row) (row-neighbors row-index next-row)]
  )

(defn do-for-alive [number-alive-neighbors]


  (def res (if (< number-alive-neighbors 2) 0 (if (> number-alive-neighbors 3) 0 1)))
  res
  )

(defn do-for-dead [number-alive-neighbors]
  (if (= number-alive-neighbors 3) 1 0)
  )

(defn logic-loop [main-array]
  (def flat-data (flatten main-array))

  (def alive-neighbors-for-index (map
                                   (fn [i] (reduce + (flatten (get-neighbors i main-array)))
                                     )
                                   (vec (range (count flat-data)))
                                   ))

  (def new-data (map
                  (fn [i] (
                            if (= (nth flat-data i) 1)
                            (do-for-alive (nth alive-neighbors-for-index i))
                            (do-for-dead (nth alive-neighbors-for-index i))
                            )
                    )
                  (vec (range (count alive-neighbors-for-index)))
                  ))



  new-data

  )

(defn print-loop [flat-data]

  (loop [i 0]
    (when (< i (count flat-data))
      (print (if (= (nth flat-data i) 1) "X" " "))


      (if (= 0 (mod (+ i 1) row-length)) (println "") ())
      (recur (+ i 1))
      )
    )
  )


(print-loop (flatten array))
(println "------------------")
(def flat-array (logic-loop array))
(print-loop (logic-loop array))




;(println flat-array)
;(println (subvec flat-array 0 row-length))
