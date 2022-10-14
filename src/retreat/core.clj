(ns retreat.core)


(def array [
            [0 1 0]
            [1 0 0]
            [1 1 0]
            ]
  )

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
  (def row-length (count (nth main-array 0)))
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

(defn logic-loop [main-array]
  (def data (flatten main-array))
  (def row-length (count (nth main-array 0)))

  (loop [i 0]
    (when (< i (count data))

      (print (nth data i))
      (if (= 0 (mod (+ i 1) row-length)) (println "") ())
      (recur (+ i 1))
      )
    )
  )

(defn print-loop [main-array] 
  (def data (flatten main-array))
  (def row-length (count (nth main-array 0)))

  (loop [i 0]
    (when (< i (count data))
      (print (nth data i))
      (if (= 0 (mod (+ i 1) row-length)) (println "") ())
      (recur (+ i 1))
      )
    )
  )


(print-loop array)
(println "--------")
(logic-loop array)
(println "--------")

(println (get-neighbors 2 array))