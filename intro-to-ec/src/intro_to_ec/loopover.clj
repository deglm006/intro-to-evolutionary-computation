(ns intro-to-ec.loopover)

(defn legal? [board-size n]
  (and (<= 0 n) (< n board-size)))

(defn swap [board old-pos new-pos]
  "Swap the tile in `old-pos` with that in `new-pos`."
  (let [old-value (get-in board old-pos)
        new-value (get-in board new-pos)]
    (assoc-in (assoc-in board old-pos new-value)
              new-pos old-value)))

(defn shift
  [board colRow dir n]
  (let [board-size-1 (dec (count board))]
    (if colRow
      (if dir
        (reduce #(swap %1 [n %2] [n (inc %2)]) board  (range board-size-1))
        (reduce #(swap %1 [n %2] [n (dec %2)]) board  (range board-size-1 0 -1))
      )
      (if dir
        (reduce #(swap %1 [%2 n] [(inc %2) n]) board  (range board-size-1))
        (reduce #(swap %1 [%2 n] [(dec %2) n]) board  (range board-size-1 0 -1))
      ))))

(defn children [board]
  "Generate the collection of child states that are reachable
   from the given state. This is what would result from moving
   any column up or down and any row left or right"
  (let [board-size (count board)]
    (for [colRow [true false]      dir [true false]      n (range 0 board-size)]
          (shift board colRow dir n))))

(defn num-wrong [goal-board current-board]
  "A simple heuristic that counts the number of incorrectly
   placed tiles."
  (count (filter identity
                 (map not=
                      (flatten goal-board)
                      (flatten current-board)))))

(defn zero-or-same? [[x y]]
  (or (= x y)
      (zero? x)
      (zero? y)))

(defn num-non-blank-wrong [goal-board current-board]
  "A simple heuristic that counts the number of incorrectly
   placed non-blank tiles."
  (count (remove zero-or-same?
                 (map (fn [x y] [x y])
                      (flatten goal-board)
                      (flatten current-board)))))

(defn make-loopover-problem
  [goal-board heuristic]
  {:goal? #(= goal-board %)
   :make-children children
   :heuristic heuristic})


; A B C D
; E F G H
; I J K L
; M N O P
