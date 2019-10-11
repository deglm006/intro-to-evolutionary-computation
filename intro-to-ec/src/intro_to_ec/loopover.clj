(ns intro-to-ec.loopover)

; This essentially defines a type, `State`, that is a map
; with at least two keys, `:board` and `:blank-position`.
(defrecord State [board])

(defn legal? [board-size n]
  (and (<= 0 n) (< n board-size)))

(defn swap [board old-pos new-pos]
  "Swap the tile in `old-pos` with that in `new-pos`."
  (let [old-value (get-in board old-pos)
        new-value (get-in board new-pos)]
    (assoc-in (assoc-in board old-pos new-value)
              new-pos old-value)))

; (reduce #(assoc-in board %1 %2)

(defn shift 
  [board colRow dir n]
  (if colRow
    (if dir
      (reduce #(swap board [n %] [n (inc %)]) (range (dec (count (:board state)))))
      (doseq [i (range (dec (count (:board state))))]
        (swap board [n i] [n (inc i)]))
      (doseq [i (range (dec (count (:board state))) 0 -1)]
        (swap board [n i] [n (dec i)]))
    )
    (if dir
      (doseq [i (range (dec (count (:board state))))]
        (swap board [i n] [(inc i) n]))
      (doseq [i (range (dec (count (:board state))) 0 -1)]
        (swap board [i n] [(dec i) n]))
    )
  )
)

(defn children [state]
  "Generate the collection of child states that are reachable
   from the given state. This is what would result from moving
   the 'blank' space up, down, left, and right."
  (let [board-size (count (:board state))]
    (for colRow [true false]
      (for dir [true false]
        (for n (range 0 board-size)
          (->State (shift (:board state) colRow dir n)))))))

(defn state->vec [state]
  (flatten (:board state)))

(defn num-wrong [goal-state current-state]
  "A simple heuristic that counts the number of incorrectly
   placed tiles."
  (count (filter identity
                 (map not=
                      (state->vec goal-state)
                      (state->vec current-state)))))

(defn zero-or-same? [[x y]]
  (or (= x y)
      (zero? x)
      (zero? y)))

(defn num-non-blank-wrong [goal-state current-state]
  "A simple heuristic that counts the number of incorrectly
   placed non-blank tiles."
  (count (remove zero-or-same?
                 (map (fn [x y] [x y])
                      (state->vec goal-state)
                      (state->vec current-state)))))

(defn make-n-puzzle-problem
  [goal-board heuristic]
  {:goal? #(= goal-board (:board %))
   :make-children children
   :heuristic heuristic})


A B C D
E F G H
I J K L
M N O P
