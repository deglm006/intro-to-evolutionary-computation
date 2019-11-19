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

(defn weighted-num-wrong [goal-board current-board]
  "A simple heuristic that counts the number of incorrectly
   placed tiles."
  (-(apply + (flatten
    (for [i (range (count goal-board))]
      (for [j (range (count goal-board))]
        ;(+ 0(* (inc(inc i))(inc j)
          (if
            (=
              (get (get goal-board i) j)
              (get (get current-board i) j))
            (+ (inc i) j (inc i) )
            0
            )
           ))))))
;))

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

(defn randomize [goal-board]
  "Takes a given board and shuffles the tiles."
  (let [n (count goal-board)
        shuf (partition n (shuffle (flatten goal-board)))
        ]
    (println shuf)
    (vec
     (for [x (range n)]
       (let [sub (first shuf)
             shuf (rest shuf)]
         (vec sub)
         )
       )
     )
    )
  )

(def goal-board-2 [[0 1][2 3]])

(def goal-board-3 [[0 1 2][3 4 5][6 7 8]])

(def goal-board-4 [[\a \b \c \d][\e \f \g \h][\i \j \k \l][\m \n \o \p]])

(defn make-loopover-problem
  [goal-board heuristic]
  {:goal? #(= goal-board %)
   :make-children children
   :heuristic (partial heuristic goal-board)
   :goal-board goal-board})


; A B C D
; E F G H
; I J K L
; M N O P
