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
        (reduce #(swap %1 [n %2] [n (dec %2)]) board  (range board-size-1 0 -1)))
      (if dir
        (reduce #(swap %1 [%2 n] [(inc %2) n]) board  (range board-size-1))
        (reduce #(swap %1 [%2 n] [(dec %2) n]) board  (range board-size-1 0 -1))))))

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

(defn squar [goal-board current-board i]
  (let [n (count goal-board)]
    (every? identity
            (for [x (range (- n i) n)
                  y (range (- n i) n)]

              (if (= (get-in goal-board [x y])
                     (get-in current-board [x y]))
                true
                false)))))

(defn row [goal-board current-board pos]
  (let [n (count goal-board)]
    (every? identity
            (for [y (range 1 n)]
              (if
               (= (get-in goal-board [pos y])
                  (get-in current-board [pos y]))
                true
                false)))))

(defn rightRow [goal-board current-board i j]

  (or
   (for [k (range (count goal-board))]

     (if
      (=
       (get (get goal-board i) k)
       (get (get current-board i) j))
       true
       false))))

(defn numRightRow [goal-board current-board i]
  (count (filter identity
                 (for [j (range (count goal-board))]
                   (rightRow goal-board current-board i j)))))

(defn coords [board val]
  (filter identity (flatten
  (for [x (range (count board))]
    (for [y (range (count board))]
      (if (= val (get (get board x) y))
        [x y]
        nil))))))

(defn distance-away [goal-board current-board val]
  ;(println (coords goal-board val))
  (+ (Math/abs (* 1.25 (- (first (coords goal-board val))
                  (first (coords current-board val)))))
     (Math/abs (* 0.75 (- (last  (coords goal-board val))
                  (last  (coords current-board val)))))))

(defn weighted-num-wrong [goal-board current-board]
  "A simple heuristic that counts the number of incorrectly
   placed tiles."

  (apply +'
         (flatten
          (concat
           (for [x (range (count goal-board))]
             (for [y (range (count goal-board))]
               (if
                (=
                 (get (get goal-board x) y)
                 (get (get current-board x) y))
                 (+ (* x 1.25)  (* y 1.75))
                 0)))

           (for [x (range (count goal-board))]
             (for [y (range (count goal-board))]
               (- (* 2 (dec (count goal-board)))
                  (distance-away goal-board current-board (get (get current-board x) y)))))

           (for [x (range (count goal-board))]
             (Math/pow 2 (* x (numRightRow goal-board current-board x))))

           (for [x (range 1 (inc(count goal-board)))]
             (if (= (intro-to-ec.loopover/squar goal-board current-board x) true)
               (Math/pow 8 x)
               0))

           (for [x (range 1 (count goal-board))]
             (if (intro-to-ec.loopover/row goal-board current-board x)
               (Math/pow 4 x)
               0))))))

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
        shuf (partition n (shuffle (flatten goal-board)))]
    (vec
     (for [x (range n)]
       (vec (nth shuf x))))))

(defn make-rand [goal-board]
  (loop [thing (children goal-board)
         num 0]
    (if (= num 100000)
      (rand-nth thing)
      (recur
       (children (rand-nth thing))
       (inc num)))))

(def goal-board-2 [[0 1] [2 3]])

(def goal-board-3 [[0 1 2] [3 4 5] [6 7 8]])

(def goal-board-4 [[\a \b \c \d] [\e \f \g \h] [\i \j \k \l] [\m \n \o \p]])

(defn goal? [goal-board]
  #(= goal-board %))

(defn make-loopover-problem
  [goal-board heuristic]
  {:goal? (goal? goal-board)
   :make-children children
   :heuristic (partial heuristic goal-board)
   :goal-board goal-board})

;(hs/a-star-search hs/heuristic-search (lo/make-loopover-problem lo/goal-board-3 lo/weighted-num-wrong) [[5 7 1] [2 0 3] [6 4 8]] 150)
; Best 116 calls - 13 Moves
;(hs/a-star-search hs/heuristic-search (lo/make-loopover-problem lo/goal-board-3 lo/weighted-num-wrong) (lo/randomize lo/goal-board-3) 300)
;(hs/a-star-search hs/heuristic-search (lo/make-loopover-problem lo/goal-board-4 lo/weighted-num-wrong) [[\p \o \m \n][\k \l \j \i][\g \h \e \f][\c \d \b \a]] 300)
;(hs/a-star-search hs/heuristic-search (lo/make-loopover-problem lo/goal-board-3 lo/weighted-num-wrong) [[2 0 1][3 4 5][8 6 7]] 100)
;(require '[intro-to-ec.loopover :as lo])(require '[intro-to-ec.heuristic-search :as hs])
