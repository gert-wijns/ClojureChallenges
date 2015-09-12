; some reddit challenge, attempt to learn some clojure!
(ns dpr-226.core)

(defn read-moves [file]
  (let [input (slurp file)]
    (->> input
         (filter #(Character/isLetter %))
         (map (fn [c]
                (-> c
                    Character/toLowerCase
                    int
                    (- (int \a))))))))

(defn ray
  [sx sy dx dy]
  (let [x (+ sx dx)
        y (+ sy dy)]
    (cons [x y] (lazy-seq (ray x y dx dy)))))

(defn play-move [board x turn]
  (let [move (first (drop-while #(get board %) (ray x -1 0 1)))]
    (-> board
        (assoc move turn)
        (update :played-moves #(cons move %)))))

(defn check-direction
  [board [row col] [dx dy]]
  (let [player (board [row col])
        matches? #(when (= (get board %) player) %)
        right (take-while matches? (ray row col dx dy))
        left (take-while matches? (ray row col (- dx) (- dy)))]
    (when (<= 3 (->> [right left] (map count) (reduce +)))
      (concat (reverse left) [[row col]] right))))

(defn check-win [board]
  (let [last-move (first (board :played-moves))]
     (reduce (fn [result direction]
              (when-let [winning-sequence (check-direction board last-move direction)]
                (reduced winning-sequence)))
            nil [[1 0] [0 1] [1 1]])))

(defn play-moves [all-moves]
  (loop [[x & moves] all-moves
         [turn & turns] (cycle [:X :O])
         board {:played-moves []}]
    (if x
      (let [board (play-move board x turn)
            win-moves (check-win board)]
        (if win-moves
          [turn win-moves (-> (get board :played-moves) count (/ 2) (Math/ceil))]
          (recur moves turns board))))))

(defn print-moves
  [[winner combination turn]]
  (let [offset-char (if (= winner :X) \A \a)
        combs (map (fn [[x y]] (str (char (+ x (int offset-char))) (inc y))) combination)]
    (println (str winner " won at move " turn " (with "
                  (apply str (interpose " " combs)) ")"))))

(defn play-game [file]
  (->> file
       (read-moves)
       (play-moves)
       (print-moves)))
