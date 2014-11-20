; some reddit challenge, attempt to learn some clojure!
(ns clojure-challenges-arrows.core
  (use clojure.tools.trace)
  (use clojure.pprint)
  (:require [clojure.string :as cstr]))

(def input0
  "v^
   >^")

(def input1
  ">>>>v
   ^v<<v
   ^>>^v
   ^>>v<
   ^<<<^")

(def input2
 "^^v>>v^>>v<<<v>v<>>>>>>>>^vvv^^vvvv<v^^><^^v>
  >><<>vv<><<<^><^<^v^^<vv>>^v<v^vv^^v<><^>><v<
  vv<^v<v<v<vvv>v<v<vv<^<v<<<<<<<<^<><>^><^v>>>
  <v<v^^<v<>v<>v<v<^v^>^<^<<v>^v><^v^>>^^^<><^v
  ^>>>^v^v^<>>vvv>v^^<^<<<><>v>>^v<^^<>v>>v<v>^
  ^^^<<^<^>>^v>>>>><>>^v<^^^<^^v^v<^<v^><<^<<<>
  v<>v^vv^v<><^>v^vv>^^v^<>v^^^>^>vv<^<<v^<<>^v
  <<<<<^<vv<^><>^^>>>^^^^<^<^v^><^v^v>^vvv>^v^^
  <<v^<v<<^^v<>v>v^<<<<<>^^v<v^>>>v^><v^v<v^^^<
  ^^>>^<vv<vv<>v^<^<^^><><^vvvv<<v<^<<^>^>vv^<v
  ^^v^>>^>^<vv^^<>>^^v>v>>v>>v^vv<vv^>><>>v<<>>
  ^v<^v<v>^^<>>^>^>^^v>v<<<<<>><><^v<^^v><v>^<<
  v>v<><^v<<^^<^>v>^><^><v^><v^^^>><^^<^vv^^^>^
  v><>^><vv^v^^>><>^<^v<^><v>^v^<^<>>^<^vv<v>^v
  ><^<v>>v>^<<^>^<^^>v^^v<>>v><<>v<<^><<>^>^v<v
  >vv>^>^v><^^<v^>^>v<^v><>vv>v<^><<<<v^<^vv<>v
  <><<^^>>^<>vv><^^<vv<<^v^v^<^^^^vv<<>^<vvv^vv
  >v<<v^><v<^^><^v^<<<>^<<vvvv^^^v<<v>vv>^>>^<>
  ^^^^<^<>^^vvv>v^<<>><^<<v>^<<v>>><>>><<^^>vv>
  <^<^<>vvv^v><<<vvv<>>>>^<<<^vvv>^<<<^vv>v^><^")

(defn add-row-to-cells [cells arrows row col]
  (if (empty? arrows)
    cells
    (let [cells+ (assoc cells [row col] (first arrows))]
      (recur cells+ (rest arrows) row (inc col)))))

(defn add-rows-to-cells [cells arrows-list row]
  (if (empty? arrows-list)
    cells
    (let [cells+ (add-row-to-cells cells (first arrows-list) row 0)]
      (recur cells+ (rest arrows-list) (inc row)))))

(defn parse-input [input]
  (let [arrows-list (-> input
                        (cstr/replace " " "")
                        (cstr/split #"\n"))]
    {:cells (add-rows-to-cells {} arrows-list 0)
     :rows (count arrows-list)
     :cols (count (first arrows-list))}))

; change position according to key
(def next-position-map {
   \> (fn [{cols :cols} [row col]] [row (if (= cols (inc col)) 0 (inc col))])
   \< (fn [{cols :cols} [row col]] [row (if (= 0 col) (dec cols) (dec col))])
   \^ (fn [{rows :rows} [row col]] [(if (= 0 row) (dec rows) (dec row)) col])
   \v (fn [{rows :rows} [row col]] [(if (= rows (inc row)) 0 (inc row)) col])})
(defn next-position [data start]
  ((next-position-map (get-in data [:cells start])) data start))

(defn remove-cells [data cells]
  (assoc data :cells (apply dissoc (data :cells) cells)))

(defn get-path
  ([data start] (get-path data start []))
  ([{:keys [cells] :as data} start path]
    (if (contains? cells start)
      (let [next-cell (next-position data start)
            next-data (remove-cells data [start])]
        (recur next-data next-cell (cons start path)))
      path)))

; if the last item of the path next cell is also part of the path,
; then the path contains a cycle. In that case, shorten path to only
; include the actual cycle
(defn get-cycle [data path]
  (let [next (next-position data (first path))]
    (if (.contains path next)
      (let [cycle (drop-while #(not (= %1 next)) path)]
        (if (empty? (rest cycle)) path cycle))
      nil)))

(defn try-find-cycle [{:keys [cells] :as data}]
  (let [path (get-path data (key (first cells)))
        data- (remove-cells data path)]
    (assoc data- :cycle (get-cycle data path))))

(defn find-cycles [data]
  (let [{:keys [cells cycle]} (try-find-cycle data)
        next-cycles (if (empty? cells) [] (find-cycles (assoc data :cells cells)))]
    (if (empty? cycle)
      next-cycles
      (cons cycle next-cycles))))

(defn find-longest-cycle [data]
  (first (sort #(compare (count %2) (count %1)) (find-cycles data))))

;create text to print:
(defn row-text [data cycle row]
  (let [text (for [col (range 0 (data :cols))]
                 (get-in data [:cells [row col]] " "))]
    (concat text "\n")))

(defn print-cells [data]
  (let [cells-text (for [row (range 0 (data :rows))] (row-text data cycle row))]
    (println (reduce concat cells-text))))

(defn print-result [data cycle]
  (let [cycle-data (remove-cells data (remove (set cycle) (keys (data :cells))))]
    (print-cells data)
    (print-cells cycle-data)))

(def alt-position {\> \R \< \L \^ \U \v \D})
(defn print-result-alt [data cycle]
  (let [cycle-cells (reduce #(assoc %1 %2 (alt-position (get-in data [:cells %2]))) (data :cells) cycle)]
    (print-cells (assoc data :cells cycle-cells))))

(let [data (parse-input input1)]
  (print-result data (find-longest-cycle data)))
