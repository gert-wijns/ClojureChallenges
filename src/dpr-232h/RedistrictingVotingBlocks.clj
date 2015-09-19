(ns dpr-232h.core
  (use clojure.pprint)
  (use clojure.tools.trace))

(defrecord Region [x y length width voters])
(defn transpose-region [{:keys [x y length width voters]}]
  (->Region y x width length voters))

(defn count-voters [voters-data row cols]
  (let [row-data (nth voters-data row)]
    (->> cols
         (map #(nth row-data %))
         (reduce +))))

(defn calculate-best-ratio
  "iterate rows to find the best split"
  [voters-data rows cols target-voters total-voters]
  (reduce (fn [result row]
            (let [left-voters (+ (count-voters voters-data row cols)
                                 (:voters result))
                  right-voters (- total-voters left-voters)
                  left-voters-diff (Math/abs (- target-voters left-voters))
                  right-voters-diff (Math/abs (- target-voters right-voters))
                  improved-left? (< left-voters-diff (:left-voters-diff result))
                  improved-right? (< right-voters-diff (:right-voters-diff result))]
              (if (or improved-left? improved-right?)
                (let [result (if improved-left?
                               (assoc result
                                 :left-voters-diff left-voters-diff
                                 :left-voters left-voters
                                 :left-row (inc row))
                               result)
                      result (if improved-right?
                               (assoc result
                                 :right-voters-diff right-voters-diff
                                 :right-voters right-voters
                                 :right-row (inc row))
                               result)]
                  (assoc result :voters left-voters))
                (reduced result))))
          {:left-voters-diff target-voters
           :right-voters-diff target-voters
           :voters 0
           :left-voters 0
           :right-voters total-voters
           :left-row 0
           :right-row 0}
          rows))

(defn calculate-best-match
  [voters-data
   start-row end-row
   start-col end-col
   target-voters-a
   total-voters]
  (let [ncols (- end-col start-col)
        rows (range start-row end-row)
        cols (range start-col end-col)
        {:keys [left-voters-diff left-voters left-row
                right-voters-diff right-voters right-row]}
        (calculate-best-ratio voters-data
                              rows cols
                              target-voters-a total-voters)]
    (if (<= left-voters-diff right-voters-diff)
      [(->Region start-col start-row ncols (- left-row start-row) left-voters)
       (->Region start-col left-row ncols (- end-row left-row) (- total-voters left-voters))]
      [(->Region start-col right-row ncols (- end-row right-row) right-voters)
       (->Region start-col start-row ncols (- right-row start-row) (- total-voters right-voters))])))

(defn calculate-best-split [state region target-voters-a-percentage]
  (let [{:keys [x y length width voters]} region
        end-row (+ y width)
        end-col (+ x length)
        target-voters-a (* voters target-voters-a-percentage)
        [h-region-a h-region-b]
        (calculate-best-match (:voters state)
                              y end-row x end-col
                              target-voters-a voters)
        diffH (Math/abs (- target-voters-a (:voters h-region-a)))
        [v-region-a v-region-b]
        (calculate-best-match (:voters-transposed state)
                              x end-col y end-row
                              target-voters-a voters)
        diffV (Math/abs (- target-voters-a (:voters v-region-a)))]
    (if (or (< diffH diffV) (and (= diffH diffV) (> length width)))
      [h-region-a h-region-b]
      [(transpose-region v-region-a) (transpose-region v-region-b)])))

(defn split-region
  "Recursively splits the region into split-amount sub regions"
  [state region n-regions]
  (if (= 1.0 n-regions)
    [region]
    (let [a (Math/floor (/ n-regions 2.0))
          b (Math/ceil (/ n-regions 2.0))
          ratio (/ a (+ a b))
          [region-a region-b] (calculate-best-split state region ratio)]
      (concat (split-region state region-a a)
              (split-region state region-b b)))))

(defn calculate-regions
  [state]
  (let [{lines :lines voters :voters
         rows :rows cols :cols} state]
    (split-region
     state
     (->Region 0 0 cols rows (->> voters
                                  (map #(reduce + %))
                                  (reduce +)))
     (inc lines))))

(defn read-input [fileName]
  (let [data (-> fileName
                 slurp                           ; read file to a string
                 read-string                     ; parse string to data structure
                 (update-in [:voters] vec))] ; change voters to vectors for random access
    (assoc data :voters-transposed
           (vec (apply map vector (:voters data))))))

(defn top-symbol [region-map row col]
  (if (= (get-in region-map [[(dec row) col] :region-id])
         (get-in region-map [[row col] :region-id]))
    " " "-"))

(defn left-symbol [region-map row col]
  (if (= (get-in region-map [[row (dec col)] :region-id])
         (get-in region-map [[row col] :region-id]))
    " " "|"))

(defn row-string [state region-map row]
  (let [row-end (if (= row 0) "+\n" "|\n")
        vs (map #(let [top (top-symbol region-map row %)
                       top2 (top-symbol region-map row (dec %))
                       left (left-symbol region-map row %)
                       left2 (left-symbol region-map (dec row) %)]
                   {:top-left (cond
                               (and (or (= top "-") (= top2 "-"))
                                    (or (= left "|") (= left2 "|"))) "+"
                               (= top "-") "-"
                               (= left "|") "|"
                               :else " ")
                    :count (get-in state [:voters row %])
                    ;:count (get-in region-map [[row %] :region-id])
                    :top top :left left})
                (range (:cols state)))]
    (str
     (reduce #(str %1 (:top-left %2) (:top %2)) "" vs) row-end
     (reduce #(str %1 (:left %2) (:count %2)) "" vs) "|\n")))

(defn state-string [state regions]
  (let [region-map (reduce #(let [region (assoc %2 :region-id (:region-id %1))
                                  {:keys [x y length width]} region]
                              (-> %1
                                  (update-in [:region-id] inc)
                                  (into 
                                   (for [cx (range x (+ x length))
                                         cy (range y (+ y width))]
                                     [[cy cx] region]))))
                           {:region-id 1} regions)
        ]
    (str (reduce #(str %1 (row-string state region-map %2))
                 "" (range (:rows state)))
         "+" (reduce str (take (- (:rows state) 1) (repeat "--"))) "-+")))

(defn output-regions [state regions]
  (let [total-voters (->> regions (map :voters) (reduce +))
        average-voters (double (/ total-voters (count regions)))
        errors (map (fn [region]
                      (->> region
                           :voters
                           (- average-voters)
                           Math/abs))
                    regions)
        total-error (reduce + errors)]
    (println (state-string state regions))
    (pprint {:total total-voters :average average-voters
             :error total-error :errors (map int errors)})
    (pprint regions)))

(defn -main [file-name & args]
  (let [state (read-input file-name)]
    (->> state
         calculate-regions
         (output-regions state))))
