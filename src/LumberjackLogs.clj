; some reddit challenge, attempt to learn some clojure!
(ns clojure-challenges-logs.core
  (use clojure.tools.trace)
  (use clojure.pprint)
  (:require [clojure.string :as cstr]))

; data containing the paths and how much they can take
(def data '(
  (:A :B 6) (:A :C 2)
  (:B :E 3) (:B :D 3)
  (:D :C 2) (:D :F 1)
  (:C :G 5)
  (:E :H 1) (:E :I 2)
  (:F :H 1)
  (:G :H 2) (:G :I 2)
  (:H :I 4)))

;------------------------------------------------------------

(defn add-network-value [network [from to capacity]]
  (let [with-relation (update-in network [:relations to] #(conj % from))]
    (assoc-in with-relation [:capacities {:from from :to to}] capacity)))

(defn create-network [data]
  (reduce add-network-value nil data))

(defn generate-paths [network path start end]
  (if (= start end)
    [path]
    (let [paths (map #(cons {:to end :from %1} path) (get-in network [:relations end]))]
      (mapcat #(generate-paths network %1 start ((first %1) :from)) paths))))

(defn reduce-capacity [network relations amount]
  (if (> amount 0)
   ; reduce each relations capacity in the network by amount
   (reduce #(update-in %1 [:capacities %2] - amount) network relations)
   network))

(defn print-send-logs [path current amount]
  (let [nodes (cons ((first path) :from) (map #(%1 :to) path))
        path-str (cstr/replace (cstr/join "->" nodes) ":" "")]
    (println (str "Log #" current " takes " path-str " - path of " (count nodes)))
    (if (> amount 1)
      (recur path (inc current) (dec amount)))))

(defn log-paths [network paths total]
  (if (empty? paths)
    (println (str "River is now full. Can send " total " logs."))
    (let [path (first paths)
          ; amount = min of remaining capacities on each relation
          amount (apply min (map #(get-in network [:capacities %1]) path))]
      ; side-effect: print send logs in case one or more logs can be sent:
      (if (> amount 0) (print-send-logs path (inc total) amount))
      ; continue with the remaining paths, having sent amount of logs
      (recur (reduce-capacity network path amount) (rest paths) (+ total amount)))))

(defn paths-between [network start end]
  (generate-paths network [] start end))

(defn sort-paths [paths]
  (sort #(compare (count %1) (count %2)) paths))

(defn main [start end]
  (let [network (create-network data)]
    (log-paths network (sort-paths (paths-between network start end)) 0)))

(main :A :I)
