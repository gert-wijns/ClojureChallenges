; some reddit challenge, attempt to learn some clojure!
(ns dpr-189.core
  (use clojure.tools.trace)
  (use clojure.pprint)
  (:require [clojure.string :as cstr]))

; significance of roman number characters:
(def to-value {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 nil 0})
(def valid-roman-string-chars #{\M \D \C \L \X \V \I \( \)})
(def allowed-next-greater-chars {\I #{\V \X} \X #{\L \C} \C #{\D \M}})
(defn max-repeats [numchar] (if (= numchar \M) 4 3))
(def max-valid-roman-number 4999)

(defn has-invalid-characters [numstr]
  (not (empty? (apply str (remove #(valid-roman-string-chars %) numstr)))))

(defn inc-repeats [numchar repeats]
  (if (= (repeats :symbol) numchar)
    (assoc repeats :repeats (inc (repeats :repeats)))
    {:symbol numchar :repeats 1}))

(defn to-10-base
  ([numstr]
    (if (has-invalid-characters numstr)
      (format "Invalid characters found in %s" numstr)
      (to-10-base numstr 1 0 {})))
  ([numstr multiplier total repeats]
    (let [numchar (first numstr)]
      (cond
        ; no remaining part end sum, done
        (nil? numchar) total
        ; start sub number with multiplier
        (= numchar \() (recur (rest numstr) (* multiplier 1000) total repeats)
        ; end sub number with multiplier
        (= numchar \)) (recur (rest numstr) (/ multiplier 1000) total repeats)
        :else (let [current-value (to-value numchar)
                    next-numchar (second numstr)
                    next-value (to-value next-numchar)
                    inced-repeats (inc-repeats numchar repeats)]
          (cond
           (nil? next-value) (recur (rest numstr) multiplier (+ (* multiplier current-value) total) {})
           (and (= next-numchar \)) (= current-value \I))
             "Character I is not allowed before closing bracket."
           (and (= next-value current-value) (= (inced-repeats :repeats) (max-repeats numchar)))
             "Character repeats too many times."
           (and (> next-value current-value) (> (inced-repeats :repeats) 1))
             "Character repeats too many times before greater number."
           (<= next-value current-value)
             (recur (rest numstr) multiplier (+ (* multiplier current-value) total) inced-repeats)
           (not (contains? (allowed-next-greater-chars numchar) next-numchar))
             (format "Character [%s] not allowed after [%s]." next-numchar numchar)
           :else (recur (rest (rest numstr)) multiplier (+ (* multiplier (- next-value current-value)) total) {})))))))

; to roman conversion:
(def roman-base-numstr ["I" "IV" "V" "IX" "X" "XL" "L" "XC" "C" "CD" "D" "CM" "M"])
(def roman-base-values (reduce #(cons {:numstr %2 :numval (to-10-base %2)} %1) [] roman-base-numstr))

; rebase number so it won't end in I
;(defn round [number]
;  (let [number-mod-5 (mod number 5)]
;    (if (< number-mod-5 4) (- number number-mod-5) number)))
(defn round [number]
  (let [number-mod-10 (mod number 10) number-mod-5 (mod number 5)]
    (cond
     (== number-mod-10 9) number
     (< number-mod-5 5) (- number number-mod-5)
     :else number)))

(defn encode-lower
  ([number] (encode-lower number roman-base-values))
  ([number [code & codes]]
  (if (zero? number)
    ""
    (let [numval (code :numval)
          times (Math/floor (/ number numval))
          remaining (- number (* times numval))]
      (if (pos? times)
        (apply str (apply str (repeat times (code :numstr))) (encode-lower remaining codes))
        (encode-lower remaining codes))))))

(defn to-roman [number]
  (cond
    (> number max-valid-roman-number)
      (let [higher (round (Math/floor (/ number 1000)))
            lower (- number (* higher 1000))]
        (str \( (to-roman higher) \) (to-roman lower)))
    (> number 0) (encode-lower number)
    :else ""))

; test unknown char
(to-10-base "IIW")
; test preceeding char
(to-10-base "IM")
(to-10-base "IIX")
(to-10-base "XVII")
; test repeat
(to-10-base "IIII")
(to-10-base "MMMM")
(to-10-base "MMMMM")

(def input-str
 "IV = 4
  XXXIV = 34
  CCLXVII = 267
  DCCLXIV = 764
  CMLXXXVII = 987
  MCMLXXXIII = 1983
  MMXIV = 2014
  MMMM = 4000
  MMMMCMXCIX = 4999
  (V) = 5000
  (V)CDLXXVIII = 5478
  (V)M = 6000
  (IX) = 9000
  (X)M = 11000
  (X)MM = 12000
  (X)MMCCCXLV = 12345
  (CCCX)MMMMCLIX = 314159
  (DLXXV)MMMCCLXVII = 578267
  (MMMCCXV)CDLXVIII = 3215468
  (MMMMCCX)MMMMCDLXVIII = 4214468
  (MMMMCCXV)CDLXVIII = 4215468
  (MMMMCCXV)MMMCDLXVIII = 4218468
  (MMMMCCXIX)CDLXVIII = 4219468
  ((XV)MDCCLXXV)MMCCXVI = 16777216
  ((CCCX)MMMMCLIX)CCLXV = 314159265
  ((MLXX)MMMDCCXL)MDCCCXXIV = 1073741824")
(def inputs (map #(cstr/split %1 #"=") (-> input-str (cstr/replace " " "") (cstr/split #"\n"))))
(defn to-10-base-str [numstr expected-numval]
  (let [actual-numval (to-10-base numstr)]
    (if (= expected-numval (str actual-numval))
      (format "%s = %s" numstr actual-numval)
      (format "ERROR: %s = %s but got %s" numstr expected-numval actual-numval))))
(println (cstr/join "\n" (map #(to-10-base-str (first %1) (second %1)) inputs)))
(defn to-roman-str [number expected-numstr]
  (let [actual-numstr (to-roman number)]
    (if (= expected-numstr actual-numstr)
      (format "%s = %s" actual-numstr number)
      (format "ERROR: %s = %s but got %s (= %s)" expected-numstr number actual-numstr (to-10-base actual-numstr)))))
(println (cstr/join "\n" (map #(to-roman-str (Long/parseLong (second %1)) (first %1)) inputs)))
