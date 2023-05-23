(ns holycow.core
  (:require
   [clojure.string :as string]))

(def words-path "resources/palavras")
(def five-letter-words-path "resources/cinco-letras")

(defn deaccent
  [str]
  (java.text.Normalizer/normalize str java.text.Normalizer$Form/NFD))

(defn read-words-file
  [path]
  (-> path
      slurp
      string/trim
      (string/split #"\n")))

;get words from file
;(def words (read-words-file words-path))

;write file with 5 letter words
;(->> words
     ;(filter #(= 5 (count %)))
     ;(string/join "\n")
     ;(spit five-letter-words-path))

(def five-letter-words (read-words-file five-letter-words-path))

(defn add-occurrence
  ([letter map]
   (add-occurrence letter 1 map))
  ([letter occurrences map]
   (let [sum-occurrences (get map letter)]
     (if sum-occurrences
       (assoc map letter (+ sum-occurrences occurrences))
       (assoc map letter occurrences)))))

(defn letter-occurrences
  [word]
  (reduce #(add-occurrence (str %2) %1) {} word))

(defn sum-occurrences
  [total map]
  (reduce #(add-occurrence (first %2) (second %2) %1) total map))

(def occurrences
  (reduce sum-occurrences {} (map letter-occurrences five-letter-words)))

(defn gen-score
  [word occurrences]
  {:word word
   :score (reduce #(+ %1 (get occurrences %2)) 0 (re-seq #"." word))})

(def ranks
  (->> five-letter-words
       (map #(gen-score % occurrences))
       (sort-by :score)
       reverse))

(defn build-pattern-contains-distinct-chars
  [word]
  (re-pattern (str ".*(" (string/join "|" (distinct word)) ").*")))

(def strongest-pair
  (let [strongest-batman (first ranks)
        robin-pattern (build-pattern-contains-distinct-chars (:word strongest-batman))
        strongest-robin (second (filter (fn [word] (not (re-matches robin-pattern (:word word)))) ranks))]
    {:first strongest-batman
     :second strongest-robin}))

;start game
;(println strongest-pair)

(defn build-any-location-pattern
  [tip]
  (re-pattern (string/lower-case (string/replace tip #"_" ".*"))))

(defn build-exact-location-pattern
  [tip]
  (let [pattern (map #(cond
                        (Character/isUpperCase (char %)) (string/lower-case %)
                        :else ".{1}") (seq tip))]
    (re-pattern (string/join pattern))))

;mid-late game
;(def tip "___rA") ;TODO add support for "letter doesnt exist in this position"

;(take 3 (filter #(and (re-matches (build-exact-location-pattern tip) (:word %))
                      ;(re-matches (build-any-location-pattern tip) (:word %))) ranks))
