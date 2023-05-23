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
     ;(map deaccent)
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
   :score (reduce #(+ %1 (get occurrences %2)) 0 (distinct (re-seq #"." word)))})

(def ranks
  (->> five-letter-words
       (map #(gen-score % occurrences))
       (sort-by :score)
       reverse))

;TODO refactor: maybe use build-regex-any-letter-except
(defn build-pattern-contains-distinct-chars
  [word]
  (re-pattern (str ".*(" (string/join "|" (distinct word)) ").*")))

(defn start-game
  []
  (let [strongest-batman (second ranks)
        robin-pattern (build-pattern-contains-distinct-chars (:word strongest-batman))
        strongest-robin (second (filter (fn [word] (not (re-matches robin-pattern (:word word)))) ranks))]
    {:first strongest-batman
     :second strongest-robin}))

(defn build-regex-any-letter-except
  [letters]
  (if (empty? letters)
    "."
    (str "[a-z&&[^" letters "]]")))

(defn get-pattern-for-position
  [position]
  (if (empty? (:correct position))
    (build-regex-any-letter-except (:wrong position))
    (:correct position)))

(defn build-query-regex
  [game-state]
  (->> game-state
       :positions
       (map get-pattern-for-position)
       (apply str)
       re-pattern))

(defn mid-game
  []
  (let [game-state {:positions [{:correct "" :wrong ""}
                                {:correct "" :wrong ""}
                                {:correct "" :wrong ""}
                                {:correct "" :wrong ""}
                                {:correct "" :wrong ""}]
                    :contains [""]}]
    (take 10 (filter #(and (re-matches (build-query-regex game-state) (:word %))
                           (every? (fn [letter] (string/includes? (:word %) letter)) (:contains game-state))) ranks))))

;(start-game)
;(mid-game)
