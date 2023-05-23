(ns holycow.file
  (:require [clojure.string :as string]))

(def words-path "resources/test-palavras")
(def five-letter-words-path "resources/test-cinco-letras")

(defn deaccent [str]
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
   (let [occurrences (get map letter)]
     (if occurrences
       (assoc map letter (+ occurrences 1))
       (assoc map letter 1))))
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

(println occurrences)
