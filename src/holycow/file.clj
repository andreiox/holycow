(ns holycow.file
  (:require [clojure.string :as string]))

;get words from file
(def words
  (-> "resources/test-palavras"
      slurp
      string/trim
      (string/split #"\n")))

;write file with 5 letter words
(->> words
     (filter #(= 5 (count %)))
     (string/join "\n")
     (spit "resources/test-cinco-letras"))
