(ns letter-boxed.core
  (:gen-class)
  (:require
   [clojure.string :as s]
   [clojure.set]
   [clojure.math.combinatorics :as combo]))

(defn valid-word?
  "Is the word valid for the sides passed in"
  ([word sides]
   (valid-word? word sides -1))

  ([word sides bad-side]
   (if (empty? word)
     true
     (let [l (first word)
           letter-on-this-side
           (->> sides
                (filter #(not= (first %) bad-side))
                (filter #(contains? (second %) l))
                (map first))]
       (if (empty? letter-on-this-side)
         false
         (recur (rest word) sides (first letter-on-this-side)))))))

(defn words-can-join?
  "Determine if `w1`'s last element can be matched to `w2`'s head"
  [w1 w2]
  (= (last w1) (first w2)))

(defn all-letters-used?
  "Are all the letters in `sides` used in the words?"
  [required-letters words]
  (let [used-letters (into #{} (flatten (map seq words)))]
    (clojure.set/subset? required-letters used-letters)))

(defn strip-read-lines
  "Read lines from file and strip each"
  [file]
  (->> file
       (slurp ,,,)
       (s/lower-case ,,,)
       (s/split-lines ,,,)
       (map s/trim ,,,)
       (filter #(not (s/ends-with? % "'s")) ,,,)))

(defn create-sides
  "Create a map of the sides of the board"
  [letters]
  (zipmap
   (range)
   (map set (partition 3 letters))))

(defn puzzle-slns
  "Return a seq of all the two words solutions to the puzzle"
  [valid-words sides]
  (let [required-letters (apply clojure.set/union (vals sides))]
    (->> (combo/permuted-combinations valid-words 2)
         (filter #(apply words-can-join? %))
         (filter (partial all-letters-used? required-letters)))))

(defn -main

  [& args]
  (def letters (first args))
  (println "the letters of the board are" letters)
  (def sides (create-sides letters))
  (println "The sides of the board are " sides)
  (def f "american_english_dictionary.txt")
  (def words (strip-read-lines f))
  (def valid-words (into [] (filter #(valid-word? % sides) words)))
  (println "Found " (count valid-words) " valid words")
  (def slns (puzzle-slns valid-words sides))
  (println slns))

