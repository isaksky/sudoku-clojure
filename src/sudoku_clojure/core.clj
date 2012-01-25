(ns sudoku-clojure.core
  (:require [clj-http.client :as client])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn working-dir-path [path]
  (str (. System getProperty "user.dir") path ))

(def sudoku-text-file-path
  (working-dir-path "/resources/sudoku.txt"))

(def sudoku-puzzles
  (let [sudoku-puzzles-file-path (working-dir-path "/resources/sudoku.txt")
        file-contents (slurp sudoku-puzzles-file-path)
        puzzle-strings (map str/join
                            (map str/split-lines
                                 (filter #(not (str/blank? %))
                                         (str/split file-contents #"Grid \d+")))) ]
    (into []
          (for [ps puzzle-strings]
            (into []
                  (for [c (map str
                               (.toCharArray ps))]
                    (Integer. c)))))))

(defn print-puzzle [puzzle]
  (print
   (interpose "\n"
              (partition 9 puzzle))))

(defn puzzle-index [col row]
  (+ col
     (* row 9)))

(defn puzzle-row [puzzle row]
  (let [rows (partition 9 puzzle)]
    (nth rows row)))

(defn puzzle-column [puzzle col]
  (into []
        (for [row (range 0 9)]
          (puzzle (puzzle-index col row)))))

(defn subgrid-coords [col row]
  (map
   (fn [n] (int (/ n 3)))
   [col row]))

(defn puzzle-subgrid [puzzle coords]
  (let [[subgrid-col subgrid-row] coords]
    (into []
          (flatten
           (let [start-col (* 3 subgrid-col)
                 start-row (* 3 subgrid-row)]
             (for [col (range
                        start-col
                        (+ 3 start-col))]
               (for [row (range
                          start-row
                          (+ 3 start-row))]
                 (puzzle (puzzle-index col row)))))))))

(defn possible-moves [puzzle col row]
  "Get all the possible sudoku moves for a col and row index on a puzzle. The square cannot have a value already."
  {:pre (= 0 (puzzle (puzzle-index  col row)))}
  (apply set/difference
         (map set
              [(range 1 10)
               (puzzle-column puzzle col)
               (puzzle-row puzzle row)
               (puzzle-subgrid puzzle
                               (subgrid-coords col row))])))

;; for REPL testing
(def puzzle
  (first sudoku-puzzles))

