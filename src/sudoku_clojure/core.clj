(ns sudoku-clojure.core
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn working-dir-path [path]
  (str (. System getProperty "user.dir") path ))

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
             (for [row (range start-row (+ 3 start-row))
                   col (range start-col (+ 3 start-col))]
               (puzzle (puzzle-index col row))))))))

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

(defn- fill-obvious-pass [puzzle]
  "Takes a puzzle, returns the puzzle with all the empty slots with only 1 possible value filled."
  (into [] (for [row (range 0 9)
                 col (range 0 9)]
             (let [slot-value (puzzle (puzzle-index col row))]
               (if (> slot-value 0)
                 slot-value
                 (let [possible-slot-values (possible-moves puzzle col row)]
                   (if (= 1 (count possible-slot-values))
                     (first possible-slot-values)
                     0)))))))

(defn fill-obvious [puzzle]
  "Do this many times, because doing one fill-obvious-pass could change some slots, which changes the possible values for other slots. Some of them could therefore only have 1 left."
  (loop [current-puzzle puzzle]
    (let [new-puzzle (fill-obvious-pass current-puzzle)]
      (if (= current-puzzle new-puzzle)
        new-puzzle
        (recur new-puzzle)))))

(defn shallow-complete-check [puzzle]
  "Checks if all slots have a value."
  (not-any? #(= 0 %) puzzle))

(defn deep-complete-check [puzzle]
  "Checks that all columns, rows, and subgrids have the legal sudoku values."
  (let [sudoku-values (range 1 10)]
    (and
     (every?
      (fn [col] (= sudoku-values
                  (sort (puzzle-column puzzle col))))
      (range 0 9))
     (every?
      (fn [row] (= sudoku-values
                  (sort (puzzle-row puzzle row))))
      (range 0 9))
     (every?
      (fn [subgrid-coords] (= sudoku-values
                             (sort (puzzle-subgrid puzzle subgrid-coords))))
      (into [] (for [subgrid-row (range 0 3)
                     subgrid-col (range 0 3)]
                 [subgrid-col subgrid-row]))))))