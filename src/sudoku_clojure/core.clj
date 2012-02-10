(ns sudoku-clojure.core
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn working-dir-path [path]
  (str (. System getProperty "user.dir") path ))

(def sudoku-puzzles
  "A vector with all the puzzles read from disk."
  (let [sudoku-puzzles-file (working-dir-path "/resources/sudoku.txt")
        file-contents (slurp sudoku-puzzles-file)
        puzzle-strings (map str/join
                            (map str/split-lines
                                 (filter #(not (str/blank? %))
                                         (str/split file-contents #"Grid \d+")))) ]
    (into [] (for [ps puzzle-strings]
               (into [] (for [char (map str (.toCharArray ps))]
                          (Integer. char)))))))

(def all-puzzle-coords
  (for [row (range 0 9)
        col (range 0 9)]
    [col row]))

(defn explode [msg]
  (throw (Exception. msg)))

(defn puzzle-index [col row]
  (+ col
     (* row 9)))

(defn print-puzzle [puzzle]
  (doseq [[col row] all-puzzle-coords]
    (cond  (= 0 col row) (print "[")
           (= 0 col) (print " "))
    (print (puzzle (puzzle-index col row)))
    (if-not (= 8 col) (print " "))
    (if  (= 8 col row) (print "]"))
    (if (= 8 col) (print "\n"))))

(defn puzzle-row [puzzle row]
  (let [rows (partition 9 puzzle)]
    (nth rows row)))

(defn puzzle-column [puzzle col]
  (for [row (range 0 9)]
    (puzzle (puzzle-index col row))))

(defn subgrid-coords [col row]
  (map (fn [n] (int (/ n 3)))
       [col row]))

(defn puzzle-subgrid [puzzle coords]
  (let [[subgrid-col subgrid-row] coords]
    (flatten
     (let [start-col (* 3 subgrid-col)
           start-row (* 3 subgrid-row)]
       (for [row (range start-row (+ 3 start-row))
             col (range start-col (+ 3 start-col))]
         (puzzle (puzzle-index col row)))))))

(defn possible-values [puzzle col row]
  "Get all the possible sudoku moves for a col and row index on a puzzle."
  {:pre (= 0 (puzzle (puzzle-index col row)))}
  (apply set/difference
         (map set [(range 1 10)
                   (puzzle-column puzzle col)
                   (puzzle-row puzzle row)
                   (puzzle-subgrid puzzle
                                   (subgrid-coords col row))])))

(defn potentially-solvable? [puzzle]
  "No empty slots that have no possible values?"
  (not-any?
   (fn [[col row]] (and (= 0 (puzzle (puzzle-index col row)))
                       (empty? (possible-values puzzle col row))))
   all-puzzle-coords))

(defn- get-slot-value [puzzle col row]
  "Returns current slot value if its already set. Otherwise, if there is only one possible value, return that. Otherwise, 0."
  (let [slot-value (puzzle (puzzle-index col row))]
    (cond (< 0 slot-value) slot-value
          :else (let [poss-values (possible-values puzzle col row)]
                  (case (count poss-values)
                    1 (first poss-values)
                    0)))))

(defn- next-coords [col row]
  (cond (= 8 col) [0 (inc row)]
        :else [(inc col) row]))

(defn- fill-obvious-pass [puzzle]
  "Takes a puzzle, returns the puzzle with all the empty slots with only 1 possible value filled."
  ;; This is very messy compared to an obvious iterative solution
  ;; (doseq and swap). But any pro functional way to do this?
  (loop [puzz puzzle [col row] [0 0]]
    (let [slot-value (puzzle (puzzle-index col row))
          on-last? (= 8 col row)]
      (cond (< 0 slot-value)
            (cond on-last? puzz
                  :else (recur puzz (next-coords col row)))
            :else (let [poss-values (possible-values puzz col row)
                        poss-values-count (count poss-values)]
                    (case poss-values-count
                      1 (let [new-puzz (assoc puzz (puzzle-index col row) (first poss-values))]
                          (cond on-last? new-puzz
                                :else (recur new-puzz (next-coords col row))))
                      (if on-last? puzz
                          (recur puzz (next-coords col row)))))))))

(defn puzzle-filled? [puzzle]
  (not-any? #(= 0 %) puzzle))

(defn fill-obvious [puzzle]
  {:pre [(not (puzzle-filled? puzzle))
         (potentially-solvable? puzzle)]
   :post [(= 81 (count %))]}
  "Runs fill-obvious-pass until there are no changes. Repetition needed for cases where some changes open up new obvious (only 1 possible value) slots."
  ;; Don't believe me? run (fill-obvious (sudoku-puzzles 0)). That
  ;; alone solves that puzzle.
  (loop [current-puzzle puzzle]
    (let [new-puzzle (fill-obvious-pass current-puzzle)]
      (cond (= current-puzzle new-puzzle) new-puzzle
            :else (recur new-puzzle)))))

(def all-subgrid-coords
  (into [] (for [subgrid-row (range 0 3)
                 subgrid-col (range 0 3)]
             [subgrid-col subgrid-row])))

(defn puzzle-complete? [puzzle]
  "Checks that all columns, rows, and subgrids have the legal sudoku values."
  (let [sudoku-values (range 1 10)]
    (and (every? (fn [col] (= sudoku-values
                             (sort (puzzle-column puzzle col))))
                 (range 0 9))
         (every? (fn [row] (= sudoku-values
                             (sort (puzzle-row puzzle row))))
                 (range 0 9))
         (every? (fn [subgrid-coords] (= sudoku-values
                                        (sort (puzzle-subgrid puzzle subgrid-coords))))
                 all-subgrid-coords))))

(defn- possible-values-map [puzzle]
  (into {} (for [row (range 0 9)
                 col (range 0 9)]
             (let [slot-value (puzzle (puzzle-index col row))]
               (if (> slot-value 0)
                 [[col row]  nil]
                 ;; (delay), because we won't neccessarily need the value.
                 [[col row] (delay (possible-values puzzle col row))])))))

;;;  Given a map of the possible values for a puzzle, pick one that it
;;;  would make sense to start guessing. It will be one with the
;;;  lowest number of possibilities, since that limits the total
;;;  number of guesses.
;;;
;;;  We start looking for the first one with 2 possibilities, because
;;;  we expect slots with only 1 possibility will be filled in
;;;  already.
(defn pick-slot-to-guess [poss-values-map]
  (loop [i 2]
    (let [[coords poss-values] (some (fn [[coords poss-values]]
                                       (and (= i (count (force poss-values)))
                                            [coords (force poss-values)]))
                                     poss-values-map)]
      (cond
       coords [coords poss-values]
       (> i 8) (explode  "Should never happen. Puzzle must have been screwed up somewhere.")
       :else (recur (inc i))))))

(defn guess-solve [puzzle fallback-solver]
  {:pre [(not (puzzle-filled? puzzle))
         (potentially-solvable? puzzle)]
   :post [(potentially-solvable? puzzle)]}
  (let [[[col row] vals-to-try] (pick-slot-to-guess (possible-values-map puzzle))]
    (loop [guess (first vals-to-try)
           remain-guesses (rest vals-to-try)]
      (let [guessed-puzzle (assoc puzzle (puzzle-index col row) guess)]
        (cond (puzzle-filled? guessed-puzzle)
              guessed-puzzle
              (potentially-solvable? guessed-puzzle)
              (or (fallback-solver guessed-puzzle)
                  (if (seq remain-guesses)
                    (recur (first remain-guesses) (rest remain-guesses)))))))))

(defn solve [puzzle]
  {:pre [(potentially-solvable? puzzle)]
   :post [(or (potentially-solvable? puzzle)
              (puzzle-complete? puzzle))]}
  (cond (puzzle-filled? puzzle)
        puzzle
        (potentially-solvable? puzzle)
        (let [puzzle-obv (fill-obvious puzzle)]
          (cond (puzzle-filled? puzzle-obv) puzzle-obv
                (potentially-solvable? puzzle-obv) (guess-solve puzzle-obv solve)))))

(defn solve-print [puzzle]
  (let [solve-attempt (solve puzzle)]
    (cond (puzzle-complete? solve-attempt) (do (println "Success:")
                                               (print-puzzle solve-attempt))
          :else (do (println "Fail. Orig:")
                    (print-puzzle puzzle)
                    (println "Attempt:")
                    (print-puzzle solve-attempt)))))

(defn solve-print-all []
  (map solve-print sudoku-puzzles))

;;; Uncomment this if running as a file
;;(solve-print-all)