(ns sudoku-clojure.test.core
  (:use [sudoku-clojure.core])
  (:use [clojure.test]))

(def test-puzzle
  (first sudoku-puzzles))

;       Cols 0 1 2 3 4 5 6 7 8
;          ...................
;     R  0 . 0 0 3 0 2 0 6 0 0
;     o  1 . 9 0 0 3 0 5 0 0 1
;     w  2 . 0 0 1 8 0 6 4 0 0
;     s  3 . 0 0 8 1 0 2 9 0 0
;        4 . 7 0 0 0 0 0 0 0 8
;        5 . 0 0 6 7 0 8 2 0 0
;        6 . 0 0 2 6 0 9 5 0 0
;        7 . 8 0 0 2 0 3 0 0 9
;        8 . 0 0 5 0 1 0 3 0 0

(deftest puzzle-row-test
  (is (= (puzzle-row test-puzzle 0) [ 0 0 3 0 2 0 6 0 0]))
  (is (= (puzzle-row test-puzzle 8) [ 0 0 5 0 1 0 3 0 0])))

(deftest puzzle-column-test
  (is (= (puzzle-column test-puzzle 0) [0 9 0 0 7 0 0 8 0]))
  (is (= (puzzle-column test-puzzle 8) [0 1 0 0 8 0 0 9 0])))

(deftest subgrid-coords-test
  (is (= (subgrid-coords 0 0)
         (subgrid-coords 1 1)
         (subgrid-coords 2 2)))
  (is (= (subgrid-coords 6 6)
         (subgrid-coords 6 7)
         (subgrid-coords 8 8)))
  (is (not (= (subgrid-coords 0 0)
              (subgrid-coords 3 3)))))

(deftest puzzle-subgrid-test
  (is (= (sort (puzzle-subgrid test-puzzle (subgrid-coords 0 0)))
         (sort [0 0 3 9 0 0 0 0 1]))
      "Sorting results, since we don't care and there is no 'natural' order for subgrids like there is for rows and columns.")
  (is (= (sort (puzzle-subgrid test-puzzle (subgrid-coords 8 8)))
         (sort [5 0 0 0 0 9 3 0 0]))))

(deftest possible-moves-test
  (is (= (possible-moves test-puzzle 0 0)
         #{4 5})))