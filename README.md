# sudoku-clojure

A clojure program able to solve all sudoku puzzles. Currently it has a list of 50 puzzles from Project Euler:
http://projecteuler.net/project/sudoku.txt

## Usage

Compile it in the REPL and type:

```clojure
(solve-print-all)
```
You can also inspect and solve puzzles interactively:

```clojure
;;Print puzzle 1/50 of the Project Euler sudoku puzzles
(print-puzzle (sudoku-puzzles 0))

;; Prints the solution to it
(print-puzzle (solve (sudoku-puzzles 0)))
```
If you don't have a REPL hooked up, uncomment the last line "(solve-print-all)" and run the program.

