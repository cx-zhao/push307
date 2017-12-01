;; Matt Goon and Candice Zhao
;; CS 307: Genetic Programming
;; Professor Helmuth
;; Term Project: Connect 4 GP System
;; Fall 2017
;;
;; This program implemented a full GP algorithm, using Push as the
;; representation language. The goal for the expanded GP system is to find
;; the program that "plays" or returns the next best move to win the
;; Connect 4 game.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns push307.core
  (:gen-class))

;;;;;;;;;;
;; Examples

;; An example Push state
(def example-push-state
  {:exec '()
   :integer '(2 3)
   :string '("***")
   :bool '()
   :game-state '([["***""***""***"]
                  ["***""ooo""***""***""***""***"]
                  ["ooo""ooo""ooo""ooo"]
                  ["ooo"]
                  []
                  []
                  ["***""ooo"]])
   :input {:in1 [[] [] [] [] [] [] []]
           :in2 "***"
           :in3 "ooo"}})

;; An example Push program
(def example-push-program
  '(integer-min check-col-top 0 3 5 integer_* "hello" 4 "world"))

;; An example individual in the population
;; Made of a map containing, at mimimum, a program, the errors for
;; the program, and a total error
(def example-individual
  {:program '(integer-min check-col-top 0 integer_> true 0)
   :errors []
   :total-error 0})

;;-----------------------------------
;; connect-4 game setup

;; An empty connect-4 game board. Each vector represents a column
;; of the game board.
(def empty-board
  [[][][][][][][]])

;; An example connect-4 game board.
;; The first element of a vector is at the bottom of a column, and
;; the last element of a vector is the top piece of a column.
(def example-board
  [["***"]
   ["***""ooo""***""***""***""***"]
   ["ooo""ooo""ooo""ooo"]
   []
   []
   []
   ["***""ooo"]])

(defn print-board
  "Takes a connect 4 game-board and print it. Returns nil"
  [board]
  (loop [index 5]
    (apply print (map #(get % index) board))
    (println)
    (if (< index 1)
      nil
      (recur (dec index)))))

(defn initialize-board
  "Returns an empty connect-4 game-board."
  []
  empty-board)

(defn check-availability
  "Takes board, a connect-4 game-board and col, a column number;
  returns true if the column is not full.
  Note that each column can take at most six discs."
  [board col]
  (< (count (get board (mod col 7))) 6))

(defn full-board
  "Returns true if the input game board is full."
  [board]
  (empty? (filter #(check-availability board %) (range 7))))

(defn next-avail-col
  "Takes a connect-4 game board and an integer column number;
  returns the column number of the next available column on the board.
  If the game-board is full, returns nil."
  [board col]
  (first (filter #(check-availability board %)
                 (map #(mod % 7) (range col (+ col 7))))))

(defn play-a-step
  "Takes a game board, a disc and a column number;
  if the column is available, place the disc at the column and returns the
  resulting game board; otherwise, place the disc at the next available
  column on the board and returns the game board.
  Note that if the game board is full, returns the board itself."
  [board disc col]
  (let [colnum (next-avail-col board col)]
    (if (= colnum nil)
      board
      (update board colnum #(conj % disc)))))

(defn check-vector
  "Takes a game-board and a column number.
  Returns a vector of all available vectors of four pieces,
  starting from the top of the game board."
  [board colnum]
  (let [col (get board colnum)
        index (dec (count col))]
    (cond-> []
      ;; check vertical
      (> index 2) (conj (subvec col (- (count col) 4)))
      ;; check horizontal to the left
      (> colnum 2) (conj (vec (map #(get (get board (- colnum %)) index)
                                   (range 4))))
      ;; check horizontal to the right
      (< colnum 4) (conj (vec (map #(get (get board (+ colnum %)) index)
                                   (range 4))))
      ;; check diagonal, to top right
      (and (< index 3) (< colnum 4))
      (conj (vec (map #(get (get board (+ colnum %))
                            (+ index %)) (range 4))))
      ;; check diagonal, to bottom left
      (and (> index 2) (> colnum 2))
      (conj (vec (map #(get (get board (- colnum %))
                            (- index %)) (range 4))))
      ;; check diagonal, to top left
      (and (< index 3) (> colnum 2))
      (conj (vec (map #(get (get board (- colnum %))
                            (+ index %)) (range 4))))
      ;; check diagonal, to bottom right
      (and (> index 2) (< colnum 4))
      (conj (vec (map #(get (get board (+ colnum %))
                            (- index %)) (range 4)))))))

(defn check-helper
  "Takes a game-board and a column number and a row number.
  Returns a vector of all available vectors of four pieces,
  starting at row rownum, column colnum on the board."
  [board colnum rownum]
  (let [col (get board colnum)
        index rownum]
    ;; (print-board board)
    (cond-> []
      ;; check vertical
      (> index 2) (conj (vec (map #(get col (- index  %)) (range 4))))
      ;; chexmapeck horizontal to the left
      (> colnum 2) (conj (vec (map #(get (get board (- colnum %)) index)
                                   (range 4))))
      ;; check horizontal to the right
      (< colnum 4) (conj (vec (map #(get (get board (+ colnum %)) index)
                                   (range 4))))
      ;; check diagonal, to top right
      (and (< index 3) (< colnum 4))
      (conj (vec (map #(get (get board (+ colnum %))
                            (+ index %)) (range 4))))
      ;; check diagonal, to bottom left
      (and (> index 2) (> colnum 2))
      (conj (vec (map #(get (get board (- colnum %))
                            (- index %)) (range 4))))
      ;; check diagonal, to top left
      (and (< index 3) (> colnum 2))
      (conj (vec (map #(get (get board (- colnum %))
                            (+ index %)) (range 4))))
      ;; check diagonal, to bottom right
      (and (> index 2) (< colnum 4))
      (conj (vec (map #(get (get board (+ colnum %))
                            (- index %)) (range 4)))))))

(defn check
  "Returns true if [disc disc disc disc] is a vector in the checklist."
  [checklist disc]
  (not= (count (filter #(= [disc disc disc disc] %) checklist)) 0))

(defn win-check
  "Returns true if player playing with disc wins."
  [board disc]
  (loop [index-num 0]
    (if (>= index-num 42)
      false
      (let [colnum (mod index-num 7)
            rownum (quot index-num 7)]
        (if (check (check-helper board colnum rownum) disc)
          true
          (recur (inc index-num)))))))

;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.
(def instructions
  (list
   'in1
   'in1
   'in1
   'in1
   'in2
   'in3
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   'integer_<
   'integer_=
   'integer_>
   'integer-dup
   'integer-from-boolean
   'integer-rand
   'integer-swap
   'integer-pop
   'integer-if
   'boolean_=
   'boolean-dup
   'boolean-from-integer
   'boolean-rand
   'boolean-swap
   'boolean-and
   'boolean-or
   'piece-rand
   'exec_=
   'exec-dup
   'exec-if
   'exec-swap
   'exec-while
   'check-col-top
   'check-col-top-2
   'get-a-piece
   'fake-step-win-checker
   'check-right
   'check-right-2
   'check-left
   'check-left-2
   'check-right-same
   'check-left-same
   'check-diag-left-bottom
   'check-diag-left-top
   'check-diag-right-top
   'check-diag-right-bottom
   'check-diag-left-bottom-2
   'check-diag-left-top-2
   'check-diag-right-top-2
   'check-diag-right-bottom-2
   'check-diag-left-bottom-same
   'check-diag-left-top-same
   'check-diag-right-top-same
   'check-diag-right-bottom-same
   0
   1
   true
   false
   ))

;;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :bool '()
   :game-state '()
   :input {}})


(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (assoc state stack (conj (state stack) item)))


(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (assoc state stack (rest (state stack))))


(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty? (state stack))
    :no-stack-item
    (first (state stack))))


(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (state stack)))


(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from.
  If there are enough args on each of the desired stacks,
  returns a map of the form {:state :args}, where
:state is the new state with args popped,
  and :args is a list of args from the stacks.
  If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))


(defn make-push-instruction
  "A utility function for making Push instructions.
  Takes a state, the function to apply to the args,
  the stacks to take the args from, and the stack to return the result to.
  Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))


(defn make-push-instruction-list
  "A utility function for making Push instructions.
  Takes a state, the function to apply to the args,
  the stacks to take the args from, and
  the list of stacks to return the result to.
  Applies the function to the args (taken from the stacks) and pushes
  the return values onto return-stacks in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (loop [result (apply function (:args args-pop-result))
             new-state (:state args-pop-result)
             return-list return-stack
             num-return (count return-list)]
        (if (zero? num-return)
          new-state
          (recur (rest result)
                 (push-to-stack new-state (first return-list)
                                (first result))
                 (rest return-list)
                 (dec num-return)))))))

;;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (push-to-stack state :exec ((state :input) :in1)))

(defn in2
  "Pushes the input labeled :in2 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (push-to-stack state :exec ((state :input) :in2)))

(defn in3
  "Pushes the input labeled :in3 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (push-to-stack state :exec ((state :input) :in3)))


(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

;;;; This is an example of what would be necessary to implement integer_+
;;;; without the useful helper function make-push-instruction.
;; (defn integer_+_without_helpers
;;   [state]
;;   (if (< (count (:integer state)) 2)
;;     state
;;     (let [arg1 (peek-stack state :integer)
;;           arg2 (peek-stack (pop-stack state :integer) :integer)
;;           popped-twice (pop-stack (pop-stack state :integer) :integer)]
;;       (push-to-stack popped-twice :integer (+' arg1 arg2)))))


(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the top integer on the stack should be subtracted from the second
  integer."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))


(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))


(defn protected-division
  "A protected division function.
  If the denominator is not 0, it acts like integer division;
  if the denominator is 0, it returns the numerator to avoid divide-by-zero
  errors."
  [numerator denominator]
  (if (zero? denominator)
    numerator
    (quot numerator denominator)))


(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time,
  but if the denominator is 0, it returns the numerator, to avoid
  divide-by-zero errors."
  [state]
  (make-push-instruction state protected-division
                         [:integer :integer]
                         :integer))

(defn integer_<_helper
  "Returns true if the second integer is less than the first integer."
  [num1 num2]
  (< num2 num1))

(defn integer_<
  "Pushes TRUE onto the BOOLEAN stack if the second item is less than the
  top item, or FALSE otherwise."
  [state]
  (make-push-instruction state integer_<_helper [:integer :integer] :bool))


(defn integer_=_helper
  "Returns true if the two integers are equal."
  [num1 num2]
  (== num2 num1))

(defn integer_=
  "Pushes TRUE onto the BOOLEAN stack if the top two items are equal,
  or FALSE otherwise."
  [state]
  (make-push-instruction state integer_=_helper [:integer :integer] :bool))


(defn integer_>_helper
  "Returns true if the second item is greater than the first item." 
  [num1 num2]
  (> num2 num1))

(defn integer_>
  "Pushes TRUE onto the BOOLEAN stack if the second item is greater
  than the top item, or FALSE otherwise."
  [state]
  (make-push-instruction state integer_>_helper [:integer :integer] :bool))


(defn integer-dup-helper
  "Duplicates the input item."
  [num]
  [num num])

(defn integer-dup
  "Duplicates the top item on the INTEGER stack."
  [state]
  (make-push-instruction-list state integer-dup-helper
                              [:integer]
                              [:integer :integer]))


(defn integer-from-boolean-helper
  "Returns 1 if input is true; otherwise, returns 0"
  [bool]
  (if bool
    1
    0))

(defn integer-from-boolean
  "Pushes 1 if the top BOOLEAN is TRUE, or 0 if the top BOOLEAN is FALSE."
  [state]
  (make-push-instruction state integer-from-boolean-helper [:bool] :integer))


(defn integer-max-helper
  "Returns the maximum of the two items."
  [num1 num2]
  (if (> num1 num2)
    num1
    num2))

(defn integer-max
  "Pushes the maximum of the top two items to the integer stack."
  [state]
  (make-push-instruction state integer-max-helper [:integer :integer] :integer))


(defn integer-min-helper
  "Returns the minimum of the two items."
  [num1 num2]
  (if (< num1 num2)
    num1
    num2))

(defn integer-min
  "Pushes the minimum of the top two items."
  [state]
  (make-push-instruction state integer-min-helper [:integer :integer] :integer))


(defn integer-pop-helper
  "Returns an empty list."
  [num]
  [])

(defn integer-pop
  "Pops an integer off of the top of the INTEGER stack."
  [state]
  (make-push-instruction-list state integer-pop-helper [:integer] []))


(def min-random-integer 0)
(def max-random-integer 7)

(defn integer-rand-helper
  "Returns a newly generated random integer that is greater than or
  equal to min-random-integer and less than or equal to
  max-random-integer."
  []
  (rand-nth (map #(+ % min-random-integer) (range (- max-random-integer min-random-integer)))))

(defn integer-rand
  "Pushes a newly generated random integer that is greater than or equal
  to min-random-integer and less than or equal to max-random-integer
  to the integer stack."
  [state]
  (make-push-instruction state integer-rand-helper [] :integer))


(defn integer-swap-helper
  "Swaps two inputs."
  [num1 num2]
  [num2 num1])

(defn integer-swap
  "Swaps the top two INTEGERs on the INTEGER stack."
  [state]
  (make-push-instruction-list state integer-swap-helper [:integer :integer] [:integer :integer]))


(defn boolean_=_helper
  "Returns TRUE if the two BOOLEANs are equal, or FALSE otherwise."
  [bool1 bool2]
  (= bool1 bool2))

(defn boolean_=
  "Pushes TRUE onto the BOOLEAN stack if the top two BOOLEANs are equal,
  or FALSE otherwise."
  [state]
  (make-push-instruction state boolean_=_helper [:bool :bool] :bool))


(defn boolean-and-helper
  "Returns the logical AND of the top two BOOLEANs."
  [bool1 bool2]
  (and bool1 bool2))

(defn boolean-and
  "Pushes the logical AND of the top two BOOLEANs on the bool stack."
  [state]
  (make-push-instruction state boolean-and-helper [:bool :bool] :bool))


(defn boolean-or-helper
  "Returns the logical OR of the top two BOOLEANs."
  [bool1 bool2]
  (or bool1 bool2))

(defn boolean-or
  "Pushes the logical OR of the top two BOOLEANs to the bool stack."
  [state]
  (make-push-instruction state boolean-and-helper [:bool :bool] :bool))


(defn boolean-dup-helper
  "Duplicates the input."
  [bool]
  [bool bool])

(defn boolean-dup
  "Duplicates the top item on the BOOLEAN stack."
  [state]
  (make-push-instruction-list state boolean-dup-helper
                              [:bool]
                              [:bool :bool]))


(defn boolean-from-integer-helper
  "Returns FALSE if the input is 0, or TRUE otherwise."
  [num]
  (if (== 0 num)
    false
    true))

(defn boolean-from-integer
  "Pushes FALSE if the top INTEGER is 0, or TRUE otherwise to the
  bool stack."
  [state]
  (make-push-instruction state boolean-from-integer-helper
                         [:integer]
                         :bool))


(defn boolean-rand-helper
  "Returns a random boolean"
  []
  (rand-nth [true false]))

(defn boolean-rand
  "Pushes a newly generated random boolean to the bool stack."
  [state]
  (make-push-instruction state boolean-rand-helper [] :bool))


(defn boolean-swap-helper
  "Swaps the two inputs."
  [bool1 bool2]
  [bool2 bool1])

(defn boolean-swap
  "Swaps the top two BOOLEANs on the BOOLEAN stack."
  [state]
  (make-push-instruction-list state boolean-swap-helper
                              [:bool :bool]
                              [:bool :bool]))


(defn integer-if-helper
  "Returns the first number if the input bool is true;
  returns the second number otherwise"
  [bool int1 int2]
  (if bool
    int1
    int2))

(defn integer-if
  "Make a basic if push instruction with ints."
  [state]
  (make-push-instruction state integer-if-helper
                         [:bool :integer :integer] :integer))


(defn exec_=_helper
  "Returns TRUE  if the two inputs are equal, or FALSE otherwise."
  [exec1 exec2]
  (= exec1 exec2))

(defn exec_=
  "Pushes TRUE onto the BOOLEAN stack if the top two items on the
  EXEC stack are equal, or FALSE otherwise."
  [state]
  (make-push-instruction state exec_=_helper [:exec :exec] :bool))


(defn exec-dup-helper
  "Duplicates the input."
  [exec]
  [exec exec])

(defn exec-dup
  "Duplicates the top item on the EXEC stack. Does not pop its argument."
  [state]
  (make-push-instruction-list state exec-dup-helper [:exec] [:exec :exec]))


(defn exec-if-helper
  "Returns the first exec statment if true; returns the second exec
  statement if false."
  [bool exec1 exec2]
  (if bool
    exec1
    exec2))

(defn exec-if
  "If the top item of the BOOLEAN stack is TRUE then this removes the second item on the EXEC stack, leaving the first item to be executed. If it is false then it removes the first item, leaving the second to be executed."
  [state]
  (make-push-instruction state exec-if-helper [:bool :exec :exec] :exec))


(defn exec-swap-helper
  "Swaps the two inputs."
  [exec1 exec2]
  [exec2 exec1])

(defn exec-swap
  "Swaps the top two items on the EXEC stack."
  [state]
  (make-push-instruction-list state exec-swap-helper [:exec :exec] [:exec :exec]))


(defn fill-in-blank
  "Returns the input state without making any changes.
  Helper function for exec-while."
  [state]
  state)

(defn exec-while-helper
  "Custom while function: does the exec function for integer times."
  [exec integer]
  (cond
    (<= integer 0) ['fill-in-blank 'fill-in-blank 'fill-in-blank
                    'fill-in-blank]
    (= integer 1) [exec 'fill-in-blank 'fill-in-blank 'fill-in-blank]
    :ELSE [exec 'exec-while (dec integer) exec]))

(defn exec-while
  "Custom while function: does the exec function for integer times."
  [state]
  (make-push-instruction-list state exec-while-helper
                              [:exec :integer]
                              [:exec :exec :exec :exec]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized instructions for connect-4 game

(defn piece-rand-helper
  "Returns a random game piece."
  []
  (rand-nth ["***" "ooo"]))

(defn piece-rand
  "Pushes a random game piece onto the string stack."
  [state]
  (make-push-instruction state boolean-rand-helper [] :string))


(defn check-col-top-helper
  "Returns true if the top piece in the column is the same as string,
  else returns false otherwise.
  If the column is empty, return false."
  [game-board num string]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)]
      (if (empty? col)
        [false game-board]
        [(= string (get col (dec (count col)))) game-board]))))

(defn check-col-top
  "Pushes true onto the boolean stack if the top piece in the column is
  the same as the top item on the string stack; Otherwise,
  pushes false to the boolean stack.
  Note that the function also pushes the input game board back to the
  game-state stack"
  [state]
  (make-push-instruction-list state check-col-top-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-col-top-2-helper
  "Returns true if the top two pieces in the column are the same as string;
  returns false otherwise.
  If the column is empty, return false."
  [game-board num string]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)]
      (cond
        (> 2 (count col)) [false game-board]
        :ELSE [(and (= string (get col (dec (count col))))
                    (= string (get col (- (count col) 2)))) game-board]))))

(defn check-col-top-2
  "Pushes true onto the boolean stack if the top two pieces in the column
  are the same as the top item on the string stack; otherwise,
  pushes false to the boolean stack.
  Note that the function also pushes the input game board back to the
  game-state stack"
  [state]
  (make-push-instruction-list state check-col-top-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-left-helper
  "Returns true if the piece to the left of the top piece in the column
  is the same as the input string; returns false otherwise."
  [game-board num string]
  (cond
    (full-board game-board) [false game-board]
    (= string "") [false game-board]
    (zero? (next-avail-col game-board num)) [false game-board]
    :else (let [colnum (next-avail-col game-board num)
                col (get game-board colnum)
                rownum (count col)
                left-piece (get (get game-board (dec colnum)) rownum)]
            [(= left-piece string) game-board])))

(defn check-left
  "Pushes true onto the boolean stack if the piece to the left of the
  top piece the top is the same as the top item on the string stack;
  otherwise, pushes false to the boolean stack.
  Note that the function also pushes the input game board back to the
  game-state stack"
  [state]
  (make-push-instruction-list state check-left-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-left-2-helper
  "Returns true if two pieces to the left of the top piece in the column
  are the same as the input string; returns false otherwise."
  [game-board num string]
  (cond
    (full-board game-board) [false game-board]
    (= string "") [false game-board]
    (> 2 (next-avail-col game-board num)) [false game-board]
    :else (let [colnum (next-avail-col game-board num)
                col (get game-board colnum)
                rownum (count col)
                left-col-1 (get game-board (dec colnum))
                left-col-2 (get game-board (- colnum 2))
                left-piece-1 (get left-col-1 rownum)
                left-piece-2 (get left-col-2 rownum)]
            [(and (= left-piece-1 string) (= left-piece-2 string))
             game-board])))

(defn check-left-2
  "Pushes true onto the boolean stack if two pieces to the left of the
  top piece the top are the same as the top item on the string stack;
  otherwise, pushes false to the boolean stack.
  Note that the function also pushes the input game board back to the
  game-state stack"
  [state]
  (make-push-instruction-list state check-left-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-right-helper
  "Returns true if the piece to the right of the top piece in the column
  is the same as the input string; returns false otherwise."
  [game-board num string]
  (cond
    (full-board game-board) [false game-board]
    (= string "") [false game-board]
    (= 6 (next-avail-col game-board num)) [false game-board]
    :else (let [colnum (next-avail-col game-board num)
                col (get game-board colnum)
                rownum (count col)
                left-piece (get (get game-board (inc colnum)) rownum)]
            [(= left-piece string) game-board])))

(defn check-right
  "Pushes true onto the boolean stack if the piece to the right of the
  top piece the top is the same as the top item on the string stack;
  otherwise, pushes false to the boolean stack.
  Note that the function also pushes the input game board back to the
  game-state stack"
  [state]
  (make-push-instruction-list state check-right-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-right-2-helper
  "Returns true if two pieces to the left of the top piece in the column
  are the same as the input string; returns false otherwise."
  [game-board num string]
  (cond
    (full-board game-board) [false game-board]
    (= string "") [false game-board]
    (< 4 (next-avail-col game-board num)) [false game-board]
    :else (let [colnum (next-avail-col game-board num)
                col (get game-board colnum)
                rownum (count col)
                right-col-1 (get game-board (inc colnum))
                right-col-2 (get game-board (+ colnum 2))
                right-piece-1 (get right-col-1 rownum)
                right-piece-2 (get right-col-2 rownum)]
            [(and (= right-piece-1 string) (= right-piece-2 string))
             game-board])))

(defn check-right-2
  "Pushes true onto the boolean stack if two pieces to the right of the
  top piece the top are the same as the top item on the string stack;
  otherwise, pushes false to the boolean stack.
  Note that the function also pushes the input game board back to the
  game-state stack"
  [state]
  (make-push-instruction-list state check-right-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-right-bottom-helper
  "Check the piece along bottom right diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (= colnum 6) (= rownum 0)) [false game-board]
        :ELSE (let [disc (get (get game-board (inc colnum)) (dec rownum))]
                [(= piece disc) game-board])))))

(defn check-diag-right-bottom
  "Check the piece along bottom right diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-right-bottom-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-right-top-helper
  "Check the piece along top right diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (= colnum 6) (= rownum 5)) [false game-board]
        :ELSE (let [disc (get (get game-board (inc colnum)) (inc rownum))]
                [(= piece disc) game-board])))))

(defn check-diag-right-top
  "Check the piece along top right diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-right-top-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-left-bottom-helper
  "Check the piece along bottom left diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (= colnum 0) (= rownum 0)) [false game-board]
        :ELSE (let [disc (get (get game-board (dec colnum)) (dec rownum))]
                [(= piece disc) game-board])))))

(defn check-diag-left-bottom
  "Check the piece along bottom left diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-left-bottom-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-left-top-helper
  "Check the piece along top left diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (= colnum 0) (= rownum 5)) [false game-board]
        :ELSE (let [disc (get (get game-board (dec colnum)) (inc rownum))]
                [(= piece disc) game-board])))))

(defn check-diag-left-top
  "Check the piece along top left diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-left-top-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-right-bottom-2-helper
  "Check two pieces along bottom right diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (> colnum 4) (< rownum 2)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (inc colnum)) (dec rownum))
                    disc2 (get (get game-board (+ colnum 2)) (- rownum 2))]
                [(and (= piece disc1) (= piece disc2)) game-board])))))

(defn check-diag-right-bottom-2
  "Check two pieces along bottom right diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-right-bottom-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-right-top-2-helper
  "Check two pieces along top right diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (> colnum 4) (> rownum 3)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (inc colnum)) (inc rownum))
                    disc2 (get (get game-board (+ colnum 2)) (+ rownum 2))]
                [(and (= piece disc1) (= piece disc2)) game-board])))))

(defn check-diag-right-top-2
  "Check two pieces along top right diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-right-top-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-left-bottom-2-helper
  "Check two pieces along bottom left diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (< colnum 2) (< rownum 2)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (dec colnum)) (dec rownum))
                    disc2 (get (get game-board (- colnum 2)) (- rownum 2))]
                [(and (= piece disc1) (= piece disc2)) game-board])))))

(defn check-diag-left-bottom-2
  "Check two pieces along bottom left diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-left-bottom-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-left-top-2-helper
  "Check two pieces along top left diagonal."
  [game-board num piece]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (< colnum 2) (> rownum 3)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (dec colnum)) (inc rownum))
                    disc2 (get (get game-board (- colnum 2)) (+ rownum 2))]
                [(and (= piece disc1) (= piece disc2)) game-board])))))

(defn check-diag-left-top-2
  "Check two pieces along top left diagonal.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-left-top-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-diag-right-bottom-same-helper
  "Returns whether the two pieces along bottom right diagonal
  are the same."
  [game-board num]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (> colnum 4) (< rownum 2)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (inc colnum)) (dec rownum))
                    disc2 (get (get game-board (+ colnum 2)) (- rownum 2))]
                [(= disc1 disc2) game-board])))))

(defn check-diag-right-bottom-same
  "Checks if the two pieces along bottom right diagonal are the same.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-right-bottom-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))


(defn check-diag-right-top-same-helper
  "Returns whether the two pieces along top right diagonal
  are the same."
  [game-board num]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (> colnum 4) (> rownum 3)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (inc colnum)) (inc rownum))
                    disc2 (get (get game-board (+ colnum 2)) (+ rownum 2))]
                [(= disc1 disc2) game-board])))))

(defn check-diag-right-top-same
  "Checks if the two pieces along top right diagonal are the same.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-right-top-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))


(defn check-diag-left-bottom-same-helper
  "Returns whether the two pieces along bottom left diagonal
  are the same."
  [game-board num]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (< colnum 2) (< rownum 2)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (dec colnum)) (dec rownum))
                    disc2 (get (get game-board (- colnum 2)) (- rownum 2))]
                [(= disc1 disc2) game-board])))))

(defn check-diag-left-bottom-same
  "Checks if the two pieces along bottom left diagonal are the same.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-left-bottom-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))


(defn check-diag-left-top-same-helper
  "Returns whether the two pieces along top left diagonal
  are the same."
  [game-board num]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)
          rownum (count col)]
      (cond
        (or (< colnum 2) (> rownum 3)) [false game-board]
        :ELSE (let [disc1 (get (get game-board (dec colnum)) (inc rownum))
                    disc2 (get (get game-board (- colnum 2)) (+ rownum 2))]
                [(= disc1 disc2) game-board])))))

(defn check-diag-left-top-same
  "Checks if the two pieces along top left diagonal are the same.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-diag-left-top-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))


(defn check-right-same-helper
  "Returns whether the two pieces to the right are the same"
  [game-board num]
  (cond
    (full-board game-board) [false game-board]
    (< 4 (next-avail-col game-board num)) [false game-board]
    :else (let [colnum (next-avail-col game-board num)
                col (get game-board colnum)
                rownum (count col)
                right-col-1 (get game-board (inc colnum))
                right-col-2 (get game-board (+ colnum 2))
                right-piece-1 (get right-col-1 rownum)
                right-piece-2 (get right-col-2 rownum)]
            [(= right-piece-1 right-piece-2) game-board])))

(defn check-right-same
  "Checks if the two pieces to the right are the same.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-right-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))


(defn check-left-same-helper
  "Returns whether the two pieces to the left are the same"
  [game-board num]
  (cond
    (full-board game-board) [false game-board]
    (> 2 (next-avail-col game-board num)) [false game-board]
    :else (let [colnum (next-avail-col game-board num)
                col (get game-board colnum)
                rownum (count col)
                left-col-1 (get game-board (dec colnum))
                left-col-2 (get game-board (- colnum 2))
                left-piece-1 (get left-col-1 rownum)
                left-piece-2 (get left-col-2 rownum)]
            [(= left-piece-1 left-piece-2) game-board])))

(defn check-left-same
  "Checks if the two pieces to the left are the same.
   Note that the function also pushes the input game board back to the
   game-state stack"
  [state]
  (make-push-instruction-list state check-left-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))


(defn get-a-piece-helper
  "Returns the piece at column c, row r on the board.
  If there is not a piece at the location, return an empty string."
  [game-board c r]
  (let [col (get game-board (mod c 7))
        piece (get col (mod r 6))]
    (if (= nil piece)
      ""
      [piece game-board])))


(defn get-a-piece
  "Pushes the piece at column c, row r on the board.
  If there is not a piece at the location, pushes an empty string."
  [state]
  (make-push-instruction-list state get-a-piece-helper [:game-state :integer :integer] [:string :game-state]))


(defn fake-step-win-checker-helper
  "Returns true if place a piece at column c will lead to a win"
  [game-board c]
  (if (full-board game-board)
    [c false game-board]
    (let [colnum (next-avail-col game-board c)
          board-o (play-a-step game-board  "ooo" colnum)
          board-* (play-a-step game-board  "***" colnum)
          checklist-o (check-vector board-o colnum)
          checklist-* (check-vector board-* colnum)]
      [c (or (check checklist-o "ooo")
             (check checklist-* "***")) game-board])))

(defn fake-step-win-checker
  "Pushes true if place a piece at the column of the top item
  on the integer stack  will lead to a win"
  [state]
  (make-push-instruction-list state fake-step-win-checker-helper
                              [:game-state :integer]
                              [:integer :bool :game-state]))

 ;;;;;;;;;;
;; Interpreter
(defn interpret-one-step
  "A helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  ;; next-instr is the next instruction on the exec stack, and
  ;; state is the state after poping the next instruction from the exec stack
  (let [next-instr (peek-stack push-state :exec)
        state (pop-stack push-state :exec)]
    (cond
      ;; If the exec stack is empty, do nothing.
      (= :no-stack-item next-instr) state
      (vector? next-instr) (push-to-stack state :game-state next-instr)
      (integer? next-instr) (push-to-stack state :integer next-instr)
      (string? next-instr) (push-to-stack state :string next-instr)
      (= (type next-instr) (type true)) (push-to-stack state :boolean next-instr)
      :ELSE ((eval next-instr) state))))


(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  ;; Pushes all elements in the program to the exec stack of the start-state.
  ;; Does a tail recursion on interpret the next instruction on the exec stack
  ;; until the exec stack is empty.
  (loop [state (assoc start-state
                      :exec
                      (concat program (start-state :exec)))
         num-of-evaluations 0]
    (cond
      (empty-stack? state :exec) state
      ;; add a max. number of evaluations for each push program
      ;; (avoid infinite loops)
      (> num-of-evaluations 200) state
      :ELSE (recur (interpret-one-step state)
                   (inc num-of-evaluations)))))



(defn switch-player
  "Returns a disc that is different from player."
  [player p1 p2]
  (if (= player p1)
    p2
    p1))


(defn switching-forward
  "Returns individual1 if the current player is ***;
  returns individual2 if the current player is ooo"
  [player individual1 individual2]
  (if (= player "ooo")
    individual2
    individual1))

(defn error-eval-forward
  "Takes two individuals as  inputs.
  Individual1 playe first using piece ooo,
  and inividual2 plays second using piece ***.
  Returns 0 if individual1 wins the game.
  Returns 1000 if individual 1 does not generate an integer outpu.
  If individual2 wins the game, returns
  100-#(pieces on the board)."
  
  [individual1 individual2]
  
  ;; init-state is an empty push state with :in1 = input value.
  ;; result-state is the state after interpreting the push program of the
  ;; individual over the init-state
  ;; output is the integer on top of the integer stack in the result state.
  (loop [init-board empty-board
         player "ooo"
         init-state (assoc empty-push-state
                           :input {:in1 init-board :in2 "ooo" :in3 "***"}
                           :game-state (list init-board))
         result-state (interpret-push-program (individual1 :program)
                                              init-state)
         step (peek-stack result-state :integer)
         round 0]

    ;; if the game-board is full, return 1, fail to win
    (if (full-board init-board)
      1
      ;; If current individual does not generate a valid output...
      (if (= step :no-stack-item)
        ;; If the current individual is individual1,
        ;; returns a penalty error of 1000.
        (if (= player "ooo")
          1000
          ;; If individual2 does not have integer outputs,
          ;; choose a random step for it and continue the game.
          (let [step (rand-int 7)
                colnum (next-avail-col init-board step)
                game-board (play-a-step init-board player step)
                init-state-2 (assoc empty-push-state
                                    :input {:in1 game-board
                                            :in2 "ooo"
                                            :in3 "***"}
                                    :game-state (list game-board))
                result-state-2 (interpret-push-program
                                ((switching-forward player
                                                    individual1
                                                    individual2) :program)
                                init-state-2)]
            ;; If individual1 wins, returns 0.
            ;; If individual2 wins, returns 100 - # of pieces on the board.
            (cond (and (win-check game-board player) (= player "ooo")) 0
                  (and (win-check game-board player) (= player "***")) (- 100 round)
                  :ELSE (recur game-board
                               "ooo"
                               init-state-2
                               result-state-2
                               (peek-stack result-state-2 :integer)
                               (inc round)))))

        ;; When the output is valid, players the game,
        ;; and changes the player
        (let [colnum (next-avail-col init-board step)
              game-board (play-a-step init-board player step)
              init-state-2 (assoc empty-push-state
                                  :input {:in1 game-board
                                          :in2 (switch-player player "ooo" "***")
                                          :in3 player}
                                  :game-state (list game-board))
              result-state-2 (interpret-push-program
                              ((switching-forward player individual1 individual2) :program)
                              init-state-2)]
          ;; If individual1 wins, returns 0.
          ;; If individual2 wins, returns 100 - # of pieces on the board.
          (cond (and (win-check game-board player) (= player "ooo")) 0
                (and (win-check game-board player) (= player "***")) (- 100 round)
                :ELSE (recur game-board
                             (switch-player player "ooo" "***")
                             init-state-2
                             result-state-2
                             (peek-stack result-state-2 :integer)
                             (inc round))))))))


(defn switching-backward
  "Returns individual2 if the current player is ***;
  returns individual1 if the current player is ooo"
  [player individual1 individual2]
  (if (= player "***")
    individual2
    individual1))

(defn error-eval-backward
  "Takes two individuals as inputs.
  Individual2 playe first using piece ooo
  and inividual1 plays second using piece ***.
  Returns 0 if individual1 wins the game.
  Returns 1000 if individual 1 does not generate an integer outpu.
  If individual2 wins the game, returns
  100-#(pieces on the board)."

  [individual1 individual2]
  ;; init-state is an empty push state with :in1 = input value.
  ;; result-state is the state after interpreting the push program of the
  ;; individual over the init-state
  ;; output is the integer on top of the integer stack in the result state.

  (loop [init-board empty-board
         player "ooo"
         init-state (assoc empty-push-state
                           :input {:in1 init-board :in2 "ooo" :in3 "***"}
                           :game-state (list init-board))
         result-state (interpret-push-program (individual2 :program)
                                              init-state)
         step (peek-stack result-state :integer)
         round 0]

    ;; if the game-board is full, return 1, fail to win
    (if (full-board init-board)
      1
      ;; If current individual does not generate a valid output...
      (if (= step :no-stack-item)
        ;; If the current individual is individual1,
        ;; returns a penalty error of 1000.
        (if (= player "***")
          1000
          ;; If individual2 does not have integer outputs,
          ;; choose a random step for it and continue the game.
          (let [step (rand-int 7)
                colnum (next-avail-col init-board step)
                game-board (play-a-step init-board player step)
                init-state-2 (assoc empty-push-state
                                    :input {:in1 game-board
                                            :in2 "***"
                                            :in3 "ooo"}
                                    :game-state (list game-board))
                result-state-2 (interpret-push-program
                                ((switching-backward player individual1 individual2) :program)
                                init-state-2)]
            ;; If individual1 wins, returns 0.
            ;; If individual2 wins, returns 100 - # of pieces on the board.
            (cond (and (win-check game-board player) (= player "***")) 0
                  (and (win-check game-board player) (= player "ooo")) (- 100 round)
                  :ELSE (recur game-board
                               "***"
                               init-state-2
                               result-state-2
                               (peek-stack result-state-2 :integer)
                               (inc round)))))
        
        ;; When the output is valid, players the game,
        ;; and changes the player
        (let [colnum (next-avail-col init-board step)
              game-board (play-a-step init-board player step)
              init-state-2 (assoc empty-push-state
                                  :input {:in1 game-board
                                          :in2 (switch-player player "ooo" "***")
                                          :in3 player}
                                  :game-state (list game-board))
              result-state-2 (interpret-push-program
                              ((switching-backward player individual1 individual2) :program)
                              init-state-2)]

          ;; If individual1 wins, returns 0.
          ;; If individual2 wins, returns 100 - # of pieces on the board.
          (cond (and (win-check game-board player) (= player "***")) 0
                (and (win-check game-board player) (= player "ooo")) (- 100 round)
                :ELSE (recur game-board
                             (switch-player player "ooo" "***")
                             init-state-2
                             result-state-2
                             (peek-stack result-state-2 :integer)
                             (inc round))))))))


;;;;;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Takes a list of instructions and a maximum initial program size.
  Creates and returns a new program with a length between 1 and
  of max-initial-program-size."
  [instructions max-initial-program-size]
  (take (inc (rand-int max-initial-program-size))
        (repeatedly #(rand-nth instructions))))


(defn make-random-individual
  "Takes a list of instructions and a maximum initial program size.
  Creates and returns a new individual with a random push program.
  Sets the default error vector to be [] and total-error to be 0."
  [instructions max-initial-program-size]
  {:program (make-random-push-program instructions max-initial-program-size)
   :errors []
   :total-error 0})


(defn tournament-selection
  "Selects an individual from the population using a tournament of size 5.
  Returned individual will be a parent in the next generation."
  [population]
  ;; Takes five individuals from the population randomly.
  ;; Does a tail recursion on comparing the total-error of two individuals;
  ;; winner will be the individual with the lowest total-error so far.
  (loop [tourn-lst (take 5 (repeatedly #(rand-nth population)))
         winner (rand-nth tourn-lst)]
    (if (empty? tourn-lst)
      winner
      (recur (rest tourn-lst)
             (if (< ((first tourn-lst) :total-error) (winner :total-error))
               (first tourn-lst)
               winner)))))


;; Note that this introduces a new parent selection method.
;; CITATION: Using the algorithm of lexicase selection from class note,
;; combining it with the the tournament selection.
(defn lexicase-tournament-selection
  "Selects an individual from the candidates using lexicase selection
  over the input case-list. Selects the individual from the final
  candidates list using a tournament of size 5.
  tournament of size 5. 
  Returned individual will be a parent in the next generation."
  [population case-list]

  (loop [candidates population
         cases case-list]
    (let [cand-list
          (filter #(zero? (nth (% :errors) (mod (first cases) 100))) candidates)]
      (cond
        (empty? cand-list) (tournament-selection candidates)
        (= 1 (count cases)) (rand-nth cand-list)
        :ELSE (recur cand-list
                     (rest cases))))))


;; Note that this introduces a new parent selection method.
;; CITATION: Using the algorithm of lexicase selection from class note,
;; combining it with the the tournament selection.
(defn tournament-lexicase-selection
  "First, generates a candidates list from the population using a
  tournament of size 5. 
  Selects an individual from the candidates using lexicase selection
  over the input case-list.
  Returned individual will be a parent in the next generation."
  [population case-list]

  (loop [candidates (take 5 (repeatedly #(tournament-selection population)))
         cases case-list]
    (let [cand-list
          (filter #(zero? (nth (% :errors) (mod (first cases) 100))) candidates)]
      (cond
        (empty? cand-list) (rand-nth candidates)
        (= 1 (count cases)) (rand-nth cand-list)
        :ELSE (recur cand-list
                     (rest cases))))))


(defn mutation-rate
  "Returns true at a 5% chance and returns false at a 95% chance."
  ([]
   (= (rand-int 20) 0))
  ([x]
   (= (rand-int 20) 0)))


(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns a child program generated from two input parent programs.
  For each common index of the parents, select an element from one parent or
  the other uniformly at random to add to the returning child program.
  For each leftover index of the longer parent program, the element will
  have a 50% chance of being added to the child program."
  [prog-a prog-b]
  ;; Creates two longer lists that start with prog-a and prog-b respectively
  ;; and fill the rest of the list with the string 'TOBEREMOVED' to make sure
  ;; that parent1 and parent2 have the same size.
  (let [parent1 (concat prog-a (repeat (count prog-b) "TOBEREMOVED"))
        parent2 (concat prog-b (repeat (count prog-a) "TOBEREMOVED"))
        size (max (count prog-a) (count prog-b))]
    ;; Then does a uniformly random selection over parent1 and parent2 over
    ;; each index of the longer list.
    ;; If an element on the leftover part of the longer parent is not selected,
    ;; 'TOBOREMOVED' will be filled in for that index. The function will
    ;; remove all the 'ROBEREMOVED' from the child program before returning.
    (filter #(not= % "TOBEREMOVED")
            (map #(rand-nth (list (nth parent1 %) (nth parent2 %)))
                 (range size)))))


(defn uniform-addition
  "Takes a program and randomly inserts new instructions into the program,
  with 5% chance of a new instruction being added before every instruction
  and at the end of the program. Returns child program."
  [prog]
  ;; pre stores the inserted program so far.
  ;; post stores the program that has not yet been processed.
  ;; Note that mutation-rate returns true at a 5% chance.
  (loop [pre '()
         post prog]
    (if (empty? post)
      (if (mutation-rate)
        (concat pre (list (rand-nth instructions)))
        pre)
      (if (mutation-rate)
        (recur (concat pre
                       (list (rand-nth instructions) (first post)))
               (rest post))
        (recur (concat pre (list (first post)))
               (rest post))))))


(defn uniform-deletion
  "Removes zero or more instructions from a program. Each instruction
  has a 5% chance of being removed. Returns child program."
  [prog]
  (filter #(not (mutation-rate %)) prog))


(defn rand-list-generator
  "Returns a list of x randomly generated numbers less than the max-value."
  [x max-value]
  (map (fn [%] (rand-int max-value)) (range x)))


(defn select-and-vary
  "Selects parent(s) from population using tournament selection of size 5
  and varies them, returning a child individual (note: not program).
  Chooses which genetic operator to use probabilistically.
  Gives 20% chance to crossover using tournament-lexicase-selection,
  20% chance to crossover using lexicase-tournament-selection,
  20% chance to crossover using regular tournament-selection,
  20% to uniform-addition,
  and 20% to uniform-deletion."
  [population]
  ;; cases is an index list for lexicase seleciton
  ;; reverse case is the reverse list of the case list.
  (let [genetic-chance (rand-int 5)
        cases (rand-list-generator 50 100)
        reverse-cases (reverse cases)]
    ;; Note that the default setting for the returned child individual
    ;; will have a error vector of [] and a total-error of 0.
    (cond
      (= genetic-chance 1) {:program (uniform-addition ((tournament-selection population) :program)),
                            :errors [],
                            :total-error 0}
      
      (= genetic-chance 2) {:program (uniform-deletion ((tournament-selection population) :program)),
                            :errors [],
                            :total-error 0}
      ;; select parent using lexicase selection over cases and reverse cases will hopefully improve the
      ;; diversity of the population.
      (= genetic-chance 3) {:program (crossover ((tournament-lexicase-selection population cases) :program)
                                                ((tournament-lexicase-selection population reverse-cases) :program)),
                            :errors [],
                            :total-error 0}
      
      (= genetic-chance 4)  {:program (crossover ((lexicase-tournament-selection population cases) :program)
                                                 ((lexicase-tournament-selection population reverse-cases) :program)),
                             :errors [],
                             :total-error 0}
      
      :ELSE  {:program (crossover ((tournament-selection population) :program)
                                  ((tournament-selection population) :program)),
              :errors [],
              :total-error 0})))



(defn best-tournament-selection
  "Selects an individual from the population using a tournament. Returned
  individual will have the lowest total-error in the population."
  [population]
  ;; Does on tail recursion on comparing the total-error of
  ;; two individuals from the population.
  ;; winner will be the individual with the lowest total-error so far.
  (loop [tourn-lst population
         winner (rand-nth tourn-lst)]
    (if (empty? tourn-lst)
      winner
      (recur (rest tourn-lst)
             (if (< ((first tourn-lst) :total-error) (winner :total-error))
               (first tourn-lst)
               winner)))))


(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

  -------------------------------------------------------
  Report for Generation 3
  -------------------------------------------------------
  Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
  Best program size: 33
  Best total error: 727
  Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)

  Returns the best program in the population. "
  [population generation]
  (let [best-program (best-tournament-selection population)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation ", generation)
    (println "-------------------------------------------------------")
    (println "Best program: " (best-program :program))
    (println "Best program size: " (count (best-program :program)))
    (println "Best total errors: " (best-program :total-error))
    (println "Best errors: " (best-program :errors))
    best-program))


(defn initialize-population
  "Takes a population-size, a list of instructions,
  , a maximum initial program size and an error function.
  Generates a population of randomly generated individuals.
  Then, evaluates the population by the input error function.
  Returns the evaluated population."
  [population-size instructions max-initial-program-size error-function]
  ;; Evaluates each individual over the error function.
  ;; Generates a population randomly.
  (let [population
        (take population-size
              (repeatedly #(make-random-individual instructions
                                                   max-initial-program-size)))]
    (map #(error-function % population) population)))


(defn generate-new-population
  "Takes a population, a population-size, and an error function to evaluate.
  Repeatedly generates an individual from the input population with
  60% chance to crossover, 20% to uniform-addition and 20% to uniform-deletion,
  and evaluates the individuals using error-function.
  Returns the evaluated new population."
  [population population-size error-function]
  (take population-size
        (repeatedly #(error-function (select-and-vary population) population))))


(defn push-gp
  "Main GP loop. Initializes with two populations, and then repeatedly
   generates and evaluates new populations. Stops if it finds an
   individual with 0 error (and should return :SUCCESS, or if it
   exceeds the maximum generations (and should return nil). Should print
   report each generation.
   --
   The only argument should be a map containing the core parameters to
   push-gp. The format given below will decompose this map into individual
   arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-program-size (max size of randomly generated programs)"
  [{:keys [population-size max-generations error-function
           instructions max-initial-program-size]}]
  ;; Starts with initializing and evaluating a population.
  ;; Does a tail recursion on generating and evaluating new populations
  ;; from the old generation. Prints out a report for each generation.
  ;; Note that the report function prints a report and
  ;; returns an individual with best total-error in the population.
  (loop [initial-population (initialize-population population-size
                                                   instructions
                                                   max-initial-program-size
                                                   error-function)
         initial-population-2 (initialize-population population-size
                                                     instructions
                                                     max-initial-program-size
                                                     error-function)
         generation 0
         best-prog (report initial-population generation)
         best-prog-2 (report initial-population-2 generation)]
    ;; Stops if it finds an individual with a total error of 0 in
    ;; one of the populations (returns :SUCCESS)
    ;; or if the program exceeds the maximum generation (returns nil).
    (cond
      (or (= (best-prog :total-error) 0) (= (best-prog-2 :total-error) 0)) :SUCCESS
      (= generation max-generations) nil
      ;; Exchanges three individuals from the populations to each other
      ;; for each generation.
      :ELSE (let [rand-subpop (map #(nth initial-population %)
                                   (rand-list-generator 3 population-size))
                  rand-subpop-2 (map #(nth initial-population-2 %)
                                     (rand-list-generator 3 population-size))
                  new-population (generate-new-population (concat initial-population
                                                                  rand-subpop-2)
                                                          population-size
                                                          error-function)
                  new-population-2 (generate-new-population (concat initial-population-2
                                                                    rand-subpop)
                                                            population-size
                                                            error-function)]
              (recur new-population
                     new-population-2
                     (inc generation)
                     (report new-population (inc generation))
                     (report new-population-2 (inc generation)))))))

;;;;;;;;;;;;;;;;;;;
;; helper push instructions for advanced test cases, but not for push gp.

(defn advanced-player-helper
  "Returns a column number that will lead to a win starting from that
  position. If there is not one, returns a random integer."
  [game-board]
  (loop [num 0]
    (cond
      (> num 6) (rand-int 7)
      (get (fake-step-win-checker-helper game-board num) 1) num
      :ELSE (recur (inc num)))))

(defn advanced-player
  "Takes a game state, returns a number that suggests the game strategy
  using advancec-player-helper function."
  [state]
  (make-push-instruction state advanced-player-helper
                         [:game-state] :integer))


(defn ultimate-win-piece-checker-helper
  "Returns a column number that will lead to a win of the given
  game-piece immediately.
  If there is not one, returns a random integer."
  [game-board game-piece]
  (loop [col 0]
    (cond
      (> col 6) (rand-int 7)
      (win-check (play-a-step game-board game-piece col) game-piece) col
      :ELSE (recur (inc col)))))

(defn ultimate-win-piece-checker
  "Takes a game state and a string,
  returns a number that suggests the game strategy
  using ultimate-win-piece-checker-helper function."
  [state]
  (make-push-instruction state ultimate-win-piece-checker-helper [:game-state :string] :integer))


(defn ultimate-win-checker-helper
  "Returns a column number that will lead to a win by any player immediately.
  If there is not one, returns a random integer."
  [game-board]
  (loop [col 0]
    (cond
      (> col 6) (rand-int 7)
      (win-check (play-a-step game-board "ooo" col) "ooo") col
      (win-check (play-a-step game-board "***" col) "***") col
      :ELSE (recur (inc col)))))

(defn ultimate-win-checker
  "Takes a game state, and returns a number that suggests the game strategy
  using ultimate-win-checker-helper function."
  [state]
  (make-push-instruction state ultimate-win-checker-helper [:game-state] :integer))


(defn best-player-helper
  "Returns a column number that will lead to a win by any player immediately.
  If there does not exist one, returns a column number that will lead to
  three pieces in a line.
  If there does not exist one, returns a column number that will lead to
  two pieces in a line.
  Otherwise, returns a random number."
  [game-board opponent-game-piece own-game-piece]
  (let [checklist1 (filter #(win-check
                             (play-a-step game-board own-game-piece %)
                             own-game-piece) (range 7))
        checklist2 (filter #(win-check
                             (play-a-step game-board opponent-game-piece %)
                             opponent-game-piece) (range 7))]
    (cond
      (not (empty? checklist1)) (first checklist1)
      (not (empty? checklist2)) (first checklist2)
      :else (loop [col 0]
              (cond
                (> col 6) (rand-int 7)
                (get (check-col-top-2-helper game-board col own-game-piece) 0) col
                (get (check-left-2-helper game-board col own-game-piece) 0) col
                (get (check-right-2-helper game-board col own-game-piece) 0) col
                (get (check-diag-left-bottom-2-helper game-board col own-game-piece) 0) col
                (get (check-diag-left-top-2-helper game-board col own-game-piece) 0) col
                (get (check-diag-right-top-2-helper game-board col own-game-piece) 0) col
                (get (check-diag-right-bottom-2-helper game-board col own-game-piece) 0) col
                (get (check-diag-left-bottom-helper game-board col own-game-piece) 0) col
                (get (check-diag-left-top-helper game-board col own-game-piece) 0) col
                (get (check-diag-right-top-helper game-board col own-game-piece) 0) col
                (get (check-diag-right-bottom-helper game-board col own-game-piece) 0) col
                (get (check-col-top-helper game-board col own-game-piece) 0) col
                (get (check-left-helper game-board col own-game-piece) 0) col
                (get (check-right-helper game-board col own-game-piece) 0) col
                :ELSE (recur (inc col)))))))

(defn best-player
  "Takes a game state, and returns a number that suggests the game strategy
  using best-player-helper function."
  [state]
  (make-push-instruction state best-player-helper [:game-state :string :string] :integer))


;; An individual that returns 0.
(def test-case-0
  {:program '(0) :errors [] :total-error 0})

;; An individual that returns 1.
(def test-case-1
  {:program '(1) :errors [] :total-error 0})

;; An individual that returns 2.
(def test-case-2
  {:program '(2) :errors [] :total-error 0})

;; An individual that returns 3.
(def test-case-3
  {:program '(3) :errors [] :total-error 0})

;; An individual that returns 4.
(def test-case-4
  {:program '(4) :errors [] :total-error 0})

;; An individual that returns 5.
(def test-case-5
  {:program '(5) :errors [] :total-error 0})

;; An individual that returns 6.
(def test-case-6
  {:program '(6) :errors [] :total-error 0})

;; An individual that uses the step helper program as its strategy. 
(def test-case-7
  {:program '(advanced-player) :errors [] :total-error 0})

;; An individual that uses the ultimate-win-piece-checker program as its
;; game strategy.
(def test-case-8
  {:program '(in3 ultimate-win-piece-checker) :errors [] :total-error 0})

;; An individual that uses the ultimate-win-checker program as its
;; game strategy.
(def test-case-9
  {:program '(ultimate-win-checker) :errors [] :total-error 0})

;; An individual that uses the best-player program as its game strategy.
(def test-case-10
  {:program '(in3 in2 best-player) :errors [] :total-error 0})

;; An individual that uses random game strategy.
(def test-case-rand
  {:program '(integer-rand) :errors [] :total-error 0})


(defn test-inputs
  "A list of test cases for the error function.
  20 of them are randomly selected from the population.
  10 of them are randomly generated.
  20 of them are designed intelligent programs.
  All test cases are individuals (maps)."
  [population]
  (let [test-cases (map #(nth population (mod % (count population)))
                        (rand-list-generator 20 (dec (count population))))
        new-test-cases (take 10 (repeatedly #(make-random-individual instructions 50)))]
    (conj (concat test-cases new-test-cases)
          test-case-0
          test-case-1
          test-case-2
          test-case-3
          test-case-4
          test-case-5
          test-case-6
          test-case-7
          test-case-7
          test-case-7
          test-case-8
          test-case-8
          test-case-8
          test-case-9
          test-case-9
          test-case-9
          test-case-10
          test-case-10
          test-case-10
          test-case-rand)))


(defn error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the game-board set to :in1, the program's piece set to in2,
  and its opponent's piece set to in3 in the :input map part of the Push
  state.
  For each test cases, run two tests on it:
  The first test will let the program play firts and the second test will
  let the test case play first.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: if the program doesn't leave anything on the integer stack,
  gives a penalty error of 1000."

  ;; test-inputs will be a list of individuals
  [individual population]
  (let [errors (concat (map #(error-eval-forward individual %) (test-inputs population))
                       (map #(error-eval-backward individual %) (test-inputs population)))
        total-error (apply +' errors)]
    ;; update the error vector and total-error of the individual
    (assoc individual :errors errors :total-error total-error)))



;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'push307.core)]
    (push-gp{:instructions instructions
             :error-function error-function
             :max-generations 100
             :population-size 200
             :max-initial-program-size 50})))

