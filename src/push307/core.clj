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
  {:exec '(integer_+)
   :integer '(3 1 2 3 4 5 6 7)
   :string '("abc" "def")
   :bool '(true false)
   :input {:in1 4 :in2 6}})

(def example-push-state-2
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
   :input {:in1 [["***"]
                 ["***""ooo""***""***""***""***"]
                 ["ooo""ooo""ooo""ooo"]
                 ["ooo"]
                 []
                 []
                 ["***""ooo"]] :in2 "***" :in3 "ooo"}})

;; An example Push program
(def example-push-program
  '(3 5 integer_* "hello" 4 "world" integer_-))

(def example-push-program-2
  '(exec-while))

;; An example individual in the population
;; Made of a map containing, at mimimum, a program, the errors for
;; the program, and a total error
(def example-individual
  {:program
   ;'(boolean-and integer-max int-if integer_% check-right-same boolean-rand exec-dup exec-swap 1 fake-step-win-checker integer_+ exec-if in1 exec_= integer-min integer_- false boolean-from-integer integer_< integer_< integer-min check-col-top check-col-top check-left-2 check-diag-left 2 boolean-and exec-swap check-col-top true)
   ;'(exec-if get-a-piece exec-dup check-diag-left int-if boolean_= in1 integer_* ex
   '(integer-max check-diag-left-bottom-2 exec-if 2 exec-if check-diag-right-bottom-2 boolean-rand check-diag-left-top 1 boolean-rand check-diag-right-top-2 integer-dup check-left-2 check-left-2 integer_< boolean-rand 2 boolean-dup 1 1 1 check-right-2 integer_+ integer_+)

   :errors []
   :total-error 0})

(def example-ind-2
   {:program
    ;'(true integer_> in1 exec-dup integer-max integer-from-boolean boolean-from-integer check-col-top-2 exec-swap 1 2 integer_+ integer_+ check-right integer_* integer-min check-right true integer-swap in1 get-a-piece exec_= integer_* integer_> integer-max check-right-same integer-min integer_> in1 check-right-2)
 
    '(int-if check-left-2 check-diag-right-bottom-same integer_> in3 check-diag-right-bottom check-left check-diag-left-bottom 1 check-left-2 check-diag-left-bottom in1 in1 integer_> check-right-2 integer_+ check-diag-right-bottom-2 integer-pop boolean-dup integer-rand check-diag-left-bottom-2 in1)
    :errors[]
    :total-error 0})


;;-----------------------------------
;; game setup
(def empty-board
  [[][][][][][][]])

(def example-board
  [["***"]
   ["***""ooo""***""***""***""***"]
   ["ooo""ooo""ooo""ooo"]
   []
   []
   []
   ["***""ooo"]])

(defn print-board
  "Print a game board"
  [board]
  (loop [index 5]
    (apply print (map #(get % index) board))
    (println)
    (if (< index 1)
      nil
      (recur (dec index)))))

;; test
;;(print-board example-board)

(defn initialize-board
  []
  empty-board)

(defn check-availability
  [board col]
  (< (count (get board (mod col 7))) 6))

(defn full-board
  [board]
  (empty? (filter #(check-availability board %) (range 7))))

(defn next-avail-col
  [board col]
  (first (filter #(check-availability board %)
                 (map #(mod % 7) (range col (+ col 7))))))

(defn play-a-step
  [board disc col]
  (let [colnum (next-avail-col board col)]
    ;;(println colnum)
    (if (= colnum nil)
      board
      (update board colnum #(conj % disc)))))


(defn switch-player
  [player p1 p2]
  (if (= player p1)
    p2
    p1))

;; test
;;(print-board (play-a-step example-board "***" 1))

(defn check
  [checklist disc]
  (not= (count (filter #(= [disc disc disc disc] %) checklist)) 0))

(defn check-vector
  [board colnum]
  (let [col (get board colnum)
        index (dec (count col))]
    ;; (print-board board)
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
      (and (< index 3) (< colnum 4)) (conj (vec (map #(get (get board (+ colnum %))
                                                           (+ index %)) (range 4))))
      ;; check diagonal, to bottom left
      (and (> index 2) (> colnum 2)) (conj (vec (map #(get (get board (- colnum %))
                                                           (- index %)) (range 4))))
      ;; check diagonal, to top left
      (and (< index 3) (> colnum 2)) (conj (vec (map #(get (get board (- colnum %))
                                                           (+ index %)) (range 4))))
      ;; check diagonal, to bottom right
      (and (> index 2) (< colnum 4)) (conj (vec (map #(get (get board (+ colnum %))
                                                           (- index %)) (range 4)))))))



(defn check-helper
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
      (and (< index 3) (< colnum 4)) (conj (vec (map #(get (get board (+ colnum %))
                                                           (+ index %)) (range 4))))
      ;; check diagonal, to bottom left
      (and (> index 2) (> colnum 2)) (conj (vec (map #(get (get board (- colnum %))
                                                           (- index %)) (range 4))))
      ;; check diagonal, to top left
      (and (< index 3) (> colnum 2)) (conj (vec (map #(get (get board (- colnum %))
                                                           (+ index %)) (range 4))))
      ;; check diagonal, to bottom right
      (and (> index 2) (< colnum 4)) (conj (vec (map #(get (get board (+ colnum %))
                                                          (- index %)) (range 4)))))))


(defn win-check
  [board disc]
  (loop [index-num 0]
    (if (>= index-num 42)
      false
      (let [colnum (mod index-num 7)
            rownum (quot index-num 7)]
        ;(println colnum)
        ;(println rownum)
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
   'integer-max
   'integer-min
   'integer-rand
   'integer-swap
   'integer-pop
   'boolean_=
   'boolean-dup
   'boolean-from-integer
   'boolean-rand
   'boolean-swap
   'boolean-and
   'boolean-or
   'piece-rand
   'int-if
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
   2
   true
   false
   ))
;;   'integer-rand
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
  the stacks to take the args from, and the stacks to return the result to.
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
                 (push-to-stack new-state (first return-list) (first result))
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
  "Pushes the input labeled :in3 on the inputs map onto the :exec stack.
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
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (make-push-instruction state protected-division [:integer :integer] :integer))

(defn integer_<_helper
  "Pushes TRUE onto the BOOLEAN stack if the second item is less than the top item, or FALSE otherwise."
  [num1 num2]
  (< num2 num1))

(defn integer_<
  "placeholder"
  [state]
  (make-push-instruction state integer_<_helper [:integer :integer] :bool))


(defn integer_=_helper
  "Pushes TRUE onto the BOOLEAN stack if the top two items are equal, or FALSE otherwise."
  [num1 num2]
  (== num2 num1))

(defn integer_=
  "placeholder"
  [state]
  (make-push-instruction state integer_=_helper [:integer :integer] :bool))


(defn integer_>_helper
  "Pushes TRUE onto the BOOLEAN stack if the second item is greater than the top item, or FALSE otherwise."
  [num1 num2]
  (> num2 num1))

(defn integer_>
  "placeholder"
  [state]
  (make-push-instruction state integer_>_helper [:integer :integer] :bool))

(defn integer-dup-helper
  "Duplicates the top item on the INTEGER stack. Does not pop its argument."
  [num]
  [num num])

(defn integer-dup
  "placeholder"
  [state]
  (make-push-instruction-list state integer-dup-helper [:integer] [:integer :integer]))


(defn integer-from-boolean-helper
  "Pushes 1 if the top BOOLEAN is TRUE, or 0 if the top BOOLEAN is FALSE."
  [bool]
  (if bool 1 0))

(defn integer-from-boolean
  "placeholder"
  [state]
  (make-push-instruction state integer-from-boolean-helper [:bool] :integer))


(defn integer-max-helper
  "Pushes the maximum of the top two items."
  [num1 num2]
  (if (> num1 num2)
    num1
    num2))

(defn integer-max
  "placeholder"
  [state]
  (make-push-instruction state integer-max-helper [:integer :integer] :integer))


(defn integer-min-helper
  "Pushes the minimum of the top two items."
  [num1 num2]
  (if (< num1 num2)
    num1
    num2))

(defn integer-min
  "placeholder"
  [state]
  (make-push-instruction state integer-min-helper [:integer :integer] :integer))


(defn integer-pop-helper
  "Pops an integer off of the top of the INTEGER stack.
  How to just pop something and return nothing to a stack?"
  [num]
  [])

(defn integer-pop
  "placeholder"
  [state]
  (make-push-instruction-list state integer-pop-helper [:integer] []))

(def min-random-integer 0)
(def max-random-integer 7)

(defn integer-rand-helper
  "Pushes a newly generated random integer that is greater than or equal to min-random-integer and less than or equal to max-random-integer."
  []
  (rand-int 7))

(defn integer-rand
  "how to not pop anything and just push number?"
  [state]
  (make-push-instruction state integer-rand-helper [] :integer))


(defn integer-swap-helper
  "Swaps the top two INTEGERs on the INTEGER stack."
  [num1 num2]
  [num2 num1])

(defn integer-swap
  "placeholder"
  [state]
  (make-push-instruction-list state integer-swap-helper [:integer :integer] [:integer :integer]))


(defn boolean_=_helper
  "Pushes TRUE onto the BOOLEAN stack if the top two BOOLEANs are equal, or FALSE otherwise."
  [bool1 bool2]
  (= bool1 bool2))

(defn boolean_=
  "placeholder"
  [state]
  (make-push-instruction state boolean_=_helper [:bool :bool] :bool))


(defn boolean-and-helper
  "Pushes the logical AND of the top two BOOLEANs."
  [bool1 bool2]
  (and bool1 bool2))

(defn boolean-and
  "placeholder"
  [state]
  (make-push-instruction state boolean-and-helper [:bool :bool] :bool))


(defn boolean-or-helper
  "Pushes the logical OR of the top two BOOLEANs."
  [bool1 bool2]
  (or bool1 bool2))

(defn boolean-or
  "placeholder"
  [state]
  (make-push-instruction state boolean-and-helper [:bool :bool] :bool))


(defn boolean-dup-helper
  "Duplicates the top item on the BOOLEAN stack. Does not pop its argument."
  [bool]
  [bool bool])

(defn boolean-dup
  "placeholder"
  [state]
  (make-push-instruction-list state boolean-dup-helper [:bool] [:bool :bool]))


(defn boolean-from-integer-helper
  "Pushes FALSE if the top INTEGER is 0, or TRUE otherwise."
  [num]
  (if (== 0 num)
    false
    true))

(defn boolean-from-integer
  "placeholder"
  [state]
  (make-push-instruction state boolean-from-integer-helper [:integer] :bool))


(defn boolean-rand-helper
  "Pushes a newly generated random integer that is greater than or equal to min-random-integer and less than or equal to max-random-integer."
  []
  (rand-nth [true false]))

(defn boolean-rand
  "placeholder"
  [state]
  (make-push-instruction state boolean-rand-helper [] :bool))


(defn boolean-swap-helper
  "Swaps the top two BOOLEANs on the BOOLEAN stack."
  [bool1 bool2]
  [bool2 bool1])

(defn boolean-swap
  "placeholder"
  [state]
  (make-push-instruction-list state boolean-swap-helper [:bool :bool] [:bool :bool]))


(defn int-if-helper
  "Takes in a bool and two integers and returns one of the integers depending on the bool value."
  [bool int1 int2]
  (if bool
    int1
    int2))

(defn int-if
  "Make a basic if push instruction with ints."
  [state]
  (make-push-instruction state int-if-helper [:bool :integer :integer] :integer))


(defn exec_=_helper
  "Pushes TRUE onto the BOOLEAN stack if the top two items on the EXEC stack are equal, or FALSE otherwise."
  [exec1 exec2]
  (= exec1 exec2))

(defn exec_=
  "placeholder"
  [state]
  (make-push-instruction state exec_=_helper [:exec :exec] :bool))


(defn exec-dup-helper
  "Duplicates the top item on the EXEC stack. Does not pop its argument."
  [exec]
  [exec exec])

(defn exec-dup
  "placeholder"
  [state]
  (make-push-instruction-list state exec-dup-helper [:exec] [:exec :exec]))


(defn exec-if-helper
  "If the top item of the BOOLEAN stack is TRUE then this removes the second item on the EXEC stack, leaving the first item to be executed. If it is false then it removes the first item, leaving the second to be executed."
  [bool exec1 exec2]
  (if bool
    exec1
    exec2))

(defn exec-if
  "Make a basic if push instruction with exec instructions."
  [state]
  (make-push-instruction state exec-if-helper [:bool :exec :exec] :exec))


(defn exec-swap-helper
  "Swaps the top two items on the EXEC stack."
  [exec1 exec2]
  [exec2 exec1])

(defn exec-swap
  "placeholder"
  [state]
  (make-push-instruction-list state exec-swap-helper [:exec :exec] [:exec :exec]))


(defn fill-in-blank
  [state]
  state)

(defn exec-while-helper
  "Custom while loop"
  [exec integer]
  (cond
    (<= integer 0) ['fill-in-blank 'fill-in-blank 'fill-in-blank 'fill-in-blank]
    (= integer 1) [exec 'fill-in-blank 'fill-in-blank 'fill-in-blank]
    :ELSE [exec 'exec-while (dec integer) exec]))


(defn exec-while
  [state]
  (make-push-instruction-list state exec-while-helper [:exec :integer] [:exec :exec :exec :exec]))


(defn piece-rand-helper
  "Pushes a random game piece onto the string stack."
  []
  (rand-nth ["***" "ooo"]))

(defn piece-rand
  "placeholder"
  [state]
  (make-push-instruction state boolean-rand-helper [] :string))


(defn check-col-top-helper
  "Returns true if the top piece in the column is the player's own piece, else returns false if it is the opponent's piece. If the column is empty, return false."
  [game-board num string]
  (if (full-board game-board)
    [false game-board]
    (let [colnum (next-avail-col game-board num)
          col (get game-board colnum)]
      (if (empty? col)
        [false game-board]
        [(= string (get col (dec (count col)))) game-board]))))

(defn check-col-top
  "placeholder"
  [state]
  (make-push-instruction-list state check-col-top-helper [:game-state :integer :string] [:bool :game-state]))

(defn check-col-top-2-helper
  "Returns true if the top piece in the column is the player's own piece, else returns false if it is the opponent's piece. If the column is empty, return false."
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
  "placeholder"
  [state]
  (make-push-instruction-list state check-col-top-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))

(defn check-left-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-left-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))


(defn check-left-2-helper
  "placeholder"
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
            [(and (= left-piece-1 string) (= left-piece-2 string)) game-board])))

(defn check-left-2
  [state]
  (make-push-instruction-list state check-left-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))

(defn check-right-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-right-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))

(defn check-right-2-helper
  "placeholder"
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
            [(and (= right-piece-1 string) (= right-piece-2 string)) game-board])))


(defn check-right-2
  [state]
  (make-push-instruction-list state check-right-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))

(defn check-diag-right-bottom-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-right-bottom-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))
  
(defn check-diag-right-top-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-right-top-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))
  
(defn check-diag-left-bottom-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-left-bottom-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))
  
(defn check-diag-left-top-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-left-top-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))
  
  
(defn check-diag-right-bottom-2-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-right-bottom-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))
  
(defn check-diag-right-top-2-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-right-top-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))
  
(defn check-diag-left-bottom-2-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-left-bottom-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))
  
(defn check-diag-left-top-2-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-left-top-2-helper
                              [:game-state :integer :string]
                              [:bool :game-state]))

  
  
(defn check-diag-right-bottom-same-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-right-bottom-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))
  
(defn check-diag-right-top-same-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-right-top-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))
  
(defn check-diag-left-bottom-same-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-left-bottom-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))
  
(defn check-diag-left-top-same-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-diag-left-top-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))

(defn check-right-same-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-right-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))

(defn check-left-same-helper
  "placeholder"
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
  [state]
  (make-push-instruction-list state check-left-same-helper
                              [:game-state :integer]
                              [:bool :game-state]))

(defn get-a-piece-helper
  "Returns the piece at column c, row r on the board. If there is not a piece at the location, return an empty string."
  [game-board c r]
  (let [col (get game-board (mod c 7))
        piece (get col (mod r 6))]
    (if (= nil piece)
      ""
      [piece game-board])))


(defn get-a-piece
  "placeholder"
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
      ;(print (check checklist-o "ooo"))
      ;(println checklist-o)
      ;(print (check checklist-* "***"))
      ;(println checklist-*)
      [c (or (check checklist-o "ooo")
             (check checklist-* "***")) game-board])))

(defn fake-step-win-checker
  [state]
  (make-push-instruction-list state fake-step-win-checker-helper
                              [:game-state :integer]
                              [:integer :bool :game-state]))

(defn step-helper
  [game-board]
  (loop [num 0]
    (cond
      (> num 6) (rand-int 7)
      (get (fake-step-win-checker-helper game-board num) 1) num
      :ELSE (recur (inc num)))))

(defn step
  [state]
  (make-push-instruction state step-helper
                         [:game-state] :integer))

(defn ultimate-win-piece-checker-helper
  ""
  [game-board game-piece]
  (loop [col 0]
    (cond
      (> col 6) (rand-int 7)
      (win-check (play-a-step game-board game-piece col) game-piece) col
      :ELSE (recur (inc col)))))

(defn ultimate-win-piece-checker
  [state]
  (make-push-instruction state ultimate-win-piece-checker-helper [:game-state :string] :integer))

(defn ultimate-win-checker-helper
  ""
  [game-board]
  (loop [col 0]
    (cond
      (> col 6) (rand-int 7)
      (win-check (play-a-step game-board "ooo" col) "ooo") col
      (win-check (play-a-step game-board "***" col) "***") col
      :ELSE (recur (inc col)))))

(defn ultimate-win-checker
  [state]
  (make-push-instruction state ultimate-win-checker-helper [:game-state] :integer))

(defn best-player-helper
  ""
  [game-board opponent-game-piece own-game-piece]
  (let [checklist1 (filter #(win-check (play-a-step game-board own-game-piece %) own-game-piece) (range 7))
        checklist2 (filter #(win-check (play-a-step game-board opponent-game-piece %) opponent-game-piece) (range 7))]
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
  [state]
  (make-push-instruction state best-player-helper [:game-state :string :string] :integer))
      
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



(defn switching
  [player individual1 individual2]
  ;;(print individual2)
  (if (= player "***")
    individual2
    individual1))

(defn switching-forward
  [player individual1 individual2]
  ;;(print individual2)
  (if (= player "ooo")
    individual2
    individual1))

(defn error-eval-old
  "Takes an individual, an individual input.
  Returns absolute value of the difference (error)
  between the output from the program and the desired output."
  [individual1 individual2]
  ;; init-state is an empty push state with :in1 = input value.
  ;; result-state is the state after interpreting the push program of the
  ;; individual over the init-state
  ;; output is the integer on top of the integer stack in the result state.

  (loop [init-board empty-board
         player "ooo"
         init-state (assoc empty-push-state :input {:in1 init-board :in2 "ooo" :in3 "***"} :game-state (list init-board))
         result-state (interpret-push-program (individual1 :program) init-state)
         step (peek-stack result-state :integer)
         round 0]

    ;(print-board init-board)
    ;(println player)
    ;(println step)
    ;; Note that if the program does not have any output,
    ;; returns a penalty error of 1000.
    ;; if the game-board is full, return 1, fail to win
    (if (full-board init-board)
      1
      (if (= step :no-stack-item)
        (if (= player "ooo")
          1000
          (let [step (rand-int 7)
                colnum (next-avail-col init-board step)
                game-board (play-a-step init-board player step)
                init-state-2 (assoc empty-push-state
                                    :input {:in1 game-board
                                            :in2 "ooo"
                                            :in3 "***"}
                                    :game-state (list game-board))
                result-state-2 (interpret-push-program
                                ((switching-forward player individual1 individual2) :program)
                                init-state-2)]
            ;;(println (init-state-2 :input))
            ;;(print-board game-board)
            ;;(println checklist)
            ;;(println player)
            ;;(println)
            (cond (and (win-check game-board player) (= player "ooo")) 0
                  (and (win-check game-board player) (= player "***")) (- 100 round)
                  :ELSE (recur game-board
                               "ooo"
                               init-state-2
                               result-state-2
                               (peek-stack result-state-2 :integer)
                               (inc round)))))
        
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

          (cond (and (win-check game-board player) (= player "ooo")) 0
                (and (win-check game-board player) (= player "***")) (- 100 round)
                :ELSE (recur game-board
                             (switch-player player "ooo" "***")
                             init-state-2
                             result-state-2
                             (peek-stack result-state-2 :integer)
                             (inc round))))))))


(defn error-eval
  "Takes an individual, an individual input.
  Returns absolute value of the difference (error)
  between the output from the program and the desired output."
  [individual1 individual2]
  ;; init-state is an empty push state with :in1 = input value.
  ;; result-state is the state after interpreting the push program of the
  ;; individual over the init-state
  ;; output is the integer on top of the integer stack in the result state.

  (loop [init-board empty-board
         player "ooo"
         init-state (assoc empty-push-state :input {:in1 init-board :in2 "ooo" :in3 "***"} :game-state (list init-board))
         result-state (interpret-push-program (individual2 :program) init-state)
         step (peek-stack result-state :integer)
         round 0]

   ;(print-board init-board)
   ;(println player)
   ;(println step)
    ;; Note that if the program does not have any output,
    ;; returns a penalty error of 1000.
    ;; if the game-board is full, return 1, fail to win
    (if (full-board init-board)
      1
      (if (= step :no-stack-item)
        (if (= player "***")
          1000
          (let [step (rand-int 7)
                colnum (next-avail-col init-board step)
                game-board (play-a-step init-board player step)
                init-state-2 (assoc empty-push-state
                                    :input {:in1 game-board
                                            :in2 "***"
                                            :in3 "ooo"}
                                    :game-state (list game-board))
                result-state-2 (interpret-push-program
                                ((switching player individual1 individual2) :program)
                                init-state-2)]
            ;;(println (init-state-2 :input))
            ;;(print-board game-board)
            ;;(println checklist)
            ;;(println player)
            ;;(println)
            (cond (and (win-check game-board player) (= player "***")) 0
                  (and (win-check game-board player) (= player "ooo")) (- 100 round)
                  :ELSE (recur game-board
                               "***"
                               init-state-2
                               result-state-2
                               (peek-stack result-state-2 :integer)
                               (inc round)))))
        
        (let [colnum (next-avail-col init-board step)
              game-board (play-a-step init-board player step)
              init-state-2 (assoc empty-push-state
                                  :input {:in1 game-board
                                          :in2 (switch-player player "ooo" "***")
                                          :in3 player}
                                  :game-state (list game-board))
              result-state-2 (interpret-push-program
                              ((switching player individual1 individual2) :program)
                              init-state-2)]
 
          (cond (and (win-check game-board player) (= player "***")) 0
                (and (win-check game-board player) (= player "ooo")) (- 100 round)
                :ELSE (recur game-board
                             (switch-player player "ooo" "***")
                             init-state-2
                             result-state-2
                             (peek-stack result-state-2 :integer)
                             (inc round))))))))

;;;;;;;;;;
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


(defn tournament-lexicase-selection
  "Selects an individual from the population using a tournament of size 5.
  Returned individual will be a parent in the next generation."
  [population case-list]
  ;; Takes five individuals from the population randomly.
  ;; Does a tail recursion on comparing the total-error of two individuals;
  ;; winner will be the individual with the lowest total-error so far.
  (loop [candidates population
         cases case-list]
    (let [cand-list
          (filter #(= (error-eval % (first cases)) 0) candidates)]
      ;;(println (count candidates))
      ;;(println (count cand-list))
      (cond
        (empty? cand-list) (tournament-selection candidates)
        (= 1 (count cases)) (rand-nth cand-list)
        :ELSE (recur cand-list
                     (rest cases))))))


(defn lexicase-tournament-selection
  "Selects an individual from the population using a tournament of size 5.
  Returned individual will be a parent in the next generation."
  [population case-list]
  ;; Takes five individuals from the population randomly.
  ;; Does a tail recursion on comparing the total-error of two individuals;
  ;; winner will be the individual with the lowest total-error so far.
  (loop [candidates (take 5 (repeatedly #(tournament-selection population)))
         cases case-list]
    (let [cand-list
          (filter #(= (error-eval % (first cases)) 0) candidates)]
      ;;(println (count candidates))
      ;;(println (count cand-list))
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


(defn select-and-vary-old
  "Selects parent(s) from population using tournament selection of size 5
  and varies them, returning a child individual (note: not program).
  Chooses which genetic operator to use probabilistically.
  Gives 50% chance to crossover, 25% to uniform-addition,
  and 25% to uniform-deletion."
  [population]
  (let [genetic-chance (rand-int 4)
        parent1 ((tournament-selection population) :program)
        parent2 ((tournament-selection population) :program)]
    ;; Note that the default setting for the returned child individual
    ;; will have a error vector of [] and a total-error of 0.
    (cond
      (= genetic-chance 0) {:program (uniform-addition parent1),
                            :errors [],
                            :total-error 0}
      (= genetic-chance 1) {:program (uniform-deletion parent2),
                            :errors [],
                            :total-error 0}
      :ELSE {:program (crossover parent1 parent2),
             :errors [],
             :total-error 0})))

(defn select-and-vary
  "Selects parent(s) from population using tournament selection of size 5
  and varies them, returning a child individual (note: not program).
  Chooses which genetic operator to use probabilistically.
  Gives 50% chance to crossover, 25% to uniform-addition,
  and 25% to uniform-deletion."
  [population]
  (let [genetic-chance (rand-int 6)
        cases (take 50 (repeatedly #(rand-nth population)))
        reverse-cases (reverse cases)]
    ;; Note that the default setting for the returned child individual
    ;; will have a error vector of [] and a total-error of 0.
    (cond
      (= genetic-chance 0) {:program (uniform-addition ((tournament-selection population) :program)),
                            :errors [],
                            :total-error 0}
      (= genetic-chance 1) {:program (uniform-deletion ((tournament-selection population) :program)),
                            :errors [],
                            :total-error 0}
      (< genetic-chance 4) {:program (crossover ((tournament-lexicase-selection population cases) :program)
                                                ((tournament-lexicase-selection population reverse-cases) :program)),
                            :errors [],
                            :total-error 0}
      :ELSE {:program (crossover ((tournament-selection population) :program)
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

  Returns the best program in the population.
  "
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


(defn initialize-population-1
  "Takes a population-size, a list of instructions,
  , a maximum initial program size and an error function.
  Generates a population of randomly generated individuals.
  Then, evaluates the population by the input error function.
  Returns the evaluated population."
  [population-size instructions max-initial-program-size error-function]
  ;; Evaluates each individual over the error function.
  ;; Generates a population randomly.
  (take population-size
        (repeatedly #(error-function
                      (make-random-individual instructions
                                              max-initial-program-size)))))

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
  50% chance to crossover, 25% to uniform-addition and 25% to uniform-deletion,
  and evaluates the individuals using error-function.
  Returns the evaluated new population."
  [population population-size error-function]
  (take population-size
        (repeatedly #(error-function (select-and-vary population) population))))

(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
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
         generation 0
         best-prog (report initial-population generation)]
    ;; Stops if it finds and individual with a total error of 0 (returns
    ;; :SUCCESS) or if the program exceeds the maximum generation (returns nil).
    (cond
      (= (best-prog :total-error) 0) :SUCCESS
      (= generation max-generations) nil
      :ELSE (let [new-population (generate-new-population initial-population
                                                          population-size
                                                          error-function)]
              (recur new-population
                     (inc generation)
                     (report new-population (inc generation)))))))

(defn rand-list-generator
  [x max-value]
  (map (fn [%] (rand-int max-value)) (range x)))

(defn push-gp-test
  "Main GP loop. Initializes the population, and then repeatedly
  +  generates and evaluates new populations. Stops if it finds an
  +  individual with 0 error (and should return :SUCCESS, or if it
  +  exceeds the maximum generations (and should return nil). Should print
  +  report each generation.
  +  --
  +  The only argument should be a map containing the core parameters to
  +  push-gp. The format given below will decompose this map into individual
  +  arguments. These arguments should include:
  +  - population-size
  +  - max-generations
  +  - error-function
  +  - instructions (a list of instructions)
  +  - max-initial-program-size (max size of randomly generated programs)"
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
    ;; Stops if it finds and individual with a total error of 0 (returns
    ;; :SUCCESS) or if the program exceeds the maximum generation (returns nil).
    (cond
      (or (= (best-prog-2 :total-error) 0) (= (best-prog :total-error) 0)) :SUCCESS
      (= generation max-generations) nil
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

;;;;;;;;;;
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ (* x x x) x 3))

(defn random-strategy
  []
  (rand-nth (range 7)))


(defn game
  [board p1 p2]
  (loop [init-board board
         player p1
         step (random-strategy)]

    (let [colnum (next-avail-col init-board step)
          game-board (play-a-step init-board player step)
          checklist (check game-board colnum)]

      (print-board game-board)
      (println checklist)
      (println player)
      (println)
      (if (win-check checklist player)
        player
        (recur game-board
               (switch-player player p1 p2)
               (random-strategy))))))


(defn mock-game
  "Takes an individual, an individual input.
  Returns absolute value of the difference (error)
  between the output from the program and the desired output."
  [individual1 individual2]
  ;; init-state is an empty push state with :in1 = input value.
  ;; result-state is the state after interpreting the push program of the
  ;; individual over the init-state
  ;; output is the integer on top of the integer stack in the result state.
  (loop [init-board empty-board
         player "ooo"
         init-state (assoc empty-push-state :input {:in1 init-board :in2 "ooo" :in3 "***"} :game-state (list init-board))
         result-state (interpret-push-program (individual1 :program) init-state)
         step (peek-stack result-state :integer)]
    (print-board init-board)
    ;(println result-state)
    ;(println init-state)
    (println player)
    (println step)
    
    ;; Note that if the program does not have any output,
    ;; returns a penalty error of 1000.
    ;; if the game-board is full, return 1, fail to win
    (if (full-board init-board)
      1
      (if (= step :no-stack-item)
        (if (= player "ooo")
          1000
          (let [step (rand-int 7)
                colnum (next-avail-col init-board step)
                game-board (play-a-step init-board player step)
                init-state-2 (assoc empty-push-state
                                    :input {:in1 game-board
                                            :in2 "ooo"
                                            :in3 "***"}
                                    :game-state (list game-board))
                result-state-2 (interpret-push-program
                                ((switching-forward player individual1 individual2) :program)
                                init-state-2)]
            ;;(println (init-state-2 :input))
            ;;(print-board game-board)
            ;;(println checklist)
            ;;(println player)
            ;;(println)
            (cond (and (win-check game-board player) (= player "ooo")) 0
                  (and (win-check game-board player) (= player "***")) 1
                  :ELSE (recur game-board
                               "ooo"
                               init-state-2
                               result-state-2
                               (peek-stack result-state-2 :integer)))))
        
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
 
          (cond (and (win-check game-board player) (= player "ooo")) 0
                (and (win-check game-board player) (= player "***")) 1
                :ELSE (recur game-board
                             (switch-player player "ooo" "***")
                             init-state-2
                             result-state-2
                             (peek-stack result-state-2 :integer))))))))


(defn generate-random-index
  []
  (range 30))

(def test-case-0
  {:program '(0) :errors [] :total-error 0})

(def test-case-1
  {:program '(1) :errors [] :total-error 0})

(def test-case-2
  {:program '(2) :errors [] :total-error 0})

(def test-case-3
  {:program '(3) :errors [] :total-error 0})

(def test-case-4
  {:program '(4) :errors [] :total-error 0})

(def test-case-5
  {:program '(5) :errors [] :total-error 0})

(def test-case-6
  {:program '(6) :errors [] :total-error 0})

(def test-case-7
  {:program '(step) :errors [] :total-error 0})

(def test-case-8
  {:program '(in3 ultimate-win-piece-checker) :errors [] :total-error 0})

(def test-case-9
  {:program '(ultimate-win-checker) :errors [] :total-error 0})

(def test-case-10
  {:program '(in3 in2 best-player) :errors [] :total-error 0})

(def test-case-rand
  {:program '(integer-rand) :errors [] :total-error 0})


(defn test-inputs
  " A list of test cases for the error function.
  All test cases are individuals (maps)."
  [population]
  (let [test-cases (map #(nth population (mod % (count population)))
                        (generate-random-index))
        new-test-cases (take 5
                             (repeatedly #(make-random-individual instructions
                                                                  50)))]
    (conj test-cases
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
          test-case-10
          test-case-rand)))



(defn error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: if the program doesn't leave anything on the integer stack,
  gives a penalty error of 1000."

  ;; test-inputs will be a list of individuals
  [individual population]
  ;;(print (test-inputs population))
  (let [;errors (concat (map #(error-eval individual %) (test-inputs population))
        errors (map #(error-eval-old individual %) (test-inputs population))
        total-error (apply +' errors)]
    ;; update the error vector and total-error of the individual
    (assoc individual :errors errors :total-error total-error)))



;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'push307.core)]
    (push-gp-test {:instructions instructions
              :error-function error-function
              :max-generations 100
              :population-size 100
              :max-initial-program-size 50})))

