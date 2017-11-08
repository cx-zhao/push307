;; Matt Goon and Candice Zhao
;; Genetic Programming
;; Term Project: Basic GP System
;; 10/09/2017
;;
;; This program implemented a full GP algorithm, using Push as the
;; representation language. The goal of the GP system is to find a program that
;; calculates the function: f(x) = x^3 + x + 3 for any integer inputs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns push307.core
  (:gen-class))

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '(integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

; An example Push program
(def example-push-program
  '(3 5 integer_* "hello" 4 "world" integer_-))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:program '(3 5 integer_* "hello" 4 "world" integer_-)
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})

;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.
(def instructions
  (list
   'in1
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   'if-int
   'exec-if
   0
   1
   2
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


(defn if-exec-help
  [bool exec1 exec2]
  (if bool
    exec1
    exec2))


(defn if-int-help
  "Takes in a bool and two integers and returns one of the integers depending on the bool value."
  [bool int1 int2]
  (if bool
    int1
    int2))


(defn if-int
  "Make a basic if push instruction with ints."
  [state]
  (make-push-instruction state if-int-help [:bool :integer :integer] :integer))


(defn if-exec-help
  "Takes in a bool and two exec instructions and returns one of the exec instructions depending on the bool value."
  [bool exec1 exec2]
  (if bool
    exec1
    exec2))

(defn exec-if
  "Make a basic if push instruction with exec instructions."
  [state]
  (make-push-instruction state if-exec-help [:bool :exec :exec] :exec))


;; remove later, just for testing
(defn test-helper
  [a]
  [1 "ert" 3 4 "asd" integer_+])


;; remove later, just for testing
(defn test1
  [state]
  (make-push-instruction-list state test-helper [:integer] [:integer :string :integer :integer :string :exec]))


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

(def x (make-random-individual instructions 50))
(def y (make-random-individual instructions 50))

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


(defn lexicase-selection
  "Selects an individual from the population using a tournament of size 5.
  Returned individual will be a parent in the next generation."
  [population error-eval]
  ;; Takes five individuals from the population randomly.
  ;; Does a tail recursion on comparing the total-error of two individuals;
  ;; winner will be the individual with the lowest total-error so far.
  (loop [candidates population
         cases (take 10 (repeatedly #(rand-nth population)))]
    (let [cand-list (filter #(= (error-eval % (first cases)) 0) population)]
      (if (= 1 (count cases))
        (if (empty? cand-list)
          (rand-nth candidates)
          (rand-nth cand-list))
        (recur cand-list
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


(defn select-and-vary
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
      (= genetic-chance 1) {:program (uniform-deletion parent1),
                            :errors [],
                            :total-error 0}
      :ELSE {:program (crossover parent1 parent2),
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
    (update board colnum #(conj % disc))))

;; test
;;(print-board (play-a-step example-board "***" 1))

(defn win-check
  [checklist disc]
  (not= (count (filter #(= [disc disc disc disc] %) checklist)) 0))

(defn check
  [board colnum]
  (let [col (get board colnum)
        index (dec (count col))]
    ;; (print-board board)
    (cond-> []
      ;; check vertical
      (> index 2) (conj (subvec col (- (count col) 4)))
      ;; check horizontal to the left
      (> colnum 2) (conj (vec (map #(get (get board (- colnum %)) index) (range 4))))
      ;; check horizontal to the right
      (< colnum 4) (conj (vec (map #(get (get board (+ colnum %)) index) (range 4))))
      ;; check diagonal, to top right
      (and (< index 3) (< colnum 4)) (conj (vec (map #(get (get board (+ colnum %))
                                                           (+ index %)) (range 4))))
      ;; check diagonal, to bottom left
      (and (> index 2) (> colnum 2)) (conj (vec (map #(get (get board (- colnum %))
                                                           (- index %)) (range 4))))
      ;;check diagonal, to top left
      (and (< index 3) (> colnum 2)) (conj (vec (map #(get (get board (- colnum %))
                                                           (+ index %)) (range 4))))
      ;; check diagonal, to bottom right
      (and (> index 2) (< colnum 4)) (conj (vec (map #(get (get board (+ colnum %))
                                                           (- index %)) (range 4)))))))

(defn switch-player
  [player p1 p2]
  (if (= player p1)
    p2
    p1))

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


(defn switching
  [player individual1 individual2]
  ;;(print individual2)
  (if (= player "ooo")
    individual2
    individual1))


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
         init-state (assoc empty-push-state :input {:in1 init-board})
         result-state (interpret-push-program (individual1 :program) init-state)
         step (peek-stack result-state :integer)]
    
    ;; Note that if the program does not have any output,
    ;; returns a penalty error of 1000.
    (if (= step :no-stack-item)
      1000
      (let [colnum (next-avail-col init-board step)
            game-board (play-a-step init-board player step)
            checklist (check game-board colnum)
            result-state-2 (interpret-push-program
                            ((switching player individual1 individual2) :program)
                            init-state)]
      
        ;;(print-board game-board)
        ;;(println checklist)
        ;;(println player)
        ;;(println)
        (cond (and (win-check checklist player) (= player "ooo")) 0
              (and (win-check checklist player) (= player "***")) 1
              :ELSE (recur game-board
                           (switch-player player "ooo" "***")
                           init-state
                           result-state-2
                           (peek-stack result-state :integer)))))))
            
(defn generate-random-index
  []
  (range 50))

(defn test-inputs
  [population]
  (let [test-cases (map #(nth population (mod % (count population))) (generate-random-index))]
    (conj test-cases
           {:program '(1)
            :errors []
            :total-error 0}
           {:program '(2)
            :errors []
            :total-error 0}
    )
    ))

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
  (let [errors (map #(error-eval individual %) (test-inputs population))
        total-error (apply +' errors)]
    ;; update the error vector and total-error of the individual
    (assoc individual :errors errors :total-error total-error)))


;;(game empty-board "***" "ooo")
;;-------------------------------------
(defn abs-val
  "Returns the absolute value of the input number."
  [x]
  (if (< x 0)
    (* -1 x)
    x))

; A list of test inputs: '(-5 -4 -3 -2 -1 0 1 2 3 4 5)


;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'push307.core)]
    (push-gp {:instructions instructions
              :error-function error-function
              :max-generations 100
              :population-size 200
              :max-initial-program-size 50})))
