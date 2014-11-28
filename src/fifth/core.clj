(ns fifth.core)

;; Functions have the rough type of ((Stack, Interpreter, Scope) -> (Stack, Scope)).
;; Convention: stack locations are referred to by "Sn", such that "S0" is the
;; top of the stack, "S1" is the next highest position, etc.

(defn recursive
  "Helper function for operating on the stack allowing for re-invoking the interpreter.
  Lifts ((Stack, Interpreter) -> Stack) into ((Stack, Interpreter, Scope) -> (Stack, Scope)).
  Interpreter is partially applied to Scope."
  [f]
  (fn [s interpret scope]
    [(f s (partial interpret scope)) scope]))

(defn pure
  "Helper for functions operating solely on the stack.
  Lifts a function (Stack -> Stack) into ((Stack, Interpreter, Scope) -> (Stack, Scope))."
  [f]
  (recursive
    (fn [s interpret] (f s))))

(defn value
  "Helper for creating value functions."
  [x]
  (pure (fn [s] (conj s x))))

(defn unop
  "Unary operator wrapper."
  [f]
  (pure (fn [[x & s]] (conj s (f x)))))

(defn binop
  "Binary operator wrapper."
  [f]
  (pure (fn [[x y & s]] (conj s (f x y)))))

(defn native
  "Helper for building core functions from Fifth code."
  [code]
  (recursive
    (fn [s interpret] (interpret s code))))

(def primops
  "Primitive operations."
  {; If. Pops S0, S1, S2; if S0 is logical true, re-pushes S1, else S2.
   '?         (pure (fn [[p t f & s]] (conj s (if f t p))))
   ; Pops S0 and prints it to stdout.
   'print     (pure (fn [[x & s]] (println x) s))
   ; Pops S0, splits into head and tail, pushes tail followed by head.
   'head-tail (pure (fn [[[x & xs] & s]] (conj s xs x)))
   ; Pops S0 and S1. Pushes result of (cons S0 S1)
   'cons      (pure (fn [[x xs & s]] (conj s (cons x xs))))
   ; Unary operators. Pops S0 and pushes result of (OPERATOR S0).
   'even?     (unop even?)
   'odd?      (unop odd?)
   'empty?    (unop empty?)
   'not       (unop not)
   'inc       (unop inc)
   'dec       (unop dec)
   ; Binary operators. Pops S0 and S1, pushes result of (OPERATOR S1 S0).
   '+         (binop +)
   '-         (binop -)
   '*         (binop *)
   '/         (binop /)
   '=         (binop =)
   '>         (binop >)
   '>=        (binop >=)
   '<         (binop <)
   '<=        (binop <=)
   ; Pops S0. Looks-up S0 in scope as function and invokes it.
   'invoke    (recursive
                (fn [[f & s] interpret]
                  (interpret s (list (symbol (name f))))))
   ; Pops S0 and S1. Builds function in scope with name S0 which pushes S1 when invoked.
   'store     (fn [[name x & s] interpreter scope] [s (assoc scope name (value x))])})

(def prelude
  "Basic functions."
  {; Identity function. Does nothing.
   'noop   (native '())
   ; Duplicates S0.
   'dup    (native '(:x store x x))
   ; Swaps top two stack elements. Pops S0 and S1, pushes S0 then S1.
   'swap   (native '(:x store :y store x y))
   ; Pops S0 and discards it.
   'pop    (native '(:x store))
   ; List reduction. Pops S0, S1 and S2. Pushes equivalent of (reduce S0 S1 S2).
   'reduce (native '(:r store :i store
                     (empty-branch pop i)
                     (rec-branch head-tail swap i r reduce swap r invoke)
                     dup empty? :empty-branch :rec-branch ? invoke))
   ; List filtering. Pops S0 and S1. Pushes equivalent of (filter S0 S1).
   'filter (native '(:p store (reducer dup p invoke :cons :pop ? invoke) [] :reducer reduce))
   ; List mapping. Pops S0 and S1. Pushes equivalent of (map S0 S1).
   'map    (native '(:f store (reducer f invoke cons) [] :reducer reduce))})

(def stdlib (merge primops prelude))

(defn term
  "Evaluate a term from command sequence with given scope."
  [x scope]
  (cond
    ; Symbols refer to functions in scope.
    (symbol? x) (let [f (scope x)]
                  (if f
                    [(scope x) scope]
                    (throw (Exception. (format "Lookup failed for symbol '%s'" (str x))))))
    ; Keywords are references to functions, not necessarily in scope.
    (keyword? x) [(fn [s interpret scope] [(conj s (symbol (name x))) scope]) scope]
    ; Lists denote function definitions. Head is function name.
    (list? x) (let [[name & body] x] [(fn [s interpret scope] [s scope])
                                      (assoc scope
                                        name (eval `(fn [s# interpret# scope#]
                                                      [(interpret# scope# s# '(~@body)) scope#])))])
    ; Anything else is just put onto the stack as a value.
    :else [(fn [s interpret scope] [(conj s x) scope]) scope]))

(defn interpret
  "Interpret a program with given intital scope, and optionally, an inital
  stack (defaults to empty)."
  ([scope program]
   (interpret scope () program))
  ([scope stack program]
   (let [[final fscope]
         ; For each term in the program...
         (reduce (fn [[s scope] t]
                   ; Evaluate it...
                   (let [[f scope'] (term t scope)]
                     ; And apply the resulting function to the stack and scope.
                     (f s interpret scope')))
                 [stack scope]
                 program)]
     final)))

(defmacro fifth
  "Helper macro for running programs. Only returns top of stack."
  [prog]
  (letfn [(->vec [x] (if (coll? x) (into [] x) x))]
    (clojure.walk/walk
      ->vec
      ->vec
      (first (interpret stdlib prog)))))
