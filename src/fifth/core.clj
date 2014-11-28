(ns fifth.core)

;; Functions have the rough type of ((Stack, Interpreter, Scope) -> (Stack, Scope)).

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
  {'?      (pure (fn [[p t f & s]] (conj s (if f t p))))
   'print  (pure (fn [[x & s]] (println x) s))
   'head-tail (pure (fn [[[x & xs] & s]] (conj s xs x)))
   'cons   (pure (fn [[x xs & s]] (conj s (cons x xs))))
   'even?  (unop even?)
   'odd?   (unop odd?)
   'empty? (unop empty?)
   'not    (unop not)
   'inc    (unop inc)
   'dec    (unop dec)
   '+      (binop +)
   '-      (binop -)
   '*      (binop *)
   '/      (binop /)
   '=      (binop =)
   '>      (binop >)
   '>=     (binop >=)
   '<      (binop <)
   '<=     (binop <=)
   'invoke (recursive
             (fn [[f & s] interpret]
               (interpret s (list (symbol (name f))))))
   'store  (fn [[name x & s] interpreter scope] [s (assoc scope name (value x))])})

(def prelude
  "Basic functions."
  {'noop   (native '())
   'dup    (native '(:x store x x))
   'swap   (native '(:x store :y store x y))
   'pop    (native '(:x store))
   'reduce (native '(:r store :i store
                     (empty-branch pop i)
                     (rec-branch head-tail swap i r reduce swap r invoke)
                     dup empty? :empty-branch :rec-branch ? invoke))
   'filter (native '(:p store (reducer dup p invoke :cons :pop ? invoke) [] :reducer reduce))
   'map   (native '(:f store (reducer f invoke cons) [] :reducer reduce))})

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
  ([scope program]
   (interpret scope () program))
  ([scope stack program]
   (let [[final fscope]
         (reduce (fn [[s scope] t]
                   ; (println "term>>" t)
                   (let [[f scope'] (term t scope)]
                     (f s interpret scope')))
                 [stack scope]
                 program)]
     final)))

(defmacro fifth [prog]
  (letfn [(->vec [x] (if (coll? x) (into [] x) x))]
    (clojure.walk/walk
      ->vec
      ->vec
      (first (interpret stdlib prog)))))
