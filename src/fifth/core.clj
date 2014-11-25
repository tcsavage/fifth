(ns fifth.core)

;; Functions have the rough type of ((Stack, Interpreter) -> Stack).

(defn recursive
  "Helper function for operating on the stack allowing for re-invoking the interpreter."
  [f]
  (fn [s interpret scope]
    [(f s (partial interpret scope)) scope]))

(defn pure
  "Helper for functions operating solely on the stack.
  Lifts a function (Stack -> Stack) into ((Stack, Interpreter) -> Stack)."
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
  {'noop   (pure identity)
   'dup    (pure (fn [[x & s]] (conj s x x)))
   'swap   (pure (fn [[x y & s]] (conj s x y)))
   'pop    (pure rest)
   'conj   (pure (fn [[x xs & s]] (conj s (conj xs x))))
   '?      (pure (fn [[p t f & s]] (conj s (if f t p))))
   'print  (pure (fn [[x & s]] (println x) s))
   'even?  (unop even?)
   'odd?   (unop odd?)
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
   'map    (recursive
             (fn [[f xs & s] interpret]
               (conj s (map (fn [x] (first (interpret (list x f)))) xs))))
   'reduce (recursive
             (fn [[r i xs & s] interpret]
               (conj s (reduce (fn [acc x] (first (interpret (list acc x r)))) i xs))))
   'invoke (recursive
             (fn [[f & s] interpret]
               (interpret s (list (symbol (name f))))))
   'store  (fn [[name x & s] interpreter scope] [s (assoc scope name (value x))])})

(def prelude
  {'filter (native '(:p store (reducer dup p invoke :conj :pop ? invoke) [] :reducer reduce))})

(def stdlib (merge primops prelude))

(defn term
  "Evaluate a term from command sequence with given scope."
  [x scope]
  (cond
    ; Symbols refer to functions in scope.
    (symbol? x) [(scope x) scope]
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

(defn run [] (fifth ((keep-odd dup even? not :conj :pop ? invoke)
                     [1 2 3 4 5] [] :keep-odd reduce)))
