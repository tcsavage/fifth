(ns fifth.core)

(defn unop
  "Unary operator wrapper."
  [f]
  (fn [[x & s] interpret]
    (conj s (f x))))

(defn binop
  "Binary operator wrapper."
  [f]
  (fn [[x y & s] interpret]
    (conj s (f x y))))

(def primops
  {'noop (fn [s interpret]
           s)
   'dup (fn [[x & s] interpret]
          (conj s x x))
   'swap (fn [[x y & s] interpret]
           (conj s x y))
   'pop (fn [[_ & s] interpret]
          s)
   'map (fn [[f xs & s] interpret]
          (conj s (map (fn [x] (first (interpret (list x f)))) xs)))
   'reduce (fn [[r i xs & s] interpret]
             (conj s (reduce (fn [acc x] (first (interpret (list acc x r)))) i xs)))
   'conj (fn [[x xs & s] interpret]
           (conj s (conj xs x)))
   '? (fn [[p t f & s] interpret]
        (conj s (if f t p)))
   'invoke (fn [[f & s] interpret]
             (interpret s (list (symbol (name f)))))
   'print (fn [[x & s] interpret]
            (println x)
            s)
   'even? (unop even?)
   'odd? (unop odd?)
   'not (unop not)
   'inc (unop inc)
   'dec (unop dec)
   '+ (binop +)
   '- (binop -)
   '* (binop *)
   '/ (binop /)
   '= (binop =)
   '> (binop >)
   '>= (binop >=)
   '< (binop <)
   '<= (binop <=)})

(defn term
  "Evaluate a term from command sequence with given scope."
  [x scope]
  (cond
    ; Symbols refer to functions in scope.
    (symbol? x) [(scope x) scope]
    ; Keywords are references to functions, not necessarily in scope.
    (keyword? x) [(fn [s interpret] (conj s (symbol (name x)))) scope]
    ; Lists denote function definitions. Head is function name.
    (list? x) (let [[name & body] x] [(fn [s interpret] s)
                                      (assoc scope
                                        name (eval `(fn [s# interpret#]
                                                      (interpret# s# '(~@body)))))])
    ; Anything else is just put onto the stack as a value.
    :else [(fn [s interpret] (conj s x)) scope]))

(defn interpret
  ([scope program]
   (interpret scope () program))
  ([scope stack program]
   (let [[final fscope]
         (reduce (fn [[s scope] t]
                   (let [[f scope'] (term t scope)]
                     [(f s (partial interpret scope')) scope']))
                 [stack scope]
                 program)]
     final)))

(defmacro fifth [prog]
  (letfn [(->vec [x] (if (coll? x) (into [] x) x))]
    (clojure.walk/walk
      ->vec
      ->vec
      (first (interpret primops prog)))))

(defn run [] (fifth ((keep-odd dup even? not :conj :pop ? invoke)
                     [1 2 3 4 5] [] :keep-odd reduce)))
