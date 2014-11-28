fifth
=====

Simple concatenative language. Looks a little bit like Forth if you squint _really_ hard. Inspired by Brandon Bloom's [tweet](https://twitter.com/BrandonBloom/status/528262785642545153).

Supports function definitions and higher-order functions.

```clojure
(use 'fifth.core)

(fifth ((plus-five 5 +)  ; Define function in scope
        [1 2 3 4 5]      ; Push vector onto stack
        :plus-five       ; Push reference to plus-five onto stack
        map))            ; Invoke map function
```
