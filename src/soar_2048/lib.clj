(ns soar-2048.lib)

(defmacro doto-returnlast 
    "Like doto but returns the result of the last form instead 
     of the first argument"
    [& args]
    `(-> (doto ~@(butlast args))
         ~(last args)))

(defmacro if-return-else
  "If test is true, return test else yield else"
  [test else]
  `(if ~test ~test ~else))
