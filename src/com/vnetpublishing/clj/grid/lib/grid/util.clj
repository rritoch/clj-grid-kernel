(ns com.vnetpublishing.clj.grid.lib.grid.util)

(defn ucfirst
  "Convert first character of string to upper case"
  [s]
  (let [n (str s)]
    (if (empty? n)
        ""
      (str (clojure.string/upper-case (subs s 0 1)) (subs s 1)))))

(defn in?
  "Test if sequence contains value"
  [seq value]  
    (some (partial = value) 
          seq))