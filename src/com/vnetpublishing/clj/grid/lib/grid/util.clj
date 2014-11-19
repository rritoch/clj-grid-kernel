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

(defmacro when-all-let
  [bindings & body]
    (when-not (vector? bindings) 
              (throw (IllegalArgumentException. (str (first &form) " requires a vector for its binding in " 
                                                     *ns* 
                                                     ":" 
                                                     (:line (meta &form))))))
    (when-not (even? (count bindings)) 
              (throw (IllegalArgumentException. (str (first &form) 
                                                     " requires an even number of forms in binding vector in " 
                                                     *ns* 
                                                     ":" 
                                                     (:line (meta &form))))))
    (loop [a (partition 2 bindings)
           b (cons 'do body)]
          (if (empty? a)
              b
              (recur (butlast a)
                     (cons `when-let
                           (conj (list b)
                                 (vec (last a))))))))