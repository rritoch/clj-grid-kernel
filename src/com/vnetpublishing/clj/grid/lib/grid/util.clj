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

(defn record-merge-nd
  "Non-destructive merge map item i into map list q having primary key pk adding matching non-primary keys as vector items"
  [pk q i] 
    (if-let [p1 (first (filter (partial (fn [k v i] (= (get i k) v)) 
                                        pk 
                                        (get i pk)) q))] 
      (conj (filterv (partial (fn [k v i] (= (get i k) v)) pk (get i pk)) q) 
            (into {} (map (partial (fn [pk p1 p2 k] 
                                       (if (or (= pk k) 
                                               (not (get p1 k)) 
                                               (not (get p2 k))) 
                                           [k (or (get p1 k) (get p2 k))] 
                                           [k (into 
                                              (if (instance? java.util.List (get p1 k))
                                                  (vec (get p1 k))
                                                  [(get p1 k)])
                                              (if (instance? java.util.List (get p2 k))
                                                  (vec (get p2 k))
                                                  [(get p2 k)]))]))
                                   pk p1 i) 
                          (seq (set (concat (keys p1) (keys i))))))) 
      (conj q i)))
