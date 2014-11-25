(ns com.vnetpublishing.clj.grid.lib.grid.osgi.activator
  (:gen-class)
  (:use [com.vnetpublishing.clj.grid.lib.grid.kernel]))

(def ^{:private true
       :doc "Grid Modules"} grid-modules
  (atom {}))

(defn get-grid-module 
  [context sym]
    (let [full-sym (symbol (str (name sym) ".module"))]
         (or (get @grid-modules name)
             (when-let [m (find-ns full-sym)]
                 (swap! grid-modules assoc sym m)
                 m)
             (do (load (name full-sym))
                 (when-let [m (find-ns full-sym)]
                         (swap! grid-modules assoc sym m)
                         m)))))

