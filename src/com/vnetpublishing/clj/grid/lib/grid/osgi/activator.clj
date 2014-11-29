(ns com.vnetpublishing.clj.grid.lib.grid.osgi.activator
  (:gen-class)
  (:require [clojure.java.io :as io]
            [com.vnetpublishing.clj.grid.lib.grid.kernel :refer :all]))

(def ^{:private true
       :doc "Grid Modules"} grid-modules
  (atom {}))

(defn get-grid-module 
  [context sym]
    (let [full-sym (symbol (str (name sym) ".module"))]
         (debug (str "get-grid-module: " (name full-sym)))
         (or (get @grid-modules sym)
             (when-let [m (find-ns full-sym)]
                 (debug (str "Found grid module: " (.toString m)))
                 (swap! grid-modules assoc sym m)
                 m)
             (when-let [r (.getResource (.getBundle context) (str (clojure.string/replace (name full-sym) "." "/") ".clj"))]
                       (debug (str "Loading grid module: " (.toString r)))
                       (load-resource (.toURI r))
                       (when-let [m (find-ns full-sym)]
                                 (swap! grid-modules assoc sym m)
                                 m)))))
