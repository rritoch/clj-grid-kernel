(ns com.vnetpublishing.clj.grid.lib.grid.osgi.activator
  (:gen-class)
  (:use [com.vnetpublishing.clj.grid.lib.grid.kernel]))

(def ^{:private true
       :doc "Grid Modules"} grid-modules
  (atom {}))

(defn get-grid-module 
  [context sym]
  (or (get @grid-modules name)
      (let [clz (resolve sym)
            m (create-instance clz [])]
        (swap! grid-modules assoc sym m)
        m)))

