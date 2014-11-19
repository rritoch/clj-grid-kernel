(ns com.vnetpublishing.clj.grid.lib.grid.http-mapper
  (:require [com.vnetpublishing.clj.grid.lib.grid.util :refer :all]))

(def ^:private mappings (atom []))

(defn remove-suffix-mapping
  [suffix]
    (swap! mappings 
           (partial filterv 
                    (partial (fn [suffix x]
                                 (not= suffix (:suffix x)))
                             suffix))))

(defn add-suffix-mapping 
  [suffix clz]
    (remove-suffix-mapping suffix)
    (swap! mappings conj {:class clz :suffix suffix}))

(defn get-default-mapping
   (first (keep #(if (:default %1)
                     (:class %1))
                @mappings)))

(defn set-default-mapping
  [clz]
    (swap! mappings (partial filterv (fn [x] (not (:default x)))))
    (swap! mappings conj {:class clz :default true}))

(defn map-url
  [url]
    (let [uri (URI. url)
          path (.getPath uri)]
         (or (first (keep (partial #(if (and (:suffix %2)
                                             (.endsWith %1 (:suffix %2)))
                                        (:class %2))
                                   path)
                          @mappings))
             (get-default-mapping))))

