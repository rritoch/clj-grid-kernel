; Note: Gen class used to identify correct class loader
(ns com.vnetpublishing.clj.grid.lib.grid.osgi.functions
  (:gen-class :name com.vnetpublishing.clj.grid.lib.grid.osgi.Functions)
  (:import [java.io BufferedReader 
                    InputStreamReader 
                    File 
                    StringWriter]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.w3c.dom Node]
           [org.osgi.framework Bundle])
  (:require [com.vnetpublishing.clj.grid.lib.grid.kernel :refer :all]
            [clojure.repl :refer [pst]]))

(def ^:dynamic *factory-uri* 
  "META-INF/services/org.osgi.framework.launch.FrameworkFactory")

(defn osgi-boot
  []
    (debug "Booting OSGi")
    (if (.isFile (File. (str *local-web-root* 
                             "WEB-INF" 
                             *ds* 
                             "plugins.xml")))
        (let [_ (debug "Loading WEB-INF/plugins.xml")
              src (File. (str *local-web-root*  
                              "WEB-INF" 
                              *ds* 
                              "plugins.xml"))
              factory (DocumentBuilderFactory/newInstance)
              builder (do (.setNamespaceAware factory true)
                          (.newDocumentBuilder factory))
              doc (.parse builder src)
              rootnode (.getDocumentElement doc)
              nfilter (fn [node]
                          (and (= "urn:vnetpublishing:com:clj:grid:plugins"
                                  (.getNamespaceURI node))
                               (= "bundle"
                                  (.getLocalName node))
                               (= Node/ELEMENT_NODE
                                  (.getNodeType node))
                               (= "true"
                                  (.getAttribute node
                                                 "active"))))
              nodes-list (.getChildNodes rootnode)
              nodes-list-n (.getLength nodes-list)
              nodes (loop [n 0 out []]
                      (if (>= n nodes-list-n)
                          out
                          (recur (+ n 1)
                                 (conj out 
                                       (.item nodes-list n)))))
              bundle-nodes (filter nfilter nodes)
              bundle-state (.getState (.getBundle *osgi-context*))]
            (debug (str "Framework State: "
                        (cond (= Bundle/ACTIVE bundle-state)
                              "Active"
                              (= Bundle/STARTING bundle-state)
                              "Starting"
                              (= Bundle/STOPPING bundle-state)
                              "Stopping"
                              :else
                              (str "Unknown[" bundle-state "]"))))
            (doseq [node bundle-nodes]
              (try (install-local-bundle (.getAttribute node "src"))
                   (catch Throwable t 
                      (let [sw (StringWriter.)]
                           (binding [*err* sw]
                                    (pst t))
                              (kdebug (.toString sw)))))))
        (debug "WEB-INF/plugins.xml not found.")))

(defn get-osgi-framework-factory
  []
  (let [url (.getResource (.getClassLoader com.vnetpublishing.clj.grid.lib.grid.osgi.Functions) *factory-uri*)
        br (if url (BufferedReader. (InputStreamReader. (.openStream url))))]
    (try (loop [s (if br (try (.readLine br))) f nil]
               (if (or (not s) f)
                   (if f
                       f
                       (throw (Exception. "Could not find framework factory.")))
                   (recur (if br (.readLine br))
                          (if (and (> (count s) 0)
                                   (not (= (subs s 0 1) "#")))
                              (.newInstance (Class/forName  s))))))
         (finally (if br (.close br))))))

(defn framework-context
  [framework]
    (.getBundleContext framework))

(defn create-framework
  [cfg]
    (let [factory (get-osgi-framework-factory)]
         (.newFramework factory cfg)))

(defn framework-init
  [framework]
    (.init framework))

(defn framework-start
  [framework]
    (.start framework))

(defn framework-stop
  [framework]
    (.stop framework))

(defn framework-backend
  [framework]
    (.waitForStop framework 0))

