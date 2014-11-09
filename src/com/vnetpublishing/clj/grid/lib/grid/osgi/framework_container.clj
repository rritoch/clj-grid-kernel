(ns com.vnetpublishing.clj.grid.lib.grid.osgi.framework-container
  (:gen-class 
    :name com.vnetpublishing.clj.grid.lib.grid.osgi.FrameworkContainer
    :state state
    :init init
    :methods [[setConfigVar [Object Object] void]
              [setDispatchHandler [Object] void]
              [get [Object] Object]
              [get [Object Object] Object]
              [set [Object Object] void]
              [postConstructHandler [] void]
              [launch [org.osgi.framework.BundleActivator] void]])
  (:import [org.apache.felix.main AutoProcessor])
  (:use [com.vnetpublishing.clj.grid.lib.grid.osgi.functions]
        [com.vnetpublishing.clj.grid.lib.grid.kernel]
        [com.vnetpublishing.clj.grid.lib.grid.util]))

(defn -launch
  [this l]
    (debug "Initiating OSGi Launch")
    (try (let [m_fwk (create-framework (.get this "_config_vars"))]
              (.set this "m_fwk" m_fwk)
              (framework-init m_fwk)
              (framework-start m_fwk)
              (if l
                  (future (.start l (framework-context m_fwk))))
              (framework-backend m_fwk))
         (catch Exception ex
                (debug (str "Could not create osgi framework: " ex))
                (.printStackTrace ex)
                (System/exit -1))))

(defn -setConfigVar
  [this name value]
    (.set this 
          "_config_vars" 
          (assoc (or (.get this "_config_vars")
                     {})
                 name 
                 value)))

(defn -get 
  ([this p d]
    (let [n (str "get" (ucfirst p))
          s (.state this)]
        (if (method-exists? this n)
            (. n this)
            (or (get (deref (:properties @s)) p)
                d))))
  ([this p] (.get this p nil)))

(defn -set
  [this p v]
    (let [n (str "set" (ucfirst p))
          s (.state this)]
         (if (method-exists? this n)
             (. n this v)
             (swap! (:properties @s) assoc p v))))

(defn -postConstructHandler
  [this]
    nil)

(defn -getProperties
  [this]
    (deref (:properties (deref (.state this)))))

(defn -init
  []
    [[] (atom { :properties (atom {})})])
       