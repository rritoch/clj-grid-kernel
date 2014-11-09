(ns com.vnetpublishing.clj.grid.lib.grid.osgi.webapp-system-activator
   (:gen-class
     :name com.vnetpublishing.clj.grid.lib.grid.osgi.WebappSystemActivator
     :state state
     :init init
     :methods [[setStartHandler [Object] void]
               [get [Object] Object]
               [get [Object Object] Object]
               [set [Object Object] void]
               [postConstructHandler [] void]]
     :implements [org.osgi.framework.BundleActivator])
   (:import [java.io StringWriter])
   (:use [com.vnetpublishing.clj.grid.lib.grid.osgi.functions]
         [clojure.stacktrace]
         [com.vnetpublishing.clj.grid.lib.grid.kernel]
         [com.vnetpublishing.clj.grid.lib.grid.util]))

(def OSGI_CONTEXT_ATTRIBUTE 
  "com.vnetpublishing.clj.grid.lib.grid.osgi.osgi-context")

(defn -start
  [this ctx]
  (future (binding [*osgi-context* ctx]
                   (osgi-boot)
           (let [sw (StringWriter.)
                 bundle (.getBundle ctx)
                         start-handler (.get this "_start_handler")]
                 (.set this "_ctx" ctx)
                 (.set this "_bundle" bundle)
                 (debug (str "Found " (count (.getBundles ctx)) " bundles."))
                         (debug (str "Start handler:" start-handler))
                 (if start-handler
                             (start-handler ctx))))))

(defn -stop
  [this ctx]
    (.set this "ctx" nil))

(defn -setStartHandler
  [this handler]
    (.set this "_start_handler" handler))

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

