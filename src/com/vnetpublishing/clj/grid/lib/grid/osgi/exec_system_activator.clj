(ns com.vnetpublishing.clj.grid.lib.grid.osgi.exec-system-activator
   (:gen-class
     :name com.vnetpublishing.clj.grid.lib.grid.osgi.ExecSystemActivator
     :state state
     :init init
     :methods [[setDispatchHandler [Object] void]
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

(defn -start
  [this ctx]
    (debug (str "Context =  " ctx))
    (future (binding [*osgi-context* ctx]
                     (osgi-boot)
                     (let [sw (StringWriter.)
                           bundle (.getBundle ctx)]
                          (.set this "_ctx" ctx)
                          (.set this "_bundle" bundle)
                          (debug (str "Found " (count (.getBundles ctx)) " bundles."))
                          (try (binding [*inc* (atom [""])
                                         *out* sw]
                                        ((.get this "_dispatch_handler")))
                               (println "#.")
                               (println "")
                               (println (.toString sw))
                               (catch Throwable 
                                      t 
                                      (let [ew (StringWriter.)]
                                           (binding [*out* ew]
                                                    (print-cause-trace t))
                                           (debug (.toString ew)))))
                          (framework-stop (.getBundle ctx))))))

(defn -stop
  [this ctx]
    (.set this "ctx" nil))

(defn -setDispatchHandler
  [this f]
    (.set this "_dispatch_handler" f))

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
             (let [properties (:properties @s)]
                  (swap! properties assoc p v)))))

(defn -postConstructHandler
  [this]
    nil)

(defn -getProperties
  [this]
    (deref (:properties (deref (.state this)))))

(defn -init
  []
    [[] (atom { :properties (atom {})})])
