(ns com.vnetpublishing.clj.grid.lib.grid.kernel
  (:gen-class :name com.vnetpublishing.clj.grid.lib.grid.Kernel)
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [com.vnetpublishing.clj.grid.lib.grid.lang :as lang]
            [clojure.reflect])
  (:use [com.vnetpublishing.clj.grid.lib.grid.util])
  (:import [java.io File 
                    InputStreamReader]
           [java.net URI]
           [clojure.lang DynamicClassLoader
                         Compiler]
           [org.osgi.framework Constants]
           [java.util Date
                      ResourceBundle
                      Hashtable]))

; Context vars

(def ^:dynamic *debug-kernel* false)

(def ^:dynamic *ds* File/separator)

(def ^:dynamic *dispatch* false)

(def ^:dynamic *servlet-request* nil)

(def ^:dynamic *servlet-response* nil)

(def ^:dynamic *inc* (atom []))

(def ^:dynamic *grid-domains* (atom []))

(def ^:dynamic *linked-hosts* (atom {}))

(def ^:dynamic *osgi-context* nil)

(def ^:dynamic *local-web-root* "")

(def ^:dynamic *project-paths* [])

(def ^:dynamic *transaction* 
  (atom {:loaded (atom (set []))
         :class-data (atom {})}))

(def ^:dynamic *default-messages-resource-bundle* 
  (ResourceBundle/getBundle "Messages"))

(def ^:private modules 
  (atom {}))

(defn to-message
  "Resolve message using *default-messages-resource-bundle*"
  [k]
    (.getString  *default-messages-resource-bundle*
                 k))

(defmacro debug
  [msg]
    `(clojure.tools.logging/debug ~msg))

(defmacro warn
  [msg]
    `(clojure.tools.logging/warn ~msg))

(defmacro kdebug
  [msg]
    `(if com.vnetpublishing.clj.grid.lib.grid.kernel/*debug-kernel*
         (clojure.tools.logging/debug (str ~msg))))

(defmacro script
  [docstr & body]
    `(if com.vnetpublishing.clj.grid.lib.grid.kernel/*dispatch*
         (do (debug (str (com.vnetpublishing.clj.grid.lib.grid.kernel/to-message com.vnetpublishing.clj.grid.lib.grid.lang/LOGTOKEN_SCRIPT_RUN)
                         ": " 
                         ~docstr))
         ~(conj body 'do))))

(defmacro fscript
  [docstr & body]
    (let [fname (gensym "__fscript_")]
         `(defn ~(with-meta fname (assoc (meta fname) :fscript true)) 
               [] 
                 (com.vnetpublishing.clj.grid.lib.grid.kernel/debug (str (com.vnetpublishing.clj.grid.lib.grid.kernel/to-message com.vnetpublishing.clj.grid.lib.grid.lang/LOGTOKEN_FSCRIPT_RUN)
                                             ": " 
                                             ~docstr))
                 ~(conj body 'do)
                 true)))

(defmacro java-new-apply
  ([klass] `(new ~klass))
  ([klass args] `(new ~klass ~@(map eval args))))

(defn java-method-apply
  [obj method args]
    (let [c-args (if args (into [obj] args) [obj])
          n-args (count args)
          s-args (map-indexed (fn [i o] (symbol (str "x" i)))
                              args)
          f-args (conj (conj (conj s-args method) (symbol "ob")) '.)
          fi-args (cons (into [(symbol "ob")] (vec s-args)) 
                        (conj '() f-args))
          o-args (conj (list fi-args) 'fn*)
          f (eval o-args)]
         (apply f c-args)))

(defn method-exists?
  [ob name]
  (in? (map #(.getName %) 
            (.getDeclaredMethods (type ob))) 
       name))

(defn create-instance
  [cl c-args & i-args]
    (let [cl-name (.getName cl)
          ob (eval `(java-new-apply ~(symbol cl-name) ~c-args))
          ia-args (if i-args i-args [])]
         (if (method-exists? ob "postConstructHandler")
           (java-method-apply ob (symbol "postConstructHandler") i-args))
         ob))

(defn absolute-path?
  [path]
    (cond (string? path)
          (.isAbsolute (io/as-file path))
          (instance? URI path)
          (or (.getScheme path)
              (= "/" (subs path 0 1)))
          :else
          false))

(defn dirname
  [path]
    (or (.getParent (io/as-file path)) 
        ""))

(defn class-exists?
  [c]
    (clojure.reflect/resolve-class (.getContextClassLoader (Thread/currentThread)) 
                                   (symbol c)))

(defn class-to-path
  [cl]
  (let [name (if (string? cl) 
                 cl 
                 (.getName cl))]
       (clojure.string/join *ds* 
                            (clojure.string/split (munge name)
                                                  #"\."))))

(defn path-to-ns
  [path]
    (let [rel-path (loop [p path]
                     (if (not= (first p) \/)
                         p
                         (recur (subs p 1))))
          _ (debug (str "rel-path: " rel-path))
          ns-path (clojure.string/replace rel-path "_" "-")
          std-path (clojure.string/replace ns-path *ds* "/")
          path-parts (clojure.string/split std-path #"/")
          path-part-last-raw (last path-parts)
          path-last (if (.endsWith path-part-last-raw ".clj")
                        (subs path-part-last-raw 0 (- (count path-part-last-raw) 4))
                        (last path-parts))]
    (clojure.string/join "." (concat (butlast path-parts) [path-last]))))

(defn backtrace
  []
    (let [bt (.getStackTrace (Thread/currentThread))]
      (rest (rest bt))))

(defn get-project-resource
  [path]
    (let [l-path (clojure.string/replace path 
                                         "/"
                                         *ds*)
          paths (map (partial #(File. (str %2 *ds* %1)) l-path)
                     *project-paths*)
          f (first (filter #(.isFile %) paths))]
         (if f (.toURL (.toURI f)))))

(defn get-bundle-resource
  [path]
  (if *osgi-context*
      (let [bundles (.getBundles *osgi-context*)
            n-bundles (count bundles)]
           (kdebug (str "get-bundle-resource " path))
           
           (loop [n 0 r nil]
             (if (or (>= n n-bundles)
                     r)
                 r
                 (recur (+ n 1)
                        (or (.getResource (nth bundles n) path)
                            (.getResource (nth bundles n) (str "META-INF/resources/"
                                                               path)))))))))
             

(defn get-local-resource
  [path]
  (let [f (File. (clojure.string/replace path 
                                         "/"
                                         *ds*))]
    (if (.isFile f)
        (.toURL (.toURI f)))))



; Deploy Order
;
; index.jsp
; WEB-INF/resources/index.jsp
; WEB-INF/lib/*.jar:/META-INF/index.jsp
; osgi:somemod:/index.jsp
; http://www.myrepo.com/index.jsp

(defn get-resource
  [path]
    (let [r-path (.getPath (.relativize (.toURI (File. *ds*))
                                        (.toURI (File. (clojure.string/replace path
                                                                             "/"
                                                                             *ds*)))))]
          (or (get-local-resource r-path)
              (get-project-resource r-path)
              (io/resource (str "META-INF/resources/" r-path))
              (get-bundle-resource r-path))))

(defn glocate-absolute
  "Locate absolute resource"
  [path]
    (let [f (.getCanonicalFile (io/as-file path))]
         (if (.exists f)
             (do (kdebug (str (to-message lang/LOGTOKEN_FOUND_RESOURCE)
                                  " " 
                                  (.toString (.toURI f)))) 
                 (.toString (.toURI f)))
             (kdebug (str "!! glocate-absolute "
                          (to-message lang/LOGTOKEN_MISSING_RESOURCE)
                          " " 
                          f)))))

(defn glocate-relative
  "Locate resource relative to class"
  [path rel]
  (if rel
      (let [dirty-caller-path (dirname (class-to-path rel))
            caller-path (if (empty? dirty-caller-path) 
                            "." 
                            dirty-caller-path)
            r-path-dirty (str caller-path 
                              *ds*
                              path)
            
            r-path (-> (.relativize (.toURI (File. ""))
                                    (.toURI (File. r-path-dirty)))
                        (.toString))
            _ (kdebug (str "Checking "
                           r-path))
            caller-path-r (if caller-path (or (get-project-resource r-path)
                                              (io/resource r-path)
                                              (get-resource r-path)))
            cr (if caller-path-r
                   (.toString caller-path-r))]
        (if caller-path-r
            (kdebug (str "glocate-relative "
                         (to-message lang/LOGTOKEN_FOUND_RESOURCE)
                         " "
                         cr)))
        cr)))

(defn glocate-in-environment
  "Locate resource in environment"
  [path]
    (let [upath (clojure.string/replace path
                                        *ds* 
                                        "/")
          incs (deref *inc*)
          _ (if *debug-kernel*
              (debug (str (count incs) ":" incs)))
          incs-n (count incs)]
         (loop [n 0 r nil]
             (if (or r (>= n incs-n))
                 (if r
                     (do (if *debug-kernel* 
                             (debug (str "glocate found " (.toString r))))
                             (.toString r))
                                  (if *debug-kernel* 
                                      (debug (str "!! glocate-in-environment "
                                                  (to-message lang/LOGTOKEN_MISSING_RESOURCE)
                                                  " "
                                                  path))))
                              (recur (+ n 1)
                                     (let [base-path (nth incs n)
                                           base-uri (if (absolute-path? base-path)
                                                        (-> (File. base-path)
                                                            (.toURI)
                                                            (.toString))
                                                        (-> (.relativize (.toURI (File. ""))
                                                                         (.toURI (File. base-path))) 
                                                            (.toString)))
                                           test-r (if (empty? base-uri)
                                                      upath
                                                      (str base-uri "/" upath))]
                                          (kdebug (str "glocate "
                                                       (to-message lang/LOGTOKEN_CHECKING)
                                                       " " 
                             test-r))
                (io/resource test-r)))))))

(defn glocate-in-bundles
  [path]
  (if *osgi-context*
      (let [incs (deref *inc*)
            incs-n (count incs) 
            bundle-path (clojure.string/replace path *ds* "/")]
        (loop [n 0 r nil]
              (if (or r (>= n incs-n))
                  (if r
                      (do (kdebug (str "glocate-in-bundles "
                                       (to-message lang/LOGTOKEN_FOUND_RESOURCE)
                                       "" 
                                       (.toString r)))
                          (.toString r)))
                  (recur (+ n 1)
                         (let [rl (first (filter (partial #(.getResource %2 %1) 
                                                          (str (nth incs n)
                                                               "/"
                                                               bundle-path)) 
                                                 (.getBundles *osgi-context*)))]
                              (if rl
                                  (.getResource rl
                                                (str (nth incs n)
                                                               "/"
                                                               bundle-path))))))))))

(defn net-resource-exists?
  [r]
  (try (if r
           (let [c (.openConnection r)]
             (.setRequestMethod c "HEAD")
             (= 200 (.getResponseCode c))))
           (catch Throwable t nil)))

(defn glocate-in-grid-domain
  [path domain]
  (let [upath (clojure.string/replace path
                                      *ds* 
                                      "/")
        host-paths (get (deref *linked-hosts*) domain)
        incs-n (count host-paths)]
    (loop [n 0 r nil]
      (if (or r (>= n incs-n))
          (if r
              (do (kdebug (str "glocate-in-grid-domain "
                                       (to-message lang/LOGTOKEN_FOUND_RESOURCE)
                                       "" 
                                       (.toString r)))
                          (.toString r))
              (recur (+ n 1)
                     (let [rl (java.net.URL (str (nth host-paths n)
                                                 "/"
                                                 upath))]
                              (if (net-resource-exists? rl)
                                  rl))))))))


(defn glocate-in-grid
  "Locate resource in grid"
  [path]
  (let [dl (deref *grid-domains*)
        dc (count dl)]
       (loop [n 0 dr nil]
         (if (or dr
                 (>= n dc))
             dr
             (recur (+ 1 n)
                    (glocate-in-grid-domain path (nth dl n)))))))

(defn gnslocate
  "Locate grid resource with namespace"
  [path base-ns & [rel-in]]
  (kdebug (str "gnslocate " path " " rel-in))
  (if *dispatch*
      (if (absolute-path? path)
          (list (glocate-absolute path) base-ns)
          (or (let [lr (glocate-relative path 
                                         (or rel-in
                                             (.getClassName (second (backtrace)))))]
                   (if lr (list lr base-ns)))
              (let [lr (glocate-in-environment path)]
                   (if lr (list lr base-ns)))
              (let [lr (glocate-in-bundles path)]
                   (if lr (list lr base-ns)))
              (let [lr (glocate-in-grid path)]
                   (if lr (list lr base-ns)))
              (do (debug (str "!! gnslocate "
                              (to-message lang/LOGTOKEN_NOT_FOUND)
                              " "
                              path
                     " "
                     base-ns))
                  nil)))
      (warn (str "gnslocate "
                 (to-message lang/LOGTOKEN_NO_DISPATCH)))))

 ;Priority:

 ;1. Absolute path
;2. Relative to caller
;3. Relative to *inc* paths using env class loader 
;4. Relative to *inc* paths using bundle class loaders
;5. Relative to *inc* paths using *grid-domains* ~> *linked-hosts*

(defn glocate
  "Locate grid resource"
  [path & [rel-in]]
    (first (gnslocate path
                      nil
                      (or rel-in
                          (.getClassName (second (backtrace)))))))

(defn load-resource
  [r]
    (if (= "file" (.getScheme r))
        (do (kdebug (str "load-file " (.getPath r)))
            (load-file (.getPath r)))
        (do (kdebug "load-reader")
            (load-reader (InputStreamReader. (.openStream (.toURL r)))))))

(defn run-fscripts
  [script-ns]
    (doseq [[s v] (doall (filter #(:fscript (meta (second %))) 
                                 (ns-publics (symbol script-ns))))]
           (v)))

(defn ginc
  "Include grid resource"
  [path]
  (kdebug (str "ginc " path))
  (if *dispatch*
      (let [c (glocate path 
                       (.getClassName (second (backtrace))))
            r (if c (java.net.URI. c))
            i (:loaded (deref *transaction*))]
           (if c
               (do (swap! i conj c)
                   (load-resource r))
               (warn (str "ginc "
                          path
                          " "
                          (to-message lang/LOGTOKEN_NOT_FOUND)))))
      (warn (str "ginc "
                 (to-message lang/LOGTOKEN_NO_DISPATCH)))))

(defn fload
  [path r-ns]
    (let [r (URI. path)
          i (:loaded (deref *transaction*))]
         (swap! i conj path)
         (load-resource r)
         (when-let [cur-ns (find-ns r-ns)]
                   (alter-meta! cur-ns 
                                assoc
                                :fload-timestamp
                                (.getTime (Date.))))))

(defn funload
  [path r-ns]
    (let [i (:loaded (deref *transaction*))]
         (remove-ns r-ns)
         (swap! i disj path)))

(defn fscript-expired?
  [path r-ns]
    (let [ftimestamp (:fload-timestamp (meta (find-ns r-ns)))
          r (URI. path)
          f (if (= "file" (.getScheme r))
                (File. r))
          ]
         (and ftimestamp
              f
              (> (.lastModified f)
                 ftimestamp))))

(defn finc
  "Function Include grid resource"
  [path base-ns]
    (kdebug (str "finc " path " " base-ns))
    (if *dispatch*
        (let [[c rns] (gnslocate path
                                 base-ns
                                     (.getClassName (second (backtrace))))
              rns-sym (symbol rns)
              i (:loaded (deref *transaction*))]
             (if (and c
                      (find-ns rns-sym)
                      (fscript-expired? c rns-sym))
                  (funload c rns-sym))
             (if (and c
                      (not (in? (deref i) c))
                      (not (find-ns rns-sym)))
                 (fload c rns-sym))
                 (if (and c
                      (find-ns rns-sym))
                 (do (if (not (in? (deref i) c))
                         (swap! i conj c))
                     (run-fscripts rns))
                     (warn (str "finc "
                                path 
                                " "
                            base-ns
                            " "
                                (to-message lang/LOGTOKEN_NOT_FOUND)))))
    (warn (str "finc "
                 (to-message lang/LOGTOKEN_NO_DISPATCH)))))

(defn frequire
  "Function Include grid resource"
  [path base-ns]
    (kdebug (str "finc " path " " base-ns))
    (if *dispatch*
        (let [[c rns] (gnslocate path
                                     base-ns
                                     (.getClassName (second (backtrace))))
              rns-sym (symbol rns)
              i (:loaded (deref *transaction*))]
              (if (and c
                       (find-ns rns-sym)
                       (fscript-expired? c rns-sym))
                  (funload c rns-sym))
              (if (and c
                       (not (in? (deref i) c))
                       (not (find-ns rns-sym)))
                     (fload c rns-sym))
                 (if (and c
                          (find-ns rns-sym))
                     (do (if (not (in? (deref i) c))
                             (swap! i conj c))
                         (run-fscripts rns))
                     (throw (Exception. (str "frequire "
                                             path 
                                             " "
                                             base-ns
                                             " "
                                             (to-message lang/LOGTOKEN_NOT_FOUND))))))
        (warn (str "frequire "
                   (to-message lang/LOGTOKEN_NO_DISPATCH)))))

(defn grequire
  "Require grid resource"
  [path]
  (kdebug (str "grequire " path))
  (if *dispatch*
      (let [bt (backtrace)
            c (glocate path 
                       (.getClassName (second bt)))
            r (if c (java.net.URI. c))
            i (:loaded (deref *transaction*))]
           (if c
               (do (swap! i conj c) 
                     (load-resource r))
               (throw (Exception. (str "grequire " 
                          path 
                          " "
                          (to-message lang/LOGTOKEN_NOT_FOUND))))))
      (throw (Exception. (str "grequire "
                 (to-message lang/LOGTOKEN_NO_DISPATCH))))))


(defn ginc-once
  "Include grid resource once"
  [path]
  (if *debug-kernel* 
      (debug (str "ginc-once " path)))
  (if *dispatch*
      (let [c (glocate path (.getClassName (second (backtrace))))
            r (if c (java.net.URI. c))
            i (:loaded (deref *transaction*))]
           (if c
               (if (not (in? (deref i) c))
                   (do (swap! i conj c)
                       (load-resource r))
               (warn (str "ginc-once " 
                          path 
                          " "
                          (to-message lang/LOGTOKEN_NOT_FOUND))))))
      (warn (str "ginc-once "
                 (to-message lang/LOGTOKEN_NO_DISPATCH)))))


(defn finc-once
  "Function Include grid resource"
  [path base-ns]
    (kdebug (str "finc-once " path " " base-ns))
    (if *dispatch*
        (if (find-ns (symbol base-ns))
            (run-fscripts base-ns)
            (let [[c rns] (gnslocate path
                                     base-ns
                                     (.getClassName (second (backtrace))))
                  rns-sym (symbol rns)
                  r (if c (java.net.URI. c))
                  i (:loaded (deref *transaction*))
                  nal (if c (not (in? (deref i) c)))]
                 (if (and c
                          nal
                          (not (find-ns rns-sym)))
                     (do (swap! i conj c)
                         (load-resource r)))
                 (if (and c
                          (find-ns rns-sym))
                     (if nal
                         (do (if (not (in? (deref i) c))
                                 (swap! i conj c))
                             (run-fscripts rns)))
                     (warn (str "finc-once " 
                                path 
                                " "
                                base-ns
                                " "
                                (to-message lang/LOGTOKEN_NOT_FOUND))))))
        (warn (str "finc-once "
                   (to-message lang/LOGTOKEN_NO_DISPATCH)))))

(defn conjure
  "Conjure Grid Resource"
  [path source-path]
  (if *debug-kernel* 
      (debug (str "conjure " path " to " source-path)))
  (if *dispatch*
      (let [c (glocate path (.getClassName (second (backtrace))))
            r (if c (java.net.URI. c))
            i (:loaded (deref *transaction*))]
           (if c
               (if (not (in? (deref i) c))
                   (do (swap! i conj c)
                      (kdebug (str "Compiler/compile")) 
                       (binding [*compile-files* true]
                                (Compiler/compile (InputStreamReader. (.openStream (.toURL r)))
                                                  source-path
                                                  "CONJURE_FILE")))
               (warn (str "conjure " 
                          path 
                          " "
                          (to-message lang/LOGTOKEN_NOT_FOUND))))))
      (warn (str "conjure "
                 (to-message lang/LOGTOKEN_NO_DISPATCH)))))


(defn add-include-path
  [path]
    (if (and *inc* (not (in? (deref *inc*) path)))
        (swap! *inc* into [path])))



(defn install-local-bundle
  [filename]
  (let [path (.toURI (File. (str *local-web-root* (clojure.string/replace filename "/" *ds* ))))
        url (.toURL path)
        _ (kdebug (str "Installing local bundle: "
                  (.toString path)))
        bundle (.installBundle *osgi-context* 
                               (.toString path)
                               (.openStream url))]
       (kdebug (str "Starting local bundle: "
                    (.toString path)))
       (let [ac-name (.get (.getHeaders bundle)
                           Constants/BUNDLE_ACTIVATOR)
             _ (kdebug (str "Activator: " ac-name))
             ac (.loadClass bundle ac-name)
             _ (kdebug (str "Activator Class: " ac))
             bcl (.getClassLoader ac)
             _ (kdebug (str "Bundle Class Loader: " bcl))
             ccl (.getContextClassLoader (Thread/currentThread))
             dcl (DynamicClassLoader. bcl)]
            (try (with-bindings {Compiler/LOADER dcl}
                   (.start bundle))
                 (finally (.setContextClassLoader (Thread/currentThread)
                                                  ccl))))
            
       (kdebug (str (str "Bundle "
                         (.toString path)
                           " started")))))

(defn uninstall-local-bundle
  [filename]
    (.uninstall (.getBundle *osgi-context*
                            (.toString (.toURI (File. filename))))))

(defn gen-transaction-state
  "Generate a transaction state"
  ([]
    (atom {:loaded (atom (set []))
           :class-data (atom {})}))
  ([c]
    (atom (merge c 
                 {:loaded (atom (set []))
                  :class-data (atom {})}))))

(defn get-ob-transaction-state
  [ob]
    (let [cur (get @*transaction* (.getName (type ob)))]
      (if cur
          cur
          (let [n (atom {})]
            (swap! *transaction* assoc (.getName (type ob)) n)
            n))))

(defmacro tglobal-get
  [v]
  `(get (deref (com.vnetpublishing.clj.grid.lib.grid.kernel/get-ob-transaction-state (symbol "symbol"))) ~v))

(defmacro tglobal-set
  [k v]
  `(swap! (com.vnetpublishing.clj.grid.lib.grid.kernel/get-ob-transaction-state (symbol "symbol")) assoc ~k ~v))


(defn get-module
  [sym]
  (if (get @modules sym)
    (get @modules sym)
    (let [mod-class (try (resolve sym)
                         (catch Throwable t nil))
          mod-name (name sym)]
      (if mod-class
          (do (swap! modules assoc sym (create-instance mod-class []))
              (if *osgi-context*
                  (.registerService *osgi-context* 
                                    mod-name
                                    (get @modules sym)
                                    (let [t (Hashtable.)]
                                         (.put t "description" "module")
                                         t)))
              (get @modules sym))
          ; look through bundles
          (let [bundles (if *osgi-context*
                            (.getBundles *osgi-context*)
                            (do (debug "OSGI Context missing???")
                                '()))
                n-bundles (count bundles)]
                (loop [n 0 r nil]
                      (if (or (>= n n-bundles)
                              r)
                          (if r
                              (do (kdebug (str "Found module "
                                               mod-name))
                                   (.getService *osgi-context*
                                                r))
                              
                              (do (kdebug (str "WARNING: Module "
                                               mod-name
                                               " Not found!!!"))
                                  nil))
                          (recur (+ n 1)
                                 (do (kdebug (str "Checking bundle "
                             (.getSymbolicName (nth bundles n)) 
                             " for module "
                             mod-name))
                                     (.getServiceReference (.getBundleContext (nth bundles n))
                                                           mod-name))))))))))

(defn call-other
  [ob f-sym & args]
    (let [ccl (.getContextClassLoader (Thread/currentThread))
          dcl (.getClassLoader (class ob))]
               (try (with-bindings {Compiler/LOADER dcl}
                                   (.setContextClassLoader (Thread/currentThread) dcl)
                                   (java-method-apply ob f-sym args))
                   (finally (.setContextClassLoader (Thread/currentThread)
                                                    ccl)))))

(defn servlet-request-resource-path
  []
  (str "/"
       (.toString (.relativize (URI. (.getContextPath *servlet-request*))
                               (URI. (.getRequestURI *servlet-request*))))))
