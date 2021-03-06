(defproject clj-grid-kernel "0.1.0-SNAPSHOT"
  :description "Grid Kernel"
  :url "http://www.vnetpublishing.com"
  :dependencies [[org.clojure/clojure "1.7.0-alpha4"]
                 [org.clojure/tools.logging "0.3.0"]
                 [org.apache.felix/org.apache.felix.main "4.4.1"]]
  ;;:repositories [["releases" {:url "http://home.vnetpublishing.com/artifactory/libs-release-local"
  ;;                            :creds :gpg}]
  ;;               ["snapshots" {:url "http://home.vnetpublishing.com/artifactory/libs-snapshot-local"
  ;;                             :creds :gpg}]]
  ;:javac-options ["-target" "1.7" "-source" "1.7" "-Xlint:-options"]
  :aot :all)
