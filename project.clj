(defproject clj-grid-kernel "0.1.0-SNAPSHOT"
  :description "Grid Kernel"
  :url "http://www.vnetpublishing.com"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.3.0"]
                 [org.apache.felix/org.apache.felix.main "4.4.1"]]
  :repositories [["releases" {:url "http://home.vnetpublishing.com/artifactory/libs-release-local"
                              :creds :gpg}]
                 ["snapshots" {:url "http://home.vnetpublishing.com/artifactory/libs-snapshot-local"
                               :creds :gpg}]]
  :aot :all)
