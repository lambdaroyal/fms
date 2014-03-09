(defproject wus "0.1-SNAPSHOT"
  :description "LambdaRoyal Logistik Warenfluss Analyse"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.apache/poi "3.9"]
                 [org.clojure/data.json "0.2.2"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.apache.commons/commons-vfs2 "2.0"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.4"]
                 [aleph "0.3.0"]
                 [com.keminglabs/c2 "0.1.0"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [org.lambdaroyal/clojure-util "1.0-SNAPSHOT"]]
  :aot [lambdaroyal.logistics.c2server.core]
  :main lambdaroyal.logistics.c2server.core)

