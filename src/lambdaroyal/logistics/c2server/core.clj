(ns lambdaroyal.logistics.c2server.core
  (:use [clojure.tools.cli :only [cli]])
  (:import org.apache.commons.vfs2.VFS
           org.apache.commons.vfs2.impl.DefaultFileMonitor
           org.apache.commons.vfs2.FileListener)
  (:require [lambdaroyal.logistics.sankey :as sankey]
            [lambdaroyal.logistics.goodsflowreader :as gdr]
            [lambdaroyal.logistics.c2server.server :as server])
  (:gen-class))

(defn output-xls
  "Output result of freshly reloaded xls file"
  [xls]
  (reset! server/current-page
          (sankey/render
            (gdr/load-xls xls))))

(defn reload-file [vfs-filename]
  (when-not (-> vfs-filename .getBaseName (.startsWith "."))
    (let [path (.getPath vfs-filename)]
      (println "reloading " path) 
      (case (.getExtension vfs-filename)
            "xls" (output-xls path)
            :else (println "I have no idea what to do with" path)))))

(defn monitor-files [file]
  "starts a listener that registers changes to files denoted by a certain path. the changes are forwarded to
  reload-file"
  (let [file-manager (VFS/getManager)
        fm (DefaultFileMonitor. 
              (reify FileListener
                (fileChanged [_ e]
                (reload-file (.getName (.getFile e))))
                (fileCreated [_ e])
                (fileDeleted [_ e])))]
    (.setBaseFile file-manager (java.io.File. "."))
    (.addFile fm (.toFileObject 
                  file-manager
                  file))
    (.setRecursive fm true)
    (.start fm)))

(defn start-server-and-monitor [path port]
  (do
    (println "WUS - Goods Flow Analysis Ver. 0.9 - build 20140301")
    (println "(c) 2013, 2014 by Lambda Royal and Rocklog (www.rocklog.ch)")
    (println "This instance license is granted to Rocklog. All rights reserved.")
    (println (format "start server on port %d ... \nopen browser an goto http://localhost:%d" port port))

    (server/start-server)
    (println (format "monitor path %s ..." path))
    (monitor-files path)))

(defn- path-to-file [path]
  "yields a path as a file and yields the uri. the path must either represent an existing file or an existing directory"
  (let [f (java.io.File. path)]
    (if (.exists f)
      f
      (throw (java.lang.IllegalArgumentException. (format "The value %s of the parameter --path does not denote a file or a directory. \nThe parameter --path must either denote an existing file or an existing directory. the path can either be relative or absolute. If the path denotes a directory then all files in the directory are observed for changes" path))))))

(defn -main [& args]
  (let [[{:keys [path port] :as opts} args banner] 
        (cli 
          args
          ["-h" "--help" "Show help" :default false :flag true]
          ["--path" "Path to watch (recursive)" :default (str (System/getProperty "user.dir") "/" "samples") :parse-fn path-to-file]
          ["--port" "Webserver port" :default 8987 :parse-fn #(Integer. %)])]
    (reset! server/opts opts)
    (start-server-and-monitor path port)))
    
