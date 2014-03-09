;; VREPL for Lambdaroyal Logistics Reports
;; Inspired by C2
(ns lambdaroyal.logistics.c2server.server
  (:use
    ;;Compojure is a small routing library for Ring that allows 
    ;;web applications to be composed of small, independent parts. 
    compojure.core
    [ring.middleware.file :only [wrap-file]]
    ;;Aleph is a Clojure framework for asynchronous communication, 
    ;;built on top of Netty and Lamina.
    [aleph.http :only [start-http-server
                       wrap-ring-handler 
                       wrap-aleph-handler]]
    ;;Lamina is for describing and analyzing streams of data. 
    ;;It provides a rich set of operators for dealing with these unrealized values, 
    ;;both individually and collectively.
    ;;with easy words: futures with callbacks
    [lamina.core :only [enqueue permanent-channel receive siphon]]
    ;;hiccup ist html abstraction per clojure - nice stuff
    [hiccup.core :only [html]]
    [clojure.data.json :only [read-json json-str]]
    [clojure.string :only [join]])
  (:require [compojure.route :as route]))


(def 
  ^{:doc "denotes the svg content to be rendered within all connected clients. just call swap! current-page set!)"} 
  current-page (atom [:div]))

(def ^{:doc "Options used to start the server"}
  opts (atom {:port 8080}))

(def broadcast-channel (permanent-channel))

;;Messages to livereload client.
;;http://help.livereload.com/kb/ecosystem/livereload-protocol
(defn lr-reload [path-to-reload]
  {:command "reload"
   :path path-to-reload
   :liveCSS true})

(defn lr-alert [msg]
  {:command "alert"
   :message msg})

(defn lr-hello []
  {:command "hello"
   :protocols ["http://livereload.com/protocols/official-7"]
   :serverName "zps.preissimulation.c2server"})

(defn livereload-handler
  "When a websocket client connects, this function runs."
  [ch handshake]
  (receive ch (fn [msg]
    (when (= "hello" (:command (read-json msg)))
          (println "Client connected.")
          ;;respond hello
          (enqueue ch (json-str (lr-hello)))
          ;;Subscribe it to events
          (siphon broadcast-channel ch)))))

(defroutes main-routes
  (GET "/" []
    (html [:html
            [:head
              [:script {:src (str "/livereload.js?port=" (:port @opts))}]
              [:style (join "\n" ["body { background-color: #222222; color: white;}"
                                  "h1 { text-align: center; font: 5em sans-serif; margin: 1em;}"])]]
              [:body @current-page]]))
  (GET "/livereload" []
    (wrap-aleph-handler livereload-handler))
  (route/resources "/"))

(add-watch current-page :reload
           (fn [_ _ _ _] (enqueue broadcast-channel (json-str (lr-reload "/")))))

(def app (-> main-routes (wrap-ring-handler)))

(defn ^{:doc "Starts the server using opts"} start-server []
  (start-http-server app {:port (:port @opts) :websocket true}))
