;; Author: christian.meichsner@informatik.tu-chemnitz.de

(ns lambdaroyal.logistics.sankey 
  (:use [c2.core :only [unify]])
  (:require [c2.scale :as scale])
  (:require [clojure.set :as set]))


(def ^{:doc "used for rendering the bubbles representing the various articles in the areas"} article-colors
  ["#c04c01" "#c08301" "#c0c001" "#72c001"])

(def goods-flow
  {:dimension
   {:dy 1000}
   :topology
   ;;array of zones
   [{:name "pool" :dx 300
     :areas [{:name "inbound" :dy 500}
             {:name "outbound"}]}
    {:name "lift" :dx 200
     :areas [{:name "lift 1&2"}]}
    {:name "commission" :dx 500
     :areas [{:name "warm" :dy 200 :separated true}
             {:name "kalt" :dy 400 :separated true}
             {:name "tiefkühl" :separated true}]}]
   :article-groups
   #{"Betriebsstoffe" "Rohstoffe" "Werkzeuge"}
   
   :goods-flow
   {"Betriebsstoffe"
    [ ["inbound" "lift 1&2" 200] ["inbound" "outbound" 10] 
    ["lift 1&2" "warm" 50] ["lift 1&2" "kalt" 70] ["lift 1&2" "tiefkühl" 80]
    ["tiefkühl" "kalt" 60] ["tiefkühl" "lift 1&2" 20] 
    ["warm" "outbound" 50] ["kalt" "outbound" 70] ["lift 1&2" "outbound" 10]]
    
    "Rohstoff"
    [["inbound" "outbound" 500]]
    
    }})


(def css
  "text {font: 12px sans-serif;}
  .zone {
    fill: lightblue;
  }
  
  .zone-text {
    fill: #777777;
    font-weight: bold;
  }

  .area {
    fill: #aaaaaa;
  }

  .area-separator {
    stroke: white;
  }

  .area-text {
    fill: #444444;
  }
  ") 

(let [;;scaling to visible pane
      width 800
      height 400
      min-x 0
      min-y 0
      zone-top-inline 35
      zone-left-right-inline 1
      max-x (apply + min-x (map :dx (-> goods-flow :topology)))
      max-y (-> goods-flow :dimension :dy)
      scale-x (scale/linear :domain [min-x max-x]
                      :range [0 width])
      scale-y (scale/linear :domain [min-y max-y]
                      :range [0 height])
      reduce-areas (fn [zone]
                     (:areas (reduce
                       (fn [acc i]
                         (let [max-y (- max-y zone-top-inline)
                               start-y (+ (:start-y acc) (if 
                                                                 (:dy i)
                                                                 (:dy i)
                                                                 (- max-y (:start-y acc))))
                               start-y (min max-y start-y)]
                          (assoc acc :start-y start-y
                                :areas (conj (:areas acc)
                                             {:x (+ (:x zone) zone-left-right-inline)
                                              :y (-> (:start-y acc) scale-y double)
                                              :width (- (:width zone) (* 2 zone-left-right-inline))
                                              :height (-> (- start-y (:start-y acc)) scale-y double)
                                              :name (:name i)
                                              :separated (:separated i)}))))
                       {:start-y zone-top-inline :areas []} (:areas zone))))
      ;;render zones
      zones (reduce
              (fn [acc i]
                (assoc acc :start-x (+ (:start-x acc) (:dx i))
                           :zones (conj (:zones acc) 
                                    {:x (-> (:start-x acc) scale-x double)
                                     :y (-> min-y scale-y double)
                                     :width (-> (:dx i) scale-x double)
                                     :height (-> max-y scale-y double)
                                     :name (:name i)
                                     :areas (:areas i)})))
              {:start-x 0 :zones []} (-> goods-flow :topology))
      zones (map #(assoc % :areas (reduce-areas %)) (:zones zones))

      ;;check which article groups we have for each area
      areas (reduce concat (map :areas zones))
      area-set-by-article (reduce
                            #(assoc % %2
                                    (set
                                      (concat
                                        (map first (get (:goods-flow goods-flow) %2))
                                        (map first (get (:goods-flow goods-flow) %2)))))
                            {} (keys (:goods-flow goods-flow)))
      article-set-by-area (apply concat
                                 (map (fn [[k v]]
                                        (zipmap v (repeatedly (fn [] k))))
                                      area-set-by-article))
      article-set-by-area (reduce
                            (fn [acc [area article]]
                              (let [existing (get acc area)]
                                (assoc acc area (if existing (conj existing article)
                                                  #{article}))))
                            {} article-set-by-area)

      ;;
      ;;now we calculate the relevant positions of each article within all the areas
      article-position-area-invariant (let [articles (keys (:goods-flow goods-flow))
                                            article-index (zipmap articles (range))
                                            start (- (/ (dec (count articles)) 2))]
                                        (reduce (fn [acc [k v]]
                                                  (assoc acc k (+ start v)))
                                                {} article-index))
      ;;calc area center
      area-center (reduce
                    (fn [acc i]
                      (assoc acc (:name i)
                             {:x (+ (:x i) (double (/ (:width i) 2)))
                              :y (+ (:y i) (double (/ (:height i) 2)))}))
                    {} areas)

      ;;ready to calc article center per area
      article-position-by-area (reduce
                                 (fn [acc [area center]]
                                   (assoc acc area
                                          (reduce (fn [acc article]
                                                 (assoc acc article 
                                                        {:x (+ (:x center) (* 20 (get article-position-area-invariant article)))
                                                         :y (+ (:y center) (* 20 (get article-position-area-invariant article)))
                                                         })) 
                                               {} (get article-set-by-area area))))
                                 {} area-center)

      article-positions (apply concat (map (fn [val] (map identity val)) (vals article-position-by-area)))

      ;;assign colors to articles
      color-by-article (zipmap (keys (:goods-flow goods-flow)) article-colors)

      ! (doseq [r zones] (println r))
      ! (println area-center)
      ! (println article-position-by-area)
      ! (println :article-positions article-positions)
      ]

  [:svg#main {:style (str
                      "background: #ffffff; " 
                      "display: block; "
                      "margin: auto;"
                      "height:" height ";"
                      "width:" width ";")}
   [:style {:type "text/css"} css]

   ;;render zones
   (unify 
     (map #(dissoc % :name) zones) 
     (fn [val]
       [:rect (merge val {:class "zone" :stroke "#a0a0a0" :stroke-width 1 :rx 8 :ry 8})]))
    
   ;;zone names
   (unify
     zones
     (fn [val]
       [:text {:class "zone-text" :x (+ 10 (:x val)) :y (+ 12 (:y val))} (:name val)]))
   
   ;;render areas
   (unify
     (reduce concat (map :areas zones))
     (fn [val]
       [:rect (merge val {:class "area"})]))

  ;;render area separation rules
  (apply concat
         (map
           #(unify
             (drop-last %)
              (fn [val]
                (if (:separated val)
                  [:line {:class "area-separator" 
                          :x1 (:x val) :x2 (+ (:x val) (:width val)) 
                          :y1 (+ (:y val) (:height val)) :y2 (+ (:y val) (:height val))}]
                  ;;else
                  nil)) )
           (map :areas zones)))
   
   (unify
     (reduce concat (map :areas zones))
     (fn [val]
       [:text {:class "area-text" :x (+ 10 (:x val)) :y (+ 12 (:y val))}
        (:name val)]))
 
   ;;render article bubbles
   (unify
     article-positions
     (fn [val]
       [:circle {:cx (-> val second :x) :cy (-> val second :y) :r 20 :fill (get color-by-article (first val))}]))
        
])
