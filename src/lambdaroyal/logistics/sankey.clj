;; Author: christian.meichsner@informatik.tu-chemnitz.de

(ns lambdaroyal.logistics.sankey 
  (:use [c2.core :only [unify]])
  (:require [c2.scale :as scale])
  (:require [clojure.set :as set]))


(def ^{:doc "used for rendering the bubbles representing the various articles in the areas"} article-colors
  ["#c04c01" "#c08301" "#c0c001" "#72c001"])

(def ^:const invariants
  {:curve-radius 30})



;; definition of goods flow
(def goods-flow
  {:title "Warenflussanalyse Mars Roboter Fabs Inc. (Sep. 2013)"
   :article-radius 10
   :dimension
   {:dy 1000}
   :topology
   ;;array of zones
   [{:name "pool" :dx 300
     :areas [{:name "inbound" :dy 500 :separated true}
             {:name "outbound" :separeted true}
             ]}
    {:name "lift" :dx 200
     :areas [{:name "lift 1&2"}]}
    {:name "förderlift" :dx 200
     :areas [{:name "lift 3"}]}
    {:name "commission" :dx 300
     :areas [{:name "warm" :dy 200 :separated true}
             {:name "kalt" :dy 400 :separated true}
             {:name "tiefkühl" :separated true}]}]
   :article-groups
   #{"Betriebsstoffe" "Rohstoffe" "Werkzeuge"}
   
   :goods-flow
   {"Betriebsstoffe"
    [ ["inbound" "lift 1&2" 200]  
    ["lift 1&2" "warm" 50]  ["lift 1&2" "tiefkühl" 80]
    ["tiefkühl" "kalt" 60] ["tiefkühl" "lift 1&2" 20] 
      ["lift 1&2" "outbound" 10]]
    
    "Rohstoff"
    [["inbound" "outbound" 500]];;["inbound" "lift 1&2" 300]["lift 1&2" "kalt" 300]]
    "Werkzeuge"
    [["inbound" "lift 1&2" 100] ["kalt" "tiefkühl" 300] ]
    }})



















;;scales the throughtput of all dataflows into [1.. :article-radius]
(def scale-throughtput 
  (let [t (map last (reduce concat (map (fn [[k v]]
                                         (map #(cons k %) v)) (:goods-flow goods-flow))))]
    (scale/linear 
               :domain [(apply min t) (apply max t)]
               :range [(max 5 (/ (:article-radius goods-flow) 5)) (* 2 (:article-radius goods-flow))])))

;;dynamic list comprehension - fine stuff
(defmacro dyn-for [xs]
  `(let [;; index set of tupel (input[i], [0..][i])
         sym-index# (zipmap (repeatedly (fn [](gensym))) ~xs)
         ;;alternating collection (index symbol, range, index symbol, range, ..., index symbol, range)
         keyvals# (reduce
                    #(conj % (first %2) (second %2))
                    [] sym-index#)
         ;;for definition
         fd# (list `for keyvals# (vec (reverse (map first sym-index#))))]
     (eval fd#)))

;;this function optimizes the positions of area centern in terms of minimization
;;of y deltas of area tupels being associated by stock flow
(defn- optimize-area-center [goods-flow areas area-center]
  (let [y-range (-> (map :y (vals area-center)) distinct sort)
        range-per-area (zipmap areas (repeatedly (fn [] y-range)))
        ;;sort out y vals that are out of area border
        range-per-area (map (fn [[k v]]
                              [k (vec (filter 
                                        #(and
                                            (>= % (:y k))
                                            (<= % (+ (:y k) (:height k)))) v))]) range-per-area)
        ;;dynamic list comprehension
        range-space (dyn-for (vec (map second range-per-area)))
        min-range (first (sort-by (juxt #(-> % distinct count) #(apply + %)) range-space))
        y-by-area (zipmap (keys area-center) min-range)
        area-center (reduce #(assoc %1 %2 (assoc (get area-center %2) :y (get y-by-area %2))) {} (keys area-center))
        ]
    area-center))

(defn- ring [from to positiv]
 "this function calculates a rectangular path from :from to :to and returns this path as vector of points" 
 (cond (and (not= (:x from) (:x to))
            (not= (:y from) (:y to)))
       (let [direction (*
                        (- (:y to) (:y from))
                        (- (:x to) (:x from)))
             direction (if positiv direction (- direction))
             inter (if (> direction 0)
                       {:x (:x to) :y (:y from)}
                       {:x (:x from) :y (:y to)})]
         [from inter to])
       ;;y is the same (goldenen schnitt anwenden)
       (= (:y from) (:y to))
       (let [diverge (/ (- (:x from) (:x to)) 3)
             diverge (if positiv diverge (- diverge))
             inter {:x (:x from) :y (+ diverge (:y from))}]
         (vec (concat [from] (ring inter to positiv))))
       :else
       (let [diverge (/ (- (:y from) (:y to)) 3)
             diverge (if positiv diverge (- diverge))
             inter {:y (:y from) :x (- (:x from) diverge)}]
         (vec (concat [from] (ring inter to positiv))))))

(defn- ascent [line]
  "calcs the ascent of a line"
  (let [dx (- (-> line second :x) (-> line first :x))
        dy (- (-> line second :y) (-> line first :y))]
    (if (= 0 dx)
      nil
      (/ dy dx))))

(defn- line-n [line m]
  "calc vertical bias of a line"
  (- (-> line first :y) (* m (-> line first :x))))

(defn- ascent-transpo [line]
  "calcs the transponed ascent of a line"
  (let [dy (- (-> line first :x) (-> line second :x))
        dx (- (-> line first :y) (-> line second :y))]
    (if (= 0 dx)
      nil
      (/ dy dx))))

(defn- line-n-transpo [reference m]
  (- (-> reference :y) (* m (-> reference :x))))

(defn- in-range? [line dim value]
  "returns true if value is in range of dim :x/:y of line"
  (and
    (>= value (min (-> line first dim) (-> line second dim)))
    (<= value (max (-> line first dim) (-> line second dim)))))

(defn- intersect? [line1 line2]
  "checks whether line1 and line2 have an intersection."
  (let [m1 (ascent line1)
        m2 (ascent line2)]
    (cond (= m1 m2)
          false
          :else
          (let [l1 (if (nil? m1) line2 line1)
                l2 (if (nil? m1) line1 line2)
                m1 (if (nil? m1) m2 m1)
                m2 (if (nil? m1) m1 m2)
                ]
            (cond 
              ;; l2 vertical
              (nil? m2)
              (in-range? l1 :x (-> l2 first :x))
              (= m1 m2)
              false
              ;; std case
              :else
              (let [n1 (line-n l1 m1)
                    n2 (line-n l2 m2)
                    x (/ (- n2 n1)
                         (- m1 m2))]
                (and
                  (in-range? l1 :x x)
                  (in-range? l2 :x x))))))))

(defn- model-article-flow [xs article-flow]
  "models an article flow using xs ({area -> {article -> {:x :y}}}) and 
  article-flow which is '('(article from to amount)*).
  result is '('(article from to amount [[{:x :y}{:x :y}][{:x :y}{:x :y}{:x :y}{:x :y}]]))"
 (map
  (fn [[article from to amount]]
    (let [path-straight [(get (get xs from) article) (get (get xs to) article)]
          path-positive (ring (get (get xs from) article) (get (get xs to) article) true)
          path-negative (ring (get (get xs from) article) (get (get xs to) article) false)]
      (list article from to amount [path-straight path-positive path-negative])))
  article-flow)) 

(defn- article-flow-fitness [article-flow]
  "an article flow is a vec [[{}{}]..[{}{}]], of vectors each containing a line"
  (let [line-set (reduce
                   (fn [acc [[a b]]]
                     (conj acc #{a b}))
                   #{} article-flow)
        line-combination-set (dyn-for [line-set line-set])
        intersection-count (reduce
                             (fn [acc i]
                              (if (apply intersect? i)
                                (inc acc)
                                acc))
                             0 line-combination-set)]
    intersection-count))

(defn- reference-point [line width]
  "calcs a reference point on the line :line with euclidic distance (with respect to :line) of (:curve-radius invariants)"
  (let [m (ascent line)
        n (if (nil? m) nil (line-n line m))]
    (cond
      ;;up/down
      (nil? m)
      ;;check whether line is going down or up
      (if (< (-> line first :y) (-> line last :y))
        ;;down
        {:x (-> line first :x) :y (- (-> line last :y) width (:article-radius goods-flow))}
        {:x (-> line first :x) :y (+ (-> line last :y) width (:article-radius goods-flow))})
      
      ;;left/right
      (= m 0)
      {:y (-> line last :y)
       :x (if (< (-> line first :x) (-> line last :x))
            (- (-> line last :x) width (:article-radius goods-flow))
            (+ (-> line last :x) width (:article-radius goods-flow)))}

      :else
      ;;calc reference point based on y using the negative m
      ;;y=mx+n
      ;;dx=ctan(-m)*width
      ;;x2=x+dx
      ;;y2=mx2+n
      (let [m (- m)
            x (-> line last :x)
            dx (* (Math/tanh m) width)
            x2 (+ x dx)
            y2 (+ (* m x2) n)]
        {:x x2 :y y2}))))

(defn- r2d [m]
  (reduce
    (fn [acc [k v]]
      (assoc acc k (if (rational? v) (double v) v)))
    {} m))

(defn- render-arrow [color-by-article x concrete-path]
  "renders an arrow designating to the target of a certain article flow. the shape will be triangle with equal spaced cathets,
  and min ancle size of 3/2 * scaled througthput. returns a map containing :reference as well as :arrow. :reference replaces the last point of the path"
  (let [last-line (last concrete-path)
        last-line [{:x (-> concrete-path drop-last last :x)
                    :y (-> concrete-path drop-last last :y)}
                   {:x (-> concrete-path last :x)
                    :y (-> concrete-path last :y)}]
        width (* (scale-throughtput (nth x 3)) 2)
        m (ascent last-line)
        n (if (nil? m) nil (line-n last-line m))
        ! (println :last-line last-line :width width :m m :n n)
        ;;calc reference point on line
        reference (reference-point last-line width) 
        ! (println :reference reference)
        ;;calc intermediate points (corners of the ancle)
        inter (cond
                
                ;;up/down 
                (nil? m)
                [{:x (+ (-> reference :x) (/ width 2))
                  :y (+ (-> reference :y))}
                 {:x (- (-> reference :x) (/ width 2))
                  :y (+ (-> reference :y))}]
                (= m 0)
                
                ;;right/left
                [{:y (+ (-> reference :y) (/ width 2))
                  :x (+ (-> reference :x))}
                 {:y (- (-> reference :y) (/ width 2))
                  :x (+ (-> reference :x))}]
                
                :else
                (let [m-transp (ascent-transpo (last last-line)) 
                      x (-> reference :x)
                      dx (* (Math/tanh m) width)
                      x2 (+ x dx)
                      y2 (+ (* m x2) n)
                      x3 (- x dx)
                      y3 (+ (* m x3) n)
                      ]
                  [{:x x2 :y y2}
                   {:x x3 :y y3}]))
        path [\M (-> inter first r2d :x) (-> inter first r2d :y) \L
              (-> inter last r2d :x) (-> inter last r2d :y)
              (-> last-line last r2d :x) (-> last-line last r2d :y) \z]
        path (interpose " " path)]
    {:reference reference
      :arrow [:path {:class "flow-arrow" :fill (get color-by-article (-> x first)) :d (apply str path)}]}))
  

(defn- render-article-flow-item [color-by-article x]
  "renders an article flow using xs ({area -> {article -> {:x :y}}})"
  (let [arrow (render-arrow color-by-article x (-> x last last))
        pos1 (-> x last last first)
        path (reduce
               (fn [acc i]
                 (let [recent (:recent acc)
                       recent-fallback (if recent recent (-> x last last first))
                       line [{:x (:x recent-fallback) :y (:y recent-fallback)} i]
                       line-reversed [i {:x (:x recent-fallback) :y (:y recent-fallback)}]
                       reference1 (r2d (reference-point line-reversed (:curve-radius invariants)))
                       reference2 (r2d (reference-point line (:curve-radius invariants)))]
                   (assoc acc 
                          :recent i
                          :path (concat 
                                  (:path acc)
                                  (if 
                                    (nil? recent)
                                    [(:x reference2) (:y reference2) \C (:x reference2) (:y reference2) (-> i r2d :x) (-> i r2d :y)]
                                    [(:x reference1) (:y reference1) \L (:x reference2) (:y reference2) \C (:x reference2) (:y reference2) (-> i r2d :x) (-> i r2d :y)])))))
               {:path []}
               (vec (-> x last last rest drop-last)))
        ;;noch die kurve fertigzeichnen
        last-reference (let [line-reversed [(-> x last last last) (-> x last last drop-last last)]]
                         (r2d (reference-point line-reversed (:curve-radius invariants))))

                       
        path (concat 
               [\M (:x (r2d pos1)) (:y (r2d pos1)) \L]
               (:path path) 
               [(:x last-reference) (:y last-reference)]
               [\L (-> arrow :reference r2d :x) (-> arrow :reference r2d :y)])
        path (interpose " " path)]
    {:arrow (:arrow arrow)
     :path [:path {:class "flow-path" :stroke (get color-by-article (-> x first)) :d (apply str path)
            ;;3 means amount (article von nach amount [pathes])
            :stroke-width (scale-throughtput (nth x 3)) }]}))

(defn- r2d [val]
  "modifies a hashmap to contain doubles instead of rationals"
  (reduce
    (fn [acc [k v]]
      (assoc acc k (if (rational? v) (double v) v)))
    {} val))

(def css
  "text {font: 12px sans-serif;}
  
  .title-text {
    font-size: 24px;
    fill: #777777;
    font-weight: bold;
  }
  
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

  .flow-path {
    fill: none; 
    stroke-opacity: 0.7;
  }
  
  .flow-arrow {
    fill-opacity: 0.7;
  }

  body {
    background: white;
  }

  ") 

(let [;;scaling to visible pane
      width 1000
      height 600
      min-x 0
      min-y 0
      zone-top-inline 35
      zone-left-right-inline 1
      top-inset 50
      left-inset 50
      max-x (apply + min-x (map :dx (-> goods-flow :topology)))
      max-y (-> goods-flow :dimension :dy)
      scale-x (scale/linear :domain [min-x max-x]
                      :range [left-inset width])
      scale-dx (scale/linear :domain [min-x max-x]
                      :range [0 (- width left-inset)])
      scale-y (scale/linear :domain [min-y max-y]
                      :range [top-inset height])
      scale-dy (scale/linear :domain [min-y max-y]
                      :range [0 (- height top-inset)])
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
                                              :y (-> (:start-y acc) scale-y )
                                              :width (- (:width zone) (* 2 zone-left-right-inline))
                                              :height (-> (- start-y (:start-y acc)) scale-dy)
                                              :name (:name i)
                                              :separated (:separated i)}))))
                       {:start-y zone-top-inline :areas []} (:areas zone))))
      ;;render zones
      zones (reduce
              (fn [acc i]
                (assoc acc :start-x (+ (:start-x acc) (:dx i))
                           :zones (conj (:zones acc) 
                                    {:x (-> (:start-x acc) scale-x )
                                     :y (-> min-y scale-y )
                                     :width (-> (:dx i) scale-dx )
                                     :height (-> max-y scale-dy)
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
                                        (map second (get (:goods-flow goods-flow) %2)))))
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
                             {:x (+ (:x i) (/ (:width i) 2))
                              :y (+ (:y i) (/ (:height i) 2))}))
                    {} areas)
      area-center (optimize-area-center goods-flow areas area-center)
      ;;ready to calc article center per area
      article-position-by-area (let [article-count (-> goods-flow :goods-flow count)
                                     ;;min distance from lower bound to center for all areas
                                     area-by-name (zipmap (map :name areas) areas)
                                     min-center-distance (first
                                                           (sort
                                                             (map (fn [[k v]]
                                                                    (- (:y v) (:y (get area-by-name k))
                                                                       (/ (-> goods-flow :article-radius) 2))) 
                                                                  area-center)))
                                     article-distance (cond (= 1 article-count)
                                                            0
                                                            :else
                                                            (/ min-center-distance (dec article-count)))
                                     article-distance (min article-distance (* 2 (-> goods-flow :article-radius)))]
                                
                                  (reduce
                                   (fn [acc [area center]]
                                     (assoc acc area
                                            (reduce (fn [acc article]
                                                   (assoc acc article 
                                                          {:x (+ (:x center) (* article-distance (get article-position-area-invariant article)))
                                                           :y (+ (:y center) (* article-distance (get article-position-area-invariant article)))
                                                           })) 
                                                 {} (get article-set-by-area area))))
                                   {} area-center))

      article-positions (apply concat (map (fn [val] (map identity val)) (vals article-position-by-area)))

      ;;assign colors to articles
      color-by-article (zipmap (keys (:goods-flow goods-flow)) article-colors)
      
      ;;consolidate article flow
      article-flow (reduce concat (map (fn [[k v]]
                                         (map #(cons k %) v)) (:goods-flow goods-flow)))
      
      article-flow-model (model-article-flow article-position-by-area article-flow)
      ;;! (doseq [r article-flow-model]
      ;;    (println :article-flow r))

      article-flow-lc (vec (let [points-sequences (map last article-flow-model)]
                             (vec (map 
                               (fn [points-seq]
                                 (vec (map
                                   (fn [points]
                                     (vec (map
                                       (fn [[point i]]
                                         [(nth points i) point])
                                       (zipmap (rest points) (range)))))
                                   points-seq)))
                               points-sequences))))
      ;;! (println :article-flow-lc (count article-flow-lc))
      ;;! (println :count (count (dyn-for article-flow-lc)))
      ! (comment (doseq [r (take 1 (dyn-for article-flow-lc))]
          (println :c r)
          (println :line-combination-fitnet (article-flow-fitness r))))
      ]
  [:body

    [:div {:class "logo-img1"} [:img {:src "/rocklog_logo_home.jpg"}]]
    [:svg#main {:style (str
                        "background: #ffffff; " 
                        "display: block; "
                        "margin: auto;"
                        "height:" height ";"
                        "width:" width ";")}
     [:style {:type "text/css"} css]
    
     ;;render title
     (unify
       [(:title goods-flow)]
       (fn [val]
        [:text 
          {:class "title-text" :x (scale-x (/ width 2)) :y (scale-dy top-inset ) :text-anchor "middle"}
          val]))

     ;;render zones
     (unify 
       (map #(dissoc % :name) zones) 
       (fn [val]
         (let [val (r2d val)]
          [:rect (merge val {:class "zone" :stroke "#a0a0a0" :stroke-width 1 :rx 8 :ry 8})])))
      
     ;;zone names
     (unify
       zones
       (fn [val]
         (let [val (r2d val)]
          [:text {:class "zone-text" :x (+ 10 (:x val)) :y (+ 12 (:y val))} (:name val)])))
     
     ;;render areas
     (unify
       (reduce concat (map :areas zones))
       (fn [val]
         (let [val (r2d val)]
          [:rect (merge val {:class "area"})])))

    ;;render area separation rules
    (apply concat
           (map
             #(unify
               (drop-last %)
                (fn [val]
                  (let [val (r2d val)]
                    (if (:separated val)
                      [:line {:class "area-separator" 
                              :x1 (:x val) :x2 (+ (:x val) (:width val)) 
                              :y1 (+ (:y val) (:height val)) :y2 (+ (:y val) (:height val))}]
                      ;;else
                      nil))))
             (map :areas zones)))
     
     (unify
       (reduce concat (map :areas zones))
       (fn [val]
         [:text {:class "area-text" :x (+ 10 (:x val)) :y (+ 12 (:y val))}
          (:name val)]))
   
     (let [paths (map (partial render-article-flow-item color-by-article)
                      article-flow-model)]
       (unify 
         (reduce  
          (fn [acc i]
            (conj acc (:path i) (:arrow i)))
          [] paths)
         identity))

     (comment (unify
      article-flow-model
       (fn [val]
         (:path (render-article-flow-item color-by-article val)))))
     
     (comment (unify
      article-flow-model
       (fn [val]
         (render-arrow color-by-article val (-> val last last)))))

     ;;render article bubbles
     (unify
       article-positions
       (fn [val]
         [:circle {:cx (-> val second :x double) :cy (-> val second :y double) :r (-> goods-flow :article-radius) :fill (get color-by-article (first val))}]))
          
  ]

    ;;[:div {:class "logo-lambdaroyal"} [:img {:src "/lambdaroyal.png"}]]
   ])
