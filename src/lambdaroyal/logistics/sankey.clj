;; Author: christian.meichsner@informatik.tu-chemnitz.de

(ns lambdaroyal.logistics.sankey 
  (:use [c2.core :only [unify]])
  (:require [c2.scale :as scale])
  (:require [clojure.set :as set]))


(def ^{:doc "used for rendering the bubbles representing the various articles in the areas"} article-colors
  ["#c04c01" "#c08301" "#c0c001" "#72c001"])

(def ^:const invariants
  {:curve-radius 20
   :bottom-inset 70})

;; definition of goods flow
(def goods-flow
  {:title "Warenflussanalyse Dummet AG (Sep. 2013)"
   :article-radius 10
   :dimension
   {:dy 1000}
   :topology
   ;;array of zones
   [{:name "Pool" :dx 300
     :areas [{:name "Inbound" :dy 500 :separated true}
             {:name "Outbound" :separeted true}
             ]}
    {:name "Lift" :dx 200
     :areas [{:name "Lift 1" :dy 500}
             {:name "Lift 2"}]}
    {:name "Lager OG" :dx 300
     :areas [
             {:name "Palettenlager" :dy 400}
             {:name "Palettenwenden" :dy 100 :separated true}
             {:name "Blocklager" :dy 300 :separated true}
             {:name "Beutellager" :dy 300 :separated true}
             ]}
    {:name "Produktion" :dx 300
     :areas [
             {:name "Produktion"}
             ]}
    
    ]
   :article-groups
   #{"Rohwaren Dosen" "Fertigwaren"}
   
   :goods-flow
   {"Rohwaren Dosen"
    [
     ["Inbound" "Lift 1" 2000]
     ["Lift 1" "Palettenwenden" 1000]
     ["Lift 1" "Palettenlager" 1000] 
     ["Palettenwenden" "Palettenlager" 1000]
     ["Palettenlager" "Palettenwenden" 1000]
     ["Palettenwenden" "Palettenlager" 1000]
     
     ["Palettenlager" "Produktion" 2000]
    ]
    
    "Fertigwaren"
    [
     ["Produktion" "Blocklager" 200]
     ["Blocklager" "Lift 2" 200]
     ["Produktion" "Lift 2" 1800]
     ["Lift 2" "Outbound" 2000]
    ]
    
    }
   
   :goods-flow-constraints
   {:source "Inbound"
    :sink "Outbound"}})




















(comment (def goods-flow
  {:title "";;"Warenflussanalyse Mars Roboter Fabs Inc. (Sep. 2013)"
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
    [ ["lift 1&2" "tiefkühl" 20] 
      ["kalt" "lift 1&2" 10]]
    
    }}))


;;genetic algorithm
;;consists of
;;1. domain abstraction: map of keys to finite value ranges. each value range represents the possible values of a domain.
;;2. fitness function: function providing a discrete numerical fitness based on the instance of a domain abstraction, higher fitness value means more fit
;;4. intern: population: a function returning n randomly generated instances of the domain abstraction
;;5. intern: selection: returning n instances of domain abstraction from a list of n instances
;;6. intern: mutation
;;7. intern: crossover


(defprotocol PGa
  (population [this])
  (sort-population [this current-population])
  (selection [this current-population])
  (mutation [this selected-population])
  (crossover [this mutated-population]))

(deftype TGa [^:unsynchronized-mutable state ^clojure.lang.PersistentArrayMap domain size fn-fitness max-round p-mutation p-crossover]
  clojure.lang.IDeref
  (deref [sc]
    (locking sc
      (or state
          (set! state
                (loop [round 0 current-pop (population sc)]
                  (let [sorted-pop (sort-population sc current-pop)
                        ! (comment (println :sorted-pop sorted-pop))
                        selected-pop (selection sc sorted-pop)
                        ! (comment (println :selected-pop selected-pop))
                        mutated-pop (mutation sc selected-pop)
                        ! (comment (println :mutated-pop mutated-pop))
                        crossover-pop (crossover sc mutated-pop)]
                    (cond
                      ;;get out
                      (= round max-round)
                        (first
                          (sort-by last
                                   (sort-population sc crossover-pop)))
                      :else
                      (recur (inc round) crossover-pop))))))))
  PGa
  (population [this]
    (let [instance (fn []
                     (vec (map #(-> % shuffle first)
                            domain)))]
      (repeatedly size instance)))
  (sort-population [this current-population]
    (zipmap current-population (pmap fn-fitness current-population)))
  (selection [this sorted-population]
    (let [max-fitness (inc (apply max (vals sorted-population)))
          normalized-population (persistent! 
                                  (reduce (fn [acc [k v]]
                                            (assoc! acc k (- max-fitness v)))
                                          (transient {}) sorted-population))
          overall-fitness (apply + (vals normalized-population))
          normalized-population (persistent!
                                  (reduce (fn [acc [k v]]
                                            (assoc! acc k (/ v overall-fitness)))
                                          (transient {}) normalized-population))
          normalized-population (vec (map identity normalized-population))]
      (repeatedly size #(let [p (Math/random)]
                     (loop [sum 0 i 0]
                       (if
                         (> (+ sum (last (nth normalized-population i))) p)
                         (first (nth normalized-population i))
                         ;;else
                         (recur (+ sum (last (nth normalized-population i))) (inc i))))))))
  (mutation [this selected-population]
    (map 
      (fn [chromosom]
        (reduce
          (fn [acc i]
            (conj
              acc
              (let [p (Math/random)]
                (if
                  (> p p-mutation)
                  (nth chromosom i)
                  ;;else
                  (-> (nth domain i) shuffle first)))))
              [] (range (count chromosom)))) 
      selected-population))
  (crossover [this mutated-population]
    (let [;;shuffle them for random selection
          population (shuffle mutated-population)
          ;;building partitions containing two elements
          partitioned (partition-all 2 population)]
        (reduce
          (fn [acc i]
            (cond (= 1 (count i))
                  (conj acc i)
                  :else
                  (let [i1 (first i)
                        i2 (last i)]
                    (if 
                      (or 
                        (> (Math/random) p-crossover)
                        (= 1 (count i1)))
                      (conj acc i1 i2)
                      ;;else
                      (let [crossover-point (inc (.nextInt (java.util.Random.) (-> i1 count dec)))]
                        (conj acc
                              (concat (take crossover-point i1) (drop crossover-point i2))
                              (concat (take crossover-point i2) (drop crossover-point i1))))))))
          [] partitioned))))

;;(time @(TGa. nil domain 30 (fn [x] (+ (first x) (last x))) 30 0.09 0.15
;;n))












(defn- eq? [a b]
  (cond 
    (= (class a) (class b))
    (= a b)
    (and (number? a) (number? b))
    (= (double a) (double b))
    :else
    (= a b)))

;;scales the throughtput of all dataflows into [1.. :article-radius]
(def scale-throughtput 
  (let [t (map last (reduce concat (map (fn [[k v]]
                                         (map #(cons k %) v)) (:goods-flow goods-flow))))]
    (scale/linear 
               :domain [0 (apply max t)]
               :range [(min 10 (/ (:article-radius goods-flow) 2)) (* 1.5 (:article-radius goods-flow))])))

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

(defn- bound-point [p]
  "checks whether a point is outside goods-flow topology"
  (let [dy (-> goods-flow :dimension :dy)
        dx (apply + (map :dx (:topology goods-flow)))
        y (cond (< (:y p) 0) 0 
                (> (:y p) dy) dy
                :else (:y p)) 
        x (cond (< (:x p) 0) 0 
                (> (:x p) dx) dx
                :else (:x p))] 
    {:x x :y y})) 

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
       (let [diverge (/ (- (:x from) (:x to)) 4)
             diverge (if positiv diverge (- diverge))
             inter (bound-point {:x (:x from) :y (+ diverge (:y from))})]
         (vec (concat [from] (ring inter to positiv))))
       :else
       (let [diverge (/ (- (:y from) (:y to)) 4)
             diverge (if positiv diverge (- diverge))
             inter (bound-point {:y (:y from) :x (- (:x from) diverge)})]
         (vec (concat [from] (ring inter to positiv))))))

(defn- ascent [line]
  "calcs the ascent of a line"
  (let [dx (- (-> line second :x) (-> line first :x))
        dy (- (-> line second :y) (-> line first :y))]
    (if (eq? 0 dx)
      nil
      (/ dy dx))))

(defmulti line-n (fn [r2 m]
  (class r2)))

(defmethod line-n clojure.lang.PersistentVector [line m]
  "calc vertical bias of a line"
  (- (-> line first :y) (* m (-> line first :x))))

(defmethod line-n clojure.lang.PersistentArrayMap [line m]
  "calc vertical bias of a line"
  (- (-> line :y) (* m (-> line :x))))

(defn- line-length [line]
  "calcs the euclidic length of a line"
  (let [dy (- (-> line first :x) (-> line second :x))
        dx (- (-> line first :y) (-> line second :y))]
    (Math/sqrt
      (+
        (* dx dx)
        (* dy dy)))))

(defn- ascent-transpo [line]
  "calcs the transponed ascent of a line"
  (let [dy (- (-> line first :x) (-> line second :x))
        dx (- (-> line first :y) (-> line second :y))]
    (if (eq? 0 dx)
      nil
      (- (/ dy dx)))))

(defn- line-n-transpo [reference m]
  (- (-> reference :y) (* m (-> reference :x))))

(defn- in-range? [line dim value]
  "returns true if value is in range of dim :x/:y of line"
  (and
    (>= value (min (-> line first dim) (-> line second dim)))
    (<= value (max (-> line first dim) (-> line second dim)))))

(defn- online? [point line]
  (let [m (ascent line)
        n (if 
            (nil? m) 
            nil
            (line-n line m))]
    (cond 
      (nil? m)
      (and
        (= (:x point) (-> line first :x))
        (in-range? line :y (:y point)))
      :else
      (let [y2 (+ n (* m (:x point)))]
        (and
          (= y2 (:y point))
          (in-range? line :y y2))))))

(defn- intersect? [line1 line2]
  "checks whether line1 and line2 have an intersection."
  (if
    (= 5 (count (set [(first line1)
                  (last line1)
                  (first line2)
                  (last line2)])))
    (let [m1 (ascent line1)
          m2 (ascent line2)]
      (cond (= m1 m2)
            ;;lines are parallel
            (or
              (online? (first line1) line2)
              (online? (last line1) line2)
              (online? (first line2) line1)
              (online? (last line2) line1))
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
                (eq? 0 (- m1 m2))
                false
                ;; std case
                :else
                (let [n1 (line-n l1 m1)
                      n2 (line-n l2 m2)
                      x (/ (- n2 n1)
                          (- m1 m2))]
                  (and
                    (in-range? l1 :x x)
                    (in-range? l2 :x x)))))))))

(defn normalize-path [path]
  (vec
    (map 
      (fn [a b] [a b])
      (drop-last path) (rest path))))

(defn normalize-pathes [path]
  (vec
    (reduce
      (fn [acc i]
        (concat acc
                (normalize-path i)))
      [] path)))

;; matrix multiplication - thanks to http://rosettacode.org/wiki/Matrix_multiplication#Clojure
(defn transpose
  [s]
  (apply map vector s))
 
(defn nested-for
  [f x y]
  (map (fn [a]
    (map (fn [b] 
            (f a b)) y))
              x))
 
(defn matrix-mult
  [a b]
  (nested-for (fn [x y] (reduce + (map * x y))) a (transpose b)))

(defn rotation-matrix [arc]
  [[(Math/cos arc) (- (Math/sin arc)) 0]
   [(Math/sin arc) (Math/cos arc) 0]
   [0 0 1]])

(defn translation-matrix [dx dy]
  [[1 0 dx]
   [0 1 dy]
   [0 0 1]])

(defn rotation-translation [point arc dx dy]
  (let [m1 (matrix-mult
             (translation-matrix dx dy) 
             (matrix-mult (rotation-matrix arc)
                          (translation-matrix (- dx) (- dy))))
        m2 [[(:x point)]
            [(:y point)]
            [1]]
        r (matrix-mult m1 m2)]
    r))

(defn- reference-point [line width]
  "calcs a reference point on the line :line with euclidic distance (with respect to :line) of (:curve-radius invariants)"
  (let [m (ascent line)
        n (if (nil? m) nil (line-n line m))
        width (min width (line-length line))]
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
      (let [m m ;;(- m)
            x (-> line last :x)
            dx (Math/abs (* (Math/sin (/ (* 180 (Math/tanh m)) Math/PI)) width))
            rotated (rotation-translation {:y (-> line last :y)
                                                        :x (if (< (-> line first :x) (-> line last :x))
                                                        (- (-> line last :x) width (:article-radius goods-flow))
                                                        (+ (-> line last :x) width (:article-radius goods-flow)))}
                                                        (Math/tanh m)
                                                       (-> line last :x)
                                                       (-> line last :y))
            x2 (-> rotated first first)
            y2 (-> rotated second first)]
        {:x x2 :y y2}))))

(defn- r2d [val]
  "modifies a hashmap to contain doubles instead of rationals"
  (reduce
    (fn [acc [k v]]
      (assoc acc k (if (rational? v) (double v) v)))
    {} val))

(defn- ring2 [from to positiv]
 "this function calculates a rectangular path from a point A to a point B on the connection line :from to :to and returns this path as vector of points"
  (let [connection-length (line-length [from to])
        reference1 (reference-point [from to] (/ connection-length 4))
        reference2 (reference-point [to from] (/ connection-length 4))]
    (concat [from]
            (ring reference2 reference1 positiv)
            [to])))

(defn- article-flow-fitness [article-flow]
  "an article flow is a vec [[{}{}]..[{}{}]], of vectors each containing a line"
  (let [article-flow (normalize-pathes article-flow)
        line-set (set article-flow)
        line-combination-set (dyn-for [line-set line-set])
        intersection-count (reduce
                             (fn [acc i]
                              (if (apply intersect? i)
                                (inc acc)
                                acc))
                             0 line-combination-set)]
    (+ (* 6 intersection-count) (count line-set))))

(defn- model-article-flow [xs article-flow]
  "models an article flow using xs ({area -> {article -> {:x :y}}}) and 
  article-flow which is '('(article from to amount)*).
  result is '('(article from to amount [[{:x :y}{:x :y}][{:x :y}{:x :y}{:x :y}{:x :y}]]))"
  (let [stage1 (map
                 (fn [[article from to amount]]
                   (let [path-straight [(get (get xs from) article) (get (get xs to) article)]
                         path-positive (ring (get (get xs from) article) (get (get xs to) article) true)
                         path-negative (ring (get (get xs from) article) (get (get xs to) article) false)
                         path2-positive (ring2 (get (get xs from) article) (get (get xs to) article) true)
                         path2-negative (ring2 (get (get xs from) article) (get (get xs to) article) false)]
                     (list article from to amount [path-straight path-positive path-negative path2-positive path2-negative])
                     ))
                  article-flow)
        domain (map last stage1)
        optimized-article-flow (time @(TGa. nil domain 4 article-flow-fitness 4 0.09 0.15))
        ! (println optimized-article-flow)
        ;;now we map back the optimized pathes into the article flow
        optimized-article-flow (map
                                 (fn [[article from to amount] v]
                                   (list article from to amount v))
                                 (map drop-last stage1)
                                 (map (fn [i] [(vec i)]) (first optimized-article-flow)))
        ]
    optimized-article-flow))

;;(time @(TGa. nil domain 30 (fn [x] (+ (first x) (last x))) 30 0.07 0.1))


(defn- render-arrow [color-by-article x concrete-path & args]
  "renders an arrow designating to the target of a certain article flow. the shape will be triangle with equal spaced cathets,
  and min ancle size of 3/2 * scaled througthput. returns a map containing :reference as well as :arrow. :reference replaces the last point of the path"
  (let [args (if args 
                 (apply hash-map args) 
                 {})
        last-line [{:x (-> concrete-path drop-last last :x)
                    :y (-> concrete-path drop-last last :y)}
                   {:x (-> concrete-path last :x)
                    :y (-> concrete-path last :y)}]
        width (cond
                (:arrow-width args) (:arrow-width args) 
                (:stroke-width args) (:stroke-width args) 
                :else (* (scale-throughtput (nth x 3)) 2))
        m (ascent last-line)
        n (if (nil? m) nil (line-n last-line m))
        ;;calc reference point on line
        ;;the reference R point is taken into account as per:
        ;;                             |---
        ;;                             |--- ---
        ;;  Start----------------------R---------->Stop
        ;;                             |-------
        ;;                             |---
        ;;
        ;;
        reference (reference-point last-line width)
        ;;calc backward reference that is the reference point of the last curve
        backward-reference (reference-point (vec (reverse last-line)) (:curve-radius invariants))
        dreference (if
                    (> 
                      (line-length [backward-reference (last concrete-path)])  
                      (line-length [reference (last concrete-path)]))
                    reference 
                    backward-reference)

        ;;calc intermediate points (corners of the ancle)
        inter [{:y (+ (-> reference :y) (/ width 2))
                :x (+ (-> reference :x))}
               {:y (- (-> reference :y) (/ width 2))
                :x (+ (-> reference :x))}
               {:y (:y reference)
                :x (+ (:x reference) width)}]
        rotation-ancle (cond
                          ;;up/down
                          (nil? m)
                          ;;check whether line is going down or up
                          (if (< (-> last-line first :y) (-> last-line last :y))
                            ;;down
                            90 
                            ;;up
                            -90)
                          
                          ;;left/right
                          (= m 0)
                          (if (< (-> last-line first :x) (-> last-line last :x))
                            ;;right
                            0
                            ;;left
                            180)

                          :else
                          (+ (if (< (-> last-line first :x) (-> last-line last :x)) 0 180) (/ (* 180 (Math/tanh m)) Math/PI)))

        path [\M (-> inter first r2d :x) (-> inter first r2d :y) \L
              (-> inter second r2d :x) (-> inter second r2d :y)
              (-> inter last r2d :x) (-> inter last r2d :y) \z]
        path (interpose " " path)]
    {:reference reference
      :arrow [:path {:class "flow-arrow" :fill (get color-by-article (-> x first)) :d (apply str path)
                     :transform (format "rotate(%f,%f,%f)" (double rotation-ancle) (-> reference :x double) (-> reference :y double))}]}))
  
(defn- monotonic-index? [line1 line2]
  "returns false if x or y vector of line1 are directed opposite to those of line2"
  (let [line1-dx (apply - (map :x line1))
        line2-dx (apply - (map :x line2))
        line1-dy (apply - (map :y line1))
        line2-dy (apply - (map :y line2))]
    (and
      (>= (* line1-dx line2-dx) 0)
      (>= (* line1-dy line2-dy) 0))))

(defn- render-article-flow-item [color-by-article x & args]
  "renders an article flow using xs ({area -> {article -> {:x :y}}})"
  (let [select first
        arrow (apply render-arrow color-by-article x (-> x last select) args)
        args (if args (apply hash-map args) 
                 {})
        pos1 (-> x last select first)
        posn (-> x last select last)
        path (if
               (< (-> x last select count) 3)
               ;;case 1: straigth line
               (concat 
                 [\M (:x (r2d pos1)) (:y (r2d pos1))]
                 [\L (-> arrow :reference r2d :x) (-> arrow :reference r2d :y)])
               ;;else
               (let [index-path (zipmap (range) (-> x last select))
                     path (reduce
                            (fn [acc index]
                              (let [preB (if (< index (-> index-path count dec))
                                           (r2d (reference-point [(get index-path index) (get index-path (inc index))] (:curve-radius invariants)))
                                           nil)
                                    preA (if (> index 0)
                                           (r2d (reference-point [(index-path (dec index)) (index-path index)] (:curve-radius invariants)))
                                           nil)
                                    postA (if (< index (-> index-path count dec))
                                           (r2d (reference-point [(index-path (inc index)) (index-path index)] (:curve-radius invariants)))
                                           nil)
                                    non-monotonic 
                                      (or
                                        (:non-monotonic acc)
                                        (and
                                          (-> preB nil? not) (-> postA nil? not)
                                          (not (monotonic-index? [postA preB] [(get index-path index) (get index-path (inc index))]))))
                                    ]
                                (assoc 
                                  acc 
                                  :non-monotonic non-monotonic
                                  :path     
                                (concat 
                                  (:path acc)
                                  (cond
                                    (= 0 index)
                                    ;;erste position -> eine linie zum preB also zum vorgängerreferenzknoten des nächsten knotens
                                    [\L (-> (index-path index) r2d :x) (-> (index-path index) r2d :y) (:x preB) (:y preB) \newline]
                                    
                                    (< index (-> index-path count dec dec))
                                    ;;die positionen von ]0 ... ende[ -> wir brauchen eine krümmung und anschliessend eine linie 
                                    ;;zum vorgängerreferenzknoten des nächsten knotens
                                    [\C (:x preA) (:y preA) (-> (index-path index) r2d :x) (-> (index-path index) r2d :y) (:x postA) (:y postA)
                                    \L  (:x preB) (:y preB)]
                                    
                                    (= index (-> index-path count dec dec))
                                    ;;vorletzte position -> zeichnen bis zur letzten position 
                                    [\C (:x preA) (:y preA) (-> (index-path index) r2d :x) (-> (index-path index) r2d :y) (:x postA) (:y postA)
                                    \L (-> arrow :reference r2d :x) (-> arrow :reference r2d :y)]
                                    
                                    :else
                                    [])))))
                            {:path [] :non-monotonic false} (range (count index-path)))

                     ! (comment (println :non-monotonic (:non-monotonic path)))

                     ;;do it again with straight lines if we have a non-monotonic shape
                     path (if 
                            (-> path :non-monotonic not) path
                            (reduce
                              (fn [acc index]
                                (assoc 
                                  acc 
                                  :path     
                                  (concat 
                                    (:path acc)
                                    (cond
                                      (< index (-> index-path count dec dec))
                                      ;;die positionen von ]0 ... ende[ -> wir brauchen eine krümmung und anschliessend eine linie 
                                      ;;zum vorgängerreferenzknoten des nächsten knotens
                                      [(-> (index-path index) r2d :x) (-> (index-path index) r2d :y)
                                       (-> (index-path (inc index)) r2d :x) (-> (index-path (inc index)) r2d :y)]
                                      
                                      (= index (-> index-path count dec dec))
                                      ;;vorletzte position -> zeichnen bis zur letzten position 
                                      [(-> (index-path index) r2d :x) (-> (index-path index) r2d :y)
                                       (-> arrow :reference r2d :x) (-> arrow :reference r2d :y)]
                                      
                                      :else
                                      []))))
                                {:path [\L] :non-monotonic false} (range (count index-path))))]
                (concat 
                 [\M (:x (r2d pos1)) (:y (r2d pos1))]
                 (:path path))))
        path (interpose " " path)]
    {:text (if
             (:stroke-text args)
             [:text {:class "stroke-text" :text-anchron "middle" 
                       :x (double (/ (apply + (map :x [pos1 posn])) 2))
                       :y (+ (double (/ (apply + (map :y [pos1 posn])) 2)) 6)} (:stroke-text args)]
             nil)
     :arrow (:arrow arrow)
     :path [:path {:class "flow-path" :stroke (get color-by-article (-> x first)) :d (apply str path)
            ;;3 means amount (article von nach amount [pathes])
            :stroke-width (if (:stroke-width args) (:stroke-width args) (scale-throughtput (nth x 3)))}]}))


(def css
  "text {font: 12px sans-serif;}
  
  .title-text {
    font-size: 24px;
    fill: #777777;
    font-weight: bold;
  }
  
  .stroke-text {
    font-size: 20px;
    fill: black;
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
      width 1200
      height 600
      min-x 0
      min-y 0
      zone-top-inline 35
      zone-left-right-inline 1
      top-inset 50
      left-inset 150
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

      ;;consolidate article flow (article from to amount & path)
      article-flow (reduce concat (map (fn [[k v]]
                                         (map #(cons k %) v)) (:goods-flow goods-flow)))
      
      ;; calc some stats 
      fan-in-fan-out (reduce
                       (fn [acc [_ from to amount & r]]
                         (let [inc-in (if (= from (-> goods-flow :goods-flow-constraints :source)) 
                                        amount 
                                        0)
                               inc-out (if (= to (-> goods-flow :goods-flow-constraints :sink)) 
                                        amount 
                                        0)]
                           (assoc acc
                                  :in (+ (:in acc) inc-in)
                                  :out (+ (:out acc) inc-out))))
                       {:in 0 :out 0} article-flow)
      ! (println :fan fan-in-fan-out)
      
      article-flow-model (model-article-flow article-position-by-area article-flow)

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
      ]
  [:body

    [:div {:class "logo-img1"} [:img {:src "/rocklog_logo_home.jpg"}]]
    [:svg#main {:style (str
                        "background: #ffffff; " 
                        "display: block; "
                        "margin: auto;"
                        "height:" (+ height (:bottom-inset invariants)) ";"
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

     ;;render article bubbles
     (unify
       article-positions
       (fn [val]
         [:circle {:cx (-> val second :x double) :cy (-> val second :y double) :r (-> goods-flow :article-radius) :fill (get color-by-article (first val))}]))

     ;;non-functional stuff
     ;;

     ;;render color bubbles below chart to designate the article groups
     (let [i-by-article (zipmap (keys (:goods-flow goods-flow)) (range))
           start-x (scale-x 10)
           dx (* 3 (:article-radius goods-flow))
           y (double (+ (scale-y (-> goods-flow :dimension :dy)) (:article-radius goods-flow) 20))]
       (unify
         (reduce
           (fn [acc [k v]]
             (conj 
               acc
               ;;bubble
               [:circle {:cx (+ start-x (* dx v)) :cy y :r (-> goods-flow :article-radius) :fill (get color-by-article k)}]
               ;;description
               [:text {:class "zone-text" 
                       :x (+ start-x (* dx v) (:article-radius goods-flow) 5) 
                       :y (+ y 6) 
                       :transform (format "rotate(%d,%f,%f)" 45 (double (+ start-x (* dx v))) y)} k]
               ))
          [] i-by-article)
         identity))

     ;;render fan in & fan out using render-arrow [color-by-article x concrete-path]
     (let [in-center (get area-center (-> goods-flow :goods-flow-constraints :source))
           out-center (get area-center (-> goods-flow :goods-flow-constraints :sink))
           paths (map (fn [[_1 _2]] (render-article-flow-item (assoc color-by-article :stats-in "green" :stats-out "red") _1 :stroke-width 70 :arrow-width 100 :stroke-text _2))
                      [[[:stats-in nil nil (:in fan-in-fan-out) [[{:x 0 :y (:y in-center)}{:x (+ (scale-x -10) (/ (:article-radius goods-flow) 2)) :y (:y in-center)}]]] 
                        (:in fan-in-fan-out)]
                       [[:stats-out nil nil (:out fan-in-fan-out) [[{:x (scale-x -10) :y (:y out-center)}{:x 0 :y (:y out-center)}]]]
                        (:out fan-in-fan-out)]])]
       (unify 
         (reduce  
          (fn [acc i]
            (conj acc (:path i) (:arrow i) (:text i)))
          [] paths)
         identity))

  ]

    ;;[:div {:class "logo-lambdaroyal"} [:img {:src "/lambdaroyal.png"}]]
   ])
