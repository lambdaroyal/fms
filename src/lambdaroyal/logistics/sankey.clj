;; Author: christian.meichsner@informatik.tu-chemnitz.de

(ns lambdaroyal.logistics.sankey 
  (:use [c2.core :only [unify]])
  (:require [c2.scale :as scale])
  (:require [clojure.set :as set])
  (:import [org.lambdaroyal.util ConsoleProgress]))

(def ^{:doc "used for rendering the bubbles representing the various articles in the areas"} article-colors
  ["#c04c01" "#c08301" "#c0c001" "#72c001" "#00cfae" "#0086cf" "#0556f2" "#7605f2" "#ea05f2" "white"])

;; definition of goods flow
(def goods-flow
  {:title "Warenfluss-Visualisierung und Simulation"
   :article-radius 10
   :dy 2000
   :curve-radius 10
   :bottom-inset 150
   :right-inset 150
   :max-fan-size 80
   :max-throughtput-factor 2
   :topology
   ;;array of zones
   [{:name "Pool" :dx 200
     :areas [{:name "Inbound" :dy 500 :separated true}
             {:name "Zwischenboden" :dy 333 :separated true}
             {:name "Unter Zwischenboden" :dy 333 :separated true}
             {:name "Gang" :dy 334 :separated true}

             {:name "Outbound" :separated true :dy 500}
             ]}
    {:name "Lager EG" :dx 200
     :areas [{:name "Palettenlager EG" :dy 500}
             {:name "Waschen" :dy 1000}
             {:name "Abfall"}]}
    {:name "Lift" :dx 200
     :areas [{:name "Lift 1" :dy 500}
             {:name " " :dy 1000}
             {:name "Lift 2"}]}
    {:name "Lager OG" :dx 300
     :areas [
             {:name "Palettenlager OG" :dy 500}
             {:name "Palettenwenden" :dy 500 :separated true}
             {:name "Blocklager OG" :dy 500 :separated true}
             {:name "Beutellager" :separated true}
             ]}
    {:name "Produktion" :dx 300
     :areas [
             {:name "Abfüllerei" :dy 500 :separated true}
             {:name "Abpackerei" :dy 1000 :separated true}
             {:name "Blocklager"}
             ]}
    
    ]
   :article-groups
   #{"Rohwaren Dosen" "Fertigwaren" "Rohwaren Fässer" "Hilfsstoffe" "Verpackung" "Leergut Migros" "Abfall" "Leergut Fässer" "Fertigwaren Migros" "Fertigwaren Rest"} 
  
   :goods-flow
   {"Rohwaren Fässer"
    [["Inbound" "Palettenlager EG" 2000]
     ["Palettenlager EG" "Waschen" 2000]
     ["Waschen" "Lift 1" 2000]
     ["Lift 1" "Abfüllerei" 2000]]
    "Rohwaren Dosen"
    [
     ["Inbound" "Lift 1" 2300]
     ["Lift 1" "Palettenwenden" 1150]
     ["Lift 1" "Palettenlager OG" 1150] 
     ["Palettenwenden" "Palettenlager OG" 2300]
     ["Palettenlager OG" "Palettenwenden" 1150]
     ["Palettenlager OG" "Abfüllerei" 2300]
     ["Abfüllerei" "Abpackerei" 2300]
    ]
    "Hilfsstoffe"
    [["Inbound" "Palettenlager EG" 42]
     ["Palettenlager EG" "Waschen" 30]
     ["Palettenlager EG" "Lift 1" 12]
     ["Lift 1" "Abfüllerei" 12]]
    "Verpackung"
    [["Inbound" "Zwischenboden" 451]
     ["Zwischenboden" "Lift 1" 451]
     ["Lift 1" "Abfüllerei" 451]]

    "Leergut Migros"
    [["Inbound" "Gang" 2366]
     ["Gang" "Lift 1" 2366]
     ["Inbound" "Lift 1" 2000]
     ["Lift 1" "Abfüllerei" 4366]]

    "Abfall"
    [["Lift 2" "Palettenlager EG" 204]
     ["Palettenlager EG" "Outbound" 204]
     ["Abfüllerei" "Lift 2" 204]]

    "Leergut Fässer"
    [["Waschen" "Unter Zwischenboden" 1400]
     ["Unter Zwischenboden" "Outbound" 1400]]
     
    
    "Fertigwaren Migros"
    [
     ["Abfüllerei" "Abpackerei" 3246]
     ["Abpackerei" "Lift 2" 2246]
     ["Blocklager" "Lift 2" 1000]
     ["Lift 2" "Outbound" 2246]
     ["Lift 2" "Gang" 1000]
     ["Abpackerei" "Blocklager" 1000]
    ]

    "Fertigwaren Rest"
    [["Lift 2" "Outbound" 2656]
     ["Abpackerei" "Lift 2" 2656]
     ["Abfüllerei" "Abpackerei" 2656]]


    }
   
   :goods-flow-constraints
   {:source "Inbound"
    :sink "Outbound"
    :drop-delay 20
    :pick-delay 15
    :transfer-delay 120}
})




















(comment (def goods-flow
  {:title "";;"Warenflussanalyse Mars Roboter Fabs Inc. (Sep. 2013)"
   :article-radius 10
   :dy 1000
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
    [  
      ["kalt" "tiefkühl" 10]
      ["warm" "tiefkühl" 6]
      ]
    }
   :goods-flow-constraints
   {:source "inbound"
    :sink "outbound"}}))


;;visualize genetic optimization progress
(def progress (ConsoleProgress. \-))
(def progress-agent (agent 0))
(def ! (add-watch progress-agent :foo 
             (fn [k r o n]
               (.showProgress progress "genetic optimization of pathes" n))))


(def progress-optimize-centers (ConsoleProgress. \-))
(def progress-agent-optimize-centers (agent 0))
(def ! (add-watch progress-agent-optimize-centers :foo 
             (fn [k r o n]
               (.showProgress progress-optimize-centers "genetic optimization of area centers" n))))

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


(deftype TGa [^:unsynchronized-mutable state ^clojure.lang.PersistentArrayMap domain size fn-fitness max-round p-mutation p-crossover progress]
  clojure.lang.IDeref
  (deref [sc]
    (locking sc
      (or state
          (set! state
                (loop [round 0 current-pop (population sc)]
                  (let [sorted-pop (sort-population sc current-pop)
                        selected-pop (selection sc sorted-pop)
                        ! (comment (println :selected-pop selected-pop))
                        mutated-pop (mutation sc selected-pop)
                        ! (comment (println :mutated-pop mutated-pop))
                        crossover-pop (crossover sc mutated-pop)
                        ! (if
                            (and
                              progress
                              (= clojure.lang.Agent (class progress)))
                            (send-off progress #(+ % (int (Math/ceil (/ 100 max-round)))))
                            nil)]
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

;;(time @(TGa. nil domain 30 (fn [x] (+ (first x) (last x))) 30 0.09 0.15 nil
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
               :range [(min 10 (/ (:article-radius goods-flow) 2)) (* (:max-throughtput-factor goods-flow) (:article-radius goods-flow))])))

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
                                            (>= % (+ (:y k) (* 2  (:article-radius goods-flow))))
                                            (<= % (- (+ (:y k) (:height k))
                                                     (* 2 (:article-radius goods-flow))))) v))]) range-per-area)
        areas (map #(-> % first :name) range-per-area)
        domain (vec (map second range-per-area))

        min-range @(TGa. nil domain 10 #(-> % distinct count) 20 0.09 0.15 progress-agent-optimize-centers)

        ;;dynamic list comprehension - to slow
        ;;range-space (dyn-for (vec (map second range-per-area)))
        ;;min-range (first (sort-by (juxt #(-> % distinct count) #(apply + %)) range-space))
        
        y-by-area (zipmap areas (first min-range))
        area-center (reduce 
                      (fn [acc [k v]] 
                        (assoc acc k 
                               (assoc v :y (get y-by-area k)))) 
                      {} area-center)]
    area-center))

(defn- bound-point [p]
  "checks whether a point is outside goods-flow topology"
  (let [dy (:dy goods-flow)
        dx (apply + (map :dx (:topology goods-flow)))
        y (cond (< (:y p) 0) 0 
                (> (:y p) dy) dy
                :else (:y p)) 
        x (cond (< (:x p) 0) 0 
                (> (:x p) dx) dx
                :else (:x p))] 
    {:x x :y y})) 

(defn- ring [from to positiv diverge-orig]
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
       (let [diverge (/ (- (:x from) (:x to)) diverge-orig)
             diverge (if positiv diverge (- diverge))
             inter (bound-point {:x (:x from) :y (+ diverge (:y from))})]
         (vec (concat [from] (ring inter to positiv diverge-orig))))
       :else
       (let [diverge (/ (- (:y from) (:y to)) diverge-orig)
             diverge (if positiv diverge (- diverge))
             inter (bound-point {:y (:y from) :x (- (:x from) diverge)})]
         (vec (concat [from] (ring inter to positiv diverge-orig))))))

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
      (eq? 0 m)
      (and
        (= (:y point) (-> line first :y))
        (in-range? line :x (:x point)))
      :else
      (let [y2 (+ n (* m (:x point)))]
        (and
          (= y2 (:y point))
          (in-range? line :y y2))))))

(defn- r2d [val]
  "modifies a hashmap to contain doubles instead of rationals"
  (reduce
    (fn [acc [k v]]
      (assoc acc k (if (rational? v) (double v) v)))
    {} val))

(defn- round [val]
  "modifies a hashmap to contain rounded numbers instead of rationals"
  (reduce
    (fn [acc [k v]]
      (assoc acc k (Math/round v)))
    {} (r2d val)))

(defn- intersect-precond? [a b]
  "returns true if a necessary precondition for an intersection is given, is not an effectual precondition. serves as performance tuning"
  (let [single-dim (fn [a b dim]
                     (let [min-a (apply min (map dim a))
                           max-a (apply max (map dim a))
                           min-b (apply min (map dim b))
                           max-b (apply max (map dim b))]
                       (and
                         (>= max-a min-b)
                         (>= max-b min-a))))]
    (and
      (single-dim a b :x)
      (single-dim a b :y))))
                            



(defn- intersect? [line1 line2]
  "checks whether line1 and line2 have an intersection."
  (let [line1 (vec (map round line1))
        line2 (vec (map round line2))]
  (if
    ;; check necessary preconditions
    (and
      ;; line are not chaining
      (let [point-set (set [(first line1)
                            (last line1)
                            (first line2)
                            (last line2)])]
        (or
          (not= 3 (count point-set))
          ;;line might be parallel with different length, having a common subset. example (A->B) and (B->C) -> intersect: true
          ;;A-------->B--------->C
          (or
            (= 3 (count (filter #(online? % line1) point-set)))
            (= 3 (count (filter #(online? % line2) point-set))))))


      ;; line1 might cross at all
      (intersect-precond? line1 line2))
      (let [m1 (ascent line1)
            m2 (ascent line2)]
        (cond (= line1 line2)
              false
              (eq? m1 m2)
              ;;lines are parallel
              (or
                (online? (first line1) line2)
                (online? (last line1) line2)
                (online? (first line2) line1)
                (online? (last line2) line1))
              :else
              (let [l1-2 (if (nil? m1) [line2 line1] [line1 line2])
                    l1 (first l1-2)
                    l2 (last l1-2)
                    m1-2 (if (nil? m1) [m2 m1] [m1 m2])
                    m1 (first m1-2)
                    m2 (last m1-2)
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
                      (in-range? l2 :x x)))))))
    ;else
    false)))

(defn normalize-path [path]
  (vec
    (map 
      (fn [a b] (with-meta [a b] (meta path)))
      (drop-last path) (rest path))))

(defn normalize-pathes [path]
  (cond (= 2 (count path))
        [path]
        :else
        (vec
          (reduce
            (fn [acc i]
              (concat acc
                      (normalize-path i)))
            [] path))))

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
  "calcs a reference point on the line :line with euclidic distance (with respect to :line) of (:curve-radius goods-flow)"
  (let [m (ascent line)
        n (if (nil? m) nil (line-n line m))
        width (min width (line-length line))
        invariant-distance (+ (:article-radius goods-flow) (/ (:article-radius goods-flow) 4))]
    (cond
      ;;up/down
      (nil? m)
      ;;check whether line is going down or up
      (if (< (-> line first :y) (-> line last :y))
        ;;down
        {:x (-> line first :x) :y (- (-> line last :y) width invariant-distance)}
        {:x (-> line first :x) :y (+ (-> line last :y) width invariant-distance)})
      
      ;;left/right
      (= m 0)
      {:y (-> line last :y)
       :x (if (< (-> line first :x) (-> line last :x))
            (- (-> line last :x) width invariant-distance)
            (+ (-> line last :x) width invariant-distance))}

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
                                                        (- (-> line last :x) width invariant-distance)
                                                        (+ (-> line last :x) width invariant-distance))}
                                                        (Math/tanh m)
                                                       (-> line last :x)
                                                       (-> line last :y))
            x2 (-> rotated first first)
            y2 (-> rotated second first)]
        {:x x2 :y y2}))))


(defn- ring2 [from to positive diverge]
 "this function calculates a rectangular path from a point A to a point B on the connection line :from to :to and returns this path as vector of points"
  (let [connection-length (line-length [from to])
        reference1 (reference-point [from to] (/ connection-length diverge))
        reference2 (reference-point [to from] (/ connection-length diverge))]
    (concat [from]
            (ring reference2 reference1 positive diverge)
            [to])))

(defn- trapez [from to positive diverge]
  "take a line from A to B, calcs reference Point A' (A->B) and B' (B->A) and rotates the reference point (+/-)45/-45 degress"
  (let [length (line-length [from to])] 
    (if
      (> length (* 5 (:article-radius goods-flow)))
      (let [ref-from (reference-point [to from] (/ length diverge))
          ref-to (reference-point [from to] (/ length diverge))
          ref-from (rotation-translation ref-from (if positive (/ Math/PI 4) (- (/ Math/PI 4))) (:x from) (:y from))
          ref-to (rotation-translation ref-to (if positive (- (/ Math/PI 4)) (/ Math/PI 4)) (:x to) (:y to))]
        [from {:x (-> ref-from first first) :y (-> ref-from second first)} {:x (-> ref-to first first) :y (-> ref-to second first)} to])
      nil)))

(defn- ext-rectangle [from to positive diverge]
  "takes a line from A to B, calcs an rectancle that exceeds the rectangular directly spanned by from->to"
  (let [start (cond
                (and (>= (:y from) (:y to)) (<= (:x from) (:x to))) :down
                (and (>= (:y from) (:y to)) (>= (:x from) (:x to))) :right
                (and (<= (:y from) (:y to)) (>= (:x from) (:x to))) :up
                (and (<= (:y from) (:y to)) (<= (:x from) (:x to))) :left)
        start (if 
                positive    
                start
                ({:down :left
                  :right :down
                  :up :right
                  :left :up} start))
        stop (if
               positive
               ({:down :right
                 :right :up
                 :up :left
                 :left :down} start)
               ({:left :up
                 :down :left
                 :right :down
                 :up :right} start))
        length (/ (line-length [from to]) diverge)
        dx {:down 0 :up 0 :left (- length) :right length}
        dy {:down length :up (- length) :right 0 :left 0}
        reference1 {:x (+ (dx start) (:x from)) :y (+ (dy start) (:y from))}
        reference2 {:x (+ (dx stop) (:x to)) :y (+ (dy stop) (:y to))}]
    (concat [from] (ring reference1 reference2 (not positive) diverge) [to])))

(defn- article-flow-fitness [article-flow]
  "an article flow is a vec [[{}{}]..[{}{}]], of vectors each containing a line"
  (let [article-flow (normalize-pathes article-flow)
        line-set (set article-flow)
        line-combination-set (dyn-for [line-set line-set])
        line-combination-set (set (map set line-combination-set))
        intersection-count (reduce
                             (fn [acc i]
                              (if (intersect? (first i) (last i))
                                (inc acc)
                                acc))
                             0 line-combination-set)]
    intersection-count))

(deftype TZigZag [^:unsynchronized-mutable state optimized-article-flow]
  clojure.lang.IDeref
  (deref [sc]
    (locking sc
      (or
        state
        (set! state
              (let [flats (atom 0)
                    bidirectional-flats (atom #{})
                    progress (ConsoleProgress. \-)
                    ! (.showProgress progress "flatten unnecessary zig-zags" 0)

                    optimized-article-flow (map
                                            (fn [i]
                                              (let [! (.showProgress progress (format "flatten unnecessary zig-zags, flats so far %d" @flats) (Math/ceil (* 100 (/ (inc i) (count optimized-article-flow))))) 
                                                    ;;! (println (drop-last (optimized-article-flow i)))
                                                    article-flow (map #(with-meta (-> % last first) {:origin (drop-last %)}) optimized-article-flow)
                                                    
                                                    ;;article flow will contain all other pathes than the currently inspected one
                                                    article-flow (normalize-pathes 
                                                                    (concat 
                                                                      (take i article-flow)
                                                                      (drop (inc i) article-flow)))
                                                    line-set (set article-flow)
                                                   
                                                    spec (take 3 (optimized-article-flow i))

                                                    ;;reference-flow will contain only the lines of the current flow
                                                    reference-flow (normalize-pathes [(-> (optimized-article-flow i) last first)])
                                                    ;;! (println :reference-flow reference-flow)
                                                    ;;! (println "the current setting")
                                                    reference-flow-intersect-count (reduce
                                                                                     (fn [acc i]
                                                                                       (+ acc
                                                                                          (count
                                                                                            (filter
                                                                                              (fn [other]
                                                                                                (if
                                                                                                  (intersect? i other)
                                                                                                  (do (comment (println :intersect i (meta i) :other other (meta other)))
                                                                                                      true)
                                                                                                  false))
                                                                                              line-set))))
                                                                                     0 reference-flow) 

                                                    ;;reference is a straight line
                                                    reference [(-> (optimized-article-flow i) last first first) (-> (optimized-article-flow i) last first last)]
                                                    m (ascent reference)
                                                    ;;count how many other lines are intersected
                                                    ;;! (println "the straigth setting")
                                                    intersects (count
                                                                 (filter
                                                                   (fn [other]
                                                                     (if 
                                                                       (intersect? reference other)
                                                                       (do (comment (println :intersect reference (meta reference) :other other (meta other)))
                                                                           true)
                                                                       false))
                                                                 line-set))
                                                    original (optimized-article-flow i)]
                                                (if
                                                  (and
                                                    (not (contains? @bidirectional-flats spec))
                                                    ;;we don't flat nice rings
                                                    (not= (-> (optimized-article-flow i) last first count) 3)
                                                    ;;we have less/equal intersects with a straight line
                                                    (<= intersects reference-flow-intersect-count))
                                                  (do
                                                    (let [reverse-spec (list (first spec) (last spec) (second spec))]
                                                      (swap! bidirectional-flats conj reverse-spec)
                                                      (swap! flats inc)
                                                      (concat (drop-last original)
                                                              [[(with-meta (vec reference) (meta original))]])))
                                                  ;;else
                                                  original)))
                                            (range (count optimized-article-flow)))]
                optimized-article-flow))))))

(defn- model-article-flow [xs article-flow]
  "models an article flow using xs ({area -> {article -> {:x :y}}}) and 
  article-flow which is '('(article from to amount)*).
  result is '('(article from to amount [[{:x :y}{:x :y}][{:x :y}{:x :y}{:x :y}{:x :y}]]))"
  (let [stage1 (map
                 (fn [[article from to amount]]
                   (let [path-straight [(get (get xs from) article) (get (get xs to) article)]
                         article-flow (reduce
                                      (fn [acc i]
                                        (let [options [(ring (get (get xs from) article) (get (get xs to) article) true i)
                                                       (ring (get (get xs from) article) (get (get xs to) article) false i)
                                                       (ring2 (get (get xs from) article) (get (get xs to) article) true i)
                                                       (ring2 (get (get xs from) article) (get (get xs to) article) false i)
                                                       (ext-rectangle (get (get xs from) article) (get (get xs to) article) true i)
                                                       (ext-rectangle (get (get xs from) article) (get (get xs to) article) false i)]]
                                          (concat acc options)))
                                      [path-straight] [5 6 7])
                         m (ascent [(get (get xs from) article) (get (get xs to) article)])
                         options (if
                                   (or (eq? 0 m) (nil? m))
                                   (concat article-flow
                                           [(trapez (get (get xs from) article) (get (get xs to) article) true 4)
                                            (trapez (get (get xs from) article) (get (get xs to) article) false 4)])
                                   article-flow)
                         ;;rounding is allowed if last line is large enought
                         options (map
                                   #(let [l (line-length [(-> % drop-last last) (last %)])]
                                      (if 
                                        (< l (* 8 (:article-radius goods-flow)))
                                        (with-meta % {:no-rounding true})
                                        %))
                                   options)]
                     (list article from to amount (vec (filter #(-> % nil? not) options)))))
                  article-flow)

        domain (map last stage1)

        ! (.showProgress progress "Start genetic optimization of pathes" 0)
        optimized-article-flow @(TGa. nil domain 4 article-flow-fitness 3 0.09 0.15 progress-agent)
        ;;now we map back the optimized pathes into the article flow
        optimized-article-flow (vec
                                 (map
                                    (fn [[article from to amount] v]
                                      (list article from to amount v))
                                    (map drop-last stage1)
                                    (map (fn [i] [(with-meta (vec i) (meta i))]) (first optimized-article-flow))))
        optimized-article-flow (vec @(TZigZag. nil optimized-article-flow))
        optimized-article-flow @(TZigZag. nil optimized-article-flow)]
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
        backward-reference (reference-point (vec (reverse last-line)) (:curve-radius goods-flow))
        curve-frauds-arrow (> 
                             (line-length [backward-reference (last concrete-path)])  
                             (line-length [reference (last concrete-path)]))

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
    {:curve-frauds-arrow curve-frauds-arrow
     :reference reference
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
        ;;adjust first and last point in order to start after the source and stop right before sink
        pos1 (-> x last select first)
        posn (-> x last select last)
        pos1post (-> x last select second)
        pos1 (reference-point [pos1post pos1] (/ (:article-radius goods-flow) 4))
        x (concat  
            (drop-last x)
            [(with-meta 
              [(vec
                 (concat [pos1] (-> x last select rest drop-last) [posn]))]
              (-> x last select meta))])

        arrow (apply render-arrow color-by-article x (-> x last select) args)
        args (if args (apply hash-map args) 
                 {})
        path (if
               (< (-> x last select count) 3)
               ;;case 1: straigth line
               (concat 
                 [\M (:x (r2d pos1)) (:y (r2d pos1))]
                 [\L (-> arrow :reference r2d :x) (-> arrow :reference r2d :y)])
               ;;else
               (let [index-path (zipmap (range) (-> x last select))
                     path (with-meta (reduce
                            (fn [acc index]
                              (let [preB (if (< index (-> index-path count dec))
                                           (r2d (reference-point [(get index-path index) (get index-path (inc index))] (:curve-radius goods-flow)))
                                           nil)
                                    preA (if (> index 0)
                                           (r2d (reference-point [(index-path (dec index)) (index-path index)] (:curve-radius goods-flow)))
                                           nil)
                                    postA (if (< index (-> index-path count dec))
                                           (r2d (reference-point [(index-path (inc index)) (index-path index)] (:curve-radius goods-flow)))
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
                            {:path [] :non-monotonic false} (range (count index-path))) (-> x last select meta))

                     ;;do it again with straight lines if we have a non-monotonic shape
                     path (if
                            (or
                              (contains? (meta path) :no-rounding)
                              (:non-monotonic path)
                              ;;(:curve-frauds-arrow arrow)
                              ) 
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
                                {:path [\L] :non-monotonic false} (range (count index-path)))
                            path)]
                (concat 
                 [\M (:x (r2d pos1)) (:y (r2d pos1))]
                 (:path path))))
        path (interpose " " path)]
    {:text (if
             (:stroke-text args)
             [:text {:class "stroke-text" :text-anchor "middle" 
                       :x (double (/ (apply + (map :x [pos1 posn])) 2))
                       :y (+ (double (/ (apply + (map :y [pos1 posn])) 2)) 6)} (:stroke-text args)]
             nil)
     :arrow (:arrow arrow)
     :path (if (:just-arrow args) nil
             [:path {:class "flow-path" :stroke (get color-by-article (-> x first)) :d (apply str path)
              ;;3 means amount (article von nach amount [pathes])
              :stroke-width (if (:stroke-width args) (:stroke-width args) (scale-throughtput (nth x 3)))}])}))

(defn- stats-article-group [goods-flow]
  "returns pick, drop as well as transfer times for all the article groups. all time goods-flow are measured in seconds"
  (do
    (println (-> goods-flow :goods-flow-constraints))
    (if-not (-> goods-flow :goods-flow-constraints :pick-delay)
      (throw (Exception. "Warenfluss muss das Attribut :pick-delay zur Kennzeichnung der Pickzeit in Sekunden enthalten")))
    (if-not (-> goods-flow :goods-flow-constraints :drop-delay)
      (throw (Exception. "Warenfluss muss das Attribut :drop-delay zur Kennzeichnung der Abstellzeit in Sekunden enthalten")))
    (if-not (-> goods-flow :goods-flow-constraints :transfer-delay)
      (throw (Exception. "Warenfluss muss das Attribut :transfer-delay zur Kennzeichnung der Transportzeit in Sekunden enthalten")))
    (reduce
      (fn [acc [k v]]
        (assoc acc k
        {:pick-delay (* (-> goods-flow :goods-flow-constraints :pick-delay) (apply + (map last v)))
          :drop-delay (* (-> goods-flow :goods-flow-constraints :drop-delay) (apply + (map last v)))
          :transfer-delay (* (-> goods-flow :goods-flow-constraints :transfer-delay) (apply + (map last v)))}))
      {} (:goods-flow goods-flow))))

(defn- convert-second-to-hour [s]
  (long
    (Math/ceil
      (double
        (/ s 3600)))))

(def css
  "text, div, td, th {
    font: 12px sans-serif;
    color: #777777;
  }
  
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
    stroke-linejoin: round;
  }
  
  .flow-arrow {
    fill-opacity: 0.7;
  }

  body {
    background: white;
  }
  
  ")

(def css-headline

   ".section {
      background-color: #eeeeee;
      border-top-color: black;
      border-top-width: 2px;
      border-top-style: solid;
    }
    
    th, td, div {
      font: 20px sans-serif;
    }")

(defn render [goods-flow]
  (let [;;scaling to visible pane
        width 1500
        height 1000
        min-x 0
        min-y 0
        zone-top-inline 35
        zone-left-right-inline 1
        top-inset 150
        left-inset 150
        max-x (apply + min-x (map :dx (-> goods-flow :topology)))
        max-y (:dy goods-flow)
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
                                                                        (/ (-> goods-flow :article-radius) 1))) 
                                                                    area-center)))
                                      article-distance (cond (= 1 article-count)
                                                              0
                                                              :else
                                                              (/ min-center-distance (dec article-count)))
                                      article-distance (max article-distance (* 2 (-> goods-flow :article-radius)))]
                                  
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

        fan-in-fan-out-by-article (reduce
                                    (fn [acc [article from to amount & r]]
                                      (let [existing (get acc article)
                                            existing (if existing existing {:in 0 :out 0})
                                            inc-in 
                                              (if 
                                                (= from (-> goods-flow :goods-flow-constraints :source)) 
                                                amount 
                                                0)
                                            inc-out 
                                              (if 
                                                (= to (-> goods-flow :goods-flow-constraints :sink)) 
                                                amount 
                                                0)]
                                        (assoc acc article
                                              (assoc existing
                                                      :in (+ (:in existing) inc-in)
                                                      :out (+ (:out existing) inc-out)))))
                                    {} article-flow)

        ;; a scaling function for the fan in fan out
        scale-fan (scale/linear :domain [0 (apply max (vals fan-in-fan-out))]
                        :range [0 (:max-fan-size goods-flow)])
        
        article-flow-model (model-article-flow article-position-by-area article-flow)

        stats-article-group (stats-article-group goods-flow)
        ]
    [:body

      [:div#headline {:style (str
                            "background: #ffffff; " 
                            "display: block; "
                            "margin: auto;")}
      [:style {:type "text/css"} css]
      [:style {:type "text/css"} css-headline]
      [:div {:class "section" :style "width: 100%"} 
        [:span {:style "font-weight: bold; font: 20px sans-serif; color: blue;"} \u03bb]
        [:span {:style "font-weight: bold; font: 16px sans-serif; color: black; padding-left: 2px;"} "Lambda Royal"]
        [:span {:style "font: 16px sans-serif; padding-left: 2px;"} " - Asthetics and Purity in Applied Computer Science  "]]]


      [:div {:class "logo-img1"} [:img {:src "/rocklog_logo_home.jpg"}]]
      [:svg#main {:style (str
                          "background: #ffffff; " 
                          "display: block; "
                          "margin: auto;"
                          "height:" (+ height (:bottom-inset goods-flow)) ";"
                          "width:" (+ width (:right-inset goods-flow)) ";")}
      [:style {:type "text/css"} css]
      
      ;;render title
      (unify
        [(:title goods-flow)]
        (fn [val]
          [:text 
            {:class "title-text" :x (/ width 2) :y (scale-dy top-inset ) :text-anchor "middle"}
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

      ;;render fan in & fan out using render-arrow [color-by-article x concrete-path]
      (let [in-center (get area-center (-> goods-flow :goods-flow-constraints :source))
            out-center (get area-center (-> goods-flow :goods-flow-constraints :sink))
            path [(render-article-flow-item 
                    (assoc color-by-article :stats-in "green" :stats-out "red") 
                    [:stats-in nil nil (:in fan-in-fan-out) [[{:x 0 :y (:y in-center)}{:x (+ (scale-x -10) (/ (:article-radius goods-flow) 2)) :y (:y in-center)}]]] 
                    :stroke-width (scale-fan (:in fan-in-fan-out)) :arrow-width (* 1.5 (scale-fan (:in fan-in-fan-out))) :stroke-text (:in fan-in-fan-out)
                    :just-arrow true)
                  (render-article-flow-item 
                    (assoc color-by-article :stats-in "green" :stats-out "red") 
                    [:stats-out nil nil (:out fan-in-fan-out) [[{:x (scale-x -10) :y (:y out-center)}{:x 0 :y (:y out-center)}]]] 
                    :stroke-width (scale-fan (:out fan-in-fan-out)) :arrow-width (* 1.5 (scale-fan (:out fan-in-fan-out))) :stroke-text (:out fan-in-fan-out)
                    :just-arrow true)]]
        (unify 
          (reduce  
            (fn [acc i]
              (conj acc (:path i) (:arrow i) (:text i)))
            [] path)
          identity))

    ]

    ;; render appendix
    (comment [:svg#appendix {:style (str
                          "background: #ffffff; " 
                          "display: block; "
                          "margin: auto;"
                          "height:" (+ (* 2 (:article-radius goods-flow)) (* (-> goods-flow :goods-flow keys count) (:article-radius goods-flow) 3)) ";"
                          "width:" width ";")}
      [:style {:type "text/css"} css]
      
      ;;render color bubbles below chart to designate the article groups
      (let [i-by-article (zipmap (keys (:goods-flow goods-flow)) (range))
            start-x 10
            y (:article-radius goods-flow)]
        (unify
          (reduce
            (fn [acc [k v]]
              (conj 
                acc
                ;;bubble
                [:circle {:cx start-x :cy (+ y (* (-> goods-flow :article-radius) 3 v)) :r (-> goods-flow :article-radius) :fill (get color-by-article k)}]
                ;;description
                [:text {:class "zone-text" 
                        :x (+ start-x (* (-> goods-flow :article-radius) 2)) 
                        :y (+ (+ y 6) (* (-> goods-flow :article-radius) 3 v))}  
                        k]
                ))
            [] i-by-article)
          identity))

    ])
    ;; render stats (picks, drops, tansfer per article group)
    [:div#stats {:style (str
                          "background: #ffffff; " 
                          "display: block; "
                          "margin: auto;")}
    [:style {:type "text/css"} css]
    [:style {:type "text/css"} 
      "
      
      th, td, div {
        font: 20px sans-serif;
      }

      th {
        color: #777777;
        font-weight: bold;
        background-color: lightblue;
      }

      .color {
        width: 20px;
      }
      
      tr.row-0 {
        background-color: #eeeeee;
      }

      tr.row-1 {
        background-color: #cccccc;
      }

      td {
        text-align: right;
        padding-right: 5px;
      }

      .section {
        background-color: #eeeeee;
        border-top-color: black;
        border-top-width: 2px;
        border-top-style: solid;
      }
      "]

    ;;statistik zu inbound/outbound
    [:div {:class "section" :style "width: 100%"} "Warengruppen und Inbound/Outbound"]
    [:br]
    [:table
      (unify
        [nil "Warengruppe" "Inbound" "Outbound"]
        (fn [val]
          [:th {:class (if (nil? val) "color" "domain")} val]))

      ;;render the stats, each stat is a map entry
      (let [background (atom 0)]
        (unify
          (keys stats-article-group)
          (fn [k]
            (let [{:keys [in out]} (get fan-in-fan-out-by-article k)]
              (do
                (swap! background inc)
                [:tr {:class (str "row-" (mod @background 2))}
                [:td {:style (format "background-color: %s;" (color-by-article k))} "  "] 
                [:td k] [:td in] [:td out]])))))]
    [:br]

    ;;statistik zu inbound/outbound
    [:div {:class "section" :style "width: 100%"} "Statistik"]
    [:br]
    [:table
      (unify
        [nil "Warengruppe" "Picktime (s)" "Picktime (h)" "Droptime (s)" "Droptime (h)" "Transfertime (s)" "Transfertime (h)"]
        (fn [val]
          [:th {:class (if (nil? val) "color" "domain")} val]))

      ;;render the stats, each stat is a map entry
      ;;Abfall {:pick-delay 9180, :drop-delay 12240, :transfer-delay 73440}
      (let [background (atom 0)]
        (unify
          stats-article-group
          (fn [[k v]]
            (let [{:keys [pick-delay drop-delay transfer-delay]} v]
              (do
                (swap! background inc)
                [:tr {:class (str "row-" (mod @background 2))}
                [:td {:style (format "background-color: %s;" (color-by-article k))} "  "] 
                [:td k] [:td pick-delay] [:td (-> pick-delay convert-second-to-hour)]  
                        [:td drop-delay] [:td (-> drop-delay convert-second-to-hour)]
                        [:td transfer-delay] [:td (-> transfer-delay convert-second-to-hour)]])))))]
    ]

  ;;[:div {:class "logo-lambdaroyal"} [:img {:src "/lambdaroyal.png"}]]
  ]))

;;(render goods-flow)


