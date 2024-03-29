;; Author: christian.meichsner@informatik.tu-chemnitz.de

(ns lambdaroyal.logistics.algorithm.genetic
  (:import [org.lambdaroyal.util ConsoleProgress])
  (:gen-class))

;;genetic algorithm consists of
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
