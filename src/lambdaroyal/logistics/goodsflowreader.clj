(ns lambdaroyal.logistics.goodsflowreader
  (:import [java.io File FileInputStream])
  (:import [org.lambdaroyal.util ConsoleProgress])
  (:import [org.apache.poi.hssf.usermodel HSSFSheet HSSFWorkbook HSSFRow HSSFCell]))

(def constants ^:const
  {:excel-file "Exceldatei"})

(def ^{:doc "Contains the most recencely loaded goods flow definition"}
  mru-goods-flow
  (agent {}))

(defn- workbook [filename]
  "gibt eines map {sheetname, sheet} zurück, die aus einem .xls File gelesen werden können"
  (let [stream (FileInputStream.
                 (File. filename))
        workbook (HSSFWorkbook. stream)
        stage1 (reduce
                 (fn [acc i]
                   (assoc acc
                          (.getSheetName workbook i)
                          (.getSheetAt workbook i)))
                 {} (range 0 (.getNumberOfSheets workbook)))]
    stage1))

(defn- zones [workbook]
  "Liesst alle Zonen aus dem Reiter \"Topologie\" in eine Liste von Tupeln {:name ... :dx ...} "
  (if-let [tab (get workbook "Topologie")]
    (reduce
      (fn [acc i]
        (conj acc
               {:name (.getStringCellValue (.getCell i 0))
                :dx (int (.getNumericCellValue (.getCell i 1)))
                :areas []}))
      []
      (rest
        (rest
          (take-while
            #(not
              (nil?
                (.getCell % 0)))
            (-> tab .rowIterator iterator-seq)))))
    (throw (Exception. "Die Exceldatei enthält keinen Reiter Topologie"))))

(defn- translate-boolean [german]
  "translates \"Ja\" to true"
  (and
    german
    (= "ja" (.toLowerCase german))))

(defn- areas [workbook]
  "Ergänzt die Topologiemap bestehend aus Liste von Tupeln {:name ... :dx ...}
  um eine Assoziation :areas auf Tupel {:name ... :dy ... :separated ...}"
  (let [tab (get workbook "Topologie")
        zones (zones workbook)
        zone-names (set
                     (map :name zones))
        stage (reduce
                (fn [acc i]
                  (let [area-name (.getStringCellValue (.getCell i 3))
                        area-dy (.getCell i 4)
                        area-dy (if area-dy (.getNumericCellValue area-dy) nil)]
                    (if-let [lru (or
                                  (if-let [current (.getStringCellValue (.getCell i 2))]
                                    (if (empty? current) nil current)
                                    nil)
                                  (:lru acc))]
                      (assoc acc
                            :lru lru
                            :areas (conj
                                      (:areas acc)
                                      {:name area-name
                                      :dy area-dy
                                      :separated (translate-boolean
                                                   (.getStringCellValue (.getCell i 5)))
                                      :zone lru}))
                      (throw (Exception. (format "Die Exceldatei kennzeichnet nicht die Zone für den Bereich %s. Die Zone kann leer sein, wenn ein zuvor definierter Bereich einer Zone zugeordnet ist." area-name))))))
                  {:areas [] :lru nil}
                  (rest
                    (rest
                      (take-while
                        #(not
                          (nil?
                            (.getCell % 3)))
                        (-> tab .rowIterator iterator-seq)))))
        stage (group-by #(:zone %) (:areas stage))
        stage (map #(assoc % :areas (get stage (:name %)))
                   zones)
        ]
    stage))

(defn- article-groups [workbook]
  "Liesst alle Artikelgruppen aus dem Reiter \"Warengruppen\" in ein Set"
  (if-let [tab (get workbook "Warengruppen")]
    (reduce
      (fn [acc i]
        (conj acc (.getStringCellValue (.getCell i 0))))
      #{}
      (rest
        (take-while
          #(not
             (nil?
               (.getCell % 0)))
          (-> tab .rowIterator iterator-seq))))
    (throw (Exception. "Die Exceldatei enthält keinen Reiter Warengruppen"))))

(defn- constraints-mapper [k]
  "Gibt Mappingfunktionen und Zielschlüssel für die Einträge des
  Reiters \"Darstellungsparameter\" anhand eines schlüssels k zurück"
  (get
    {"Titel" {:k :title :f #(.getStringCellValue %)}
     "Quelle" {:k :source :f #(.getStringCellValue %)}
     "Ziel" {:k :sink :f #(.getStringCellValue %)}
     "Drop Zeit (s)" {:k :drop-delay :f #(.getNumericCellValue %)}
     "Pick Zeit (s)" {:k :pick-delay :f #(.getNumericCellValue %)}
     "Transfer Zeit (s)" {:k :transfer-delay :f #(.getNumericCellValue %)}
     "Artikel Radius (Pixel)" {:k :article-radius :f #(.getNumericCellValue %)}
     "Unterer Rand (Pixel)" {:k :bottom-inset :f #(.getNumericCellValue %)}
     "Rechter Rand (Pixel)" {:k :right-inset :f #(.getNumericCellValue %)}
     "Höhe Ausgabe (Pixel)" {:k :dy :f #(.getNumericCellValue %)}
     "Kurven Radius (Pixel)" {:k :curve-radius :f #(.getNumericCellValue %)}
     "Maximale Grösse Eingangs-/Ausgangsfluss (Pixel)" {:k :max-fan-size :f #(.getNumericCellValue %)}
     "Maximale Flussgrösse (Faktor)" {:k :max-throughtput-factor :f #(.getNumericCellValue %)}}
    k))




(defn- constraints [workbook sheet names]
  "Liesst alle Einträge aus dem Reiter sheet und mappt die Ergebnisse mittels
  constraints-mapper auf eine map"
  (let [stage (if-let [tab (get workbook sheet)]
                (reduce
                  (fn [acc i]
                    (assoc
                      acc
                      (.getStringCellValue (.getCell i 0))
                      (.getCell i 1)))
                  {}
                  (rest
                    (rest
                      (take-while
                        #(not
                          (nil?
                            (.getCell % 0)))
                        (-> tab .rowIterator iterator-seq)))))
                (throw (Exception. (format "Die Exceldatei enthält keinen Reiter %s" sheet))))
      stage2 (select-keys stage names)
      stage3 (reduce
               (fn [acc [k v]]
                 (let [c (constraints-mapper k)]
                   (assoc acc (:k c) ((:f c) v))))
               {} stage2)]
    stage3))


(defn- goods-flow [workbook]
  "Liesst alle Warenflüsse aus dem Reiter \"Warenflüsse\" und legt diese als Map von Entries
  Warengruppe->[[bereich-von bereich-nach menge]*] zurück"
  (let [tab (get workbook "Warenflüsse")
        article-groups (article-groups workbook)
        stage (reduce
                (fn [acc i]
                  (let [area-from (.getStringCellValue (.getCell i 1))
                        area-to (.getStringCellValue (.getCell i 2))
                        amount (.getNumericCellValue (.getCell i 3))]
                    ;;lru denotes the article-group currently in use
                    (if-let [lru (or
                                  (if-let [current (.getStringCellValue (.getCell i 0))]
                                    (if (empty? current) nil current)
                                    nil)
                                  (:lru acc))]
                      (assoc
                        acc
                        :lru lru
                        :goods-flow
                        (assoc
                          (:goods-flow acc)
                          lru
                          (let [existing (get (:goods-flow acc) lru)
                                new [area-from area-to amount]]
                            (if
                              existing
                              (conj existing new)
                              [new]))))
                      (throw (Exception. (format "Die Exceldatei kennzeichnet nicht den Bereich für den Warenfluss von %s nach %s mit Menge %d. Der Bereich kann leer sein, wenn ein zuvor definierter Warenfluss einem Bereich zugeordnet ist." area-from area-to amount))))))
                  {:areas [] :lru nil}
                  (rest
                    (take-while
                      #(not
                        (nil?
                          (.getCell % 1)))
                      (-> tab .rowIterator iterator-seq))))
        ! (send mru-goods-flow merge (:goods-flow stage))
        ]
    (:goods-flow stage)))

(defn load-xls [filename]
  (let [wb (workbook filename)]
    (assoc
      (constraints wb "Darstellungsparameter" ["Titel"
                                               "Artikel Radius (Pixel)"
                                               "Höhe Ausgabe (Pixel)"
                                               "Kurven Radius (Pixel)"
                                               "Maximale Grösse Eingangs-/Ausgangsfluss (Pixel)"
                                               "Maximale Flussgrösse (Faktor)"
                                               "Unterer Rand (Pixel)"
                                               "Rechter Rand (Pixel)"])
      :goods-flow-constraints
      (constraints wb "Darstellungsparameter" ["Quelle" "Ziel" "Drop Zeit (s)"
                                               "Pick Zeit (s)"
                                               "Transfer Zeit (s)"])
      :topology
      (vec
        (areas wb))
      :article-groups
      (article-groups wb)
      :goods-flow
      (goods-flow wb))))

