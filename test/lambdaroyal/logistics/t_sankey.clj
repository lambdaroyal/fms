(ns lambdaroyal.logistics.t-sankey
  (:use midje.sweet)
  (:require [lambdaroyal.logistics.sankey :as sankey]))

;; definition of goods flow to create a visualization for
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
     ["Waschen" "Lift 1" 2000]]
    }

   :goods-flow-constraints
   {:source "Inbound"
    :sink "Outbound"
    :drop-delay 20
    :pick-delay 15
    :transfer-delay 120}
})

(fact (sankey/render goods-flow) =not=> nil?)
