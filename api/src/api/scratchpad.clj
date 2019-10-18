(ns api.scratchpad
  (:require [clojure.string :as string]))

(defn node-id [prefix index] (keyword (str prefix (or index ""))))

(defn update-semantic-graph [sg nodes relations]
  (assoc sg
         (concat (get sg :nodes) nodes)
         (concat (get sg :relations) relations)))

(defmulti process-node (fn [_ node] (-> node :type (string/lower-case) keyword)))

(defmethod process-node :cell [sg {field :name}]
  (update-semantic-graph sg
                         [{:type :data :id (node-id (first field) 0)}]
                         []))

(defmethod process-node :dictionary-item-modifier [sg {child :child item-id :itemId word-id :name}]
  (let [{child-nodes :nodes child-relations :relations} (process-node sg child)]
    (update-semantic-graph sg
                           (conj child-nodes {:type :modifier :id (node-id item-id 0) :dictionary word-id})
                           (conj child-relations []))))

(defn parse-document-plan [{segments :segments}]
  (map-indexed
    (fn [idx child]
      (process-node (update-semantic-graph {:nodes [] :relations []}
                                           [{:type :segment :id (node-id "s" idx)}]
                                           [])
                    child))
    segments))
