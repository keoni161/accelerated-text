(ns api.scratchpad
  (:require [clojure.string :as string]))

(defn update-semantic-graph
  ([{:keys [nodes relations] :as sg}
    new-nodes new-relations]
   (assoc sg
          :nodes (set (concat nodes new-nodes))
          :relations (set (concat relations new-relations))))
  ([sg new-nodes] (update-semantic-graph sg new-nodes [])))

(defmulti build-node (fn [node] (-> node :type (string/lower-case) keyword)))

(defmethod build-node :default [node] {:UNK node})

(defmethod build-node :segment [_]
  {:type :segment :id "segment"})

(defmethod build-node :document-plan [_]
  {:type :document-plan :id "document-plan"})

(defmethod build-node :cell [{field :name}]
  {:type :data :field (keyword field) :id (str "data-" field)})

(defmethod build-node :dictionary-item-modifier [{word-id :name}]
  {:type :modifier :dictionary (keyword word-id)
   :id (str "modifier-" word-id)})

;;In Blockly structure this is root AMR node - event. `dictionaryItem` is an
;;unfortunate name.
(defmethod build-node :dictionary-item [{node-name :name}]
  {:type :amr
   :name (keyword node-name)
   :id (str "amr-" node-name)})

(defn block-children
  "Since block tree data structure coming from UI differs in how different
  types of parent nodes represent children (not all are under `children` key),
  there we need to wrap how those children are located.

  FIXME. Change JS part to return all children under `children`"
  [block]
  (cond
    (contains? block :child) [(:child block)]
    (contains? block :segments) (:segments block)
    :else (:children block)))

(defn modifier-linked [[from _ to]]
  (or (string/starts-with? from "modifier-")
      (string/starts-with? to "modifier-")))

(defn reverse-modifier-data
  [{relations :relations :as semantic-graph}]
  ;; The way relation here is constructed is not good.
  ;; In Blockly we get MODIFIER(Adjective)->CELL(Noun) structure.
  ;; Which is inverse from what we should have in semantic graph:
  ;; DATA->MODIFIER. Thus a reversing of relation is needed.
  ;; Reversing itself depends on child-nodes being in a *vector* data structure.
  (let [mod-linked (group-by modifier-linked relations)
        [from1 rel1 to1]
        (some #(when (string/starts-with? (first %) "modifier-") %)
              (get mod-linked true))
        [from2 _ _]
        (some #(when (string/starts-with? (last %) "modifier-") %)
              (get mod-linked true))
        reversed [[from2 rel1 to1] [to1 rel1 from1]]]
    (assoc
     semantic-graph
     :relations (set (concat reversed (get mod-linked false))))))

(defn parse-nodes [parent-node block]
  (let [current-node (build-node block)]
    (reduce
     (fn [sg child-block]
       (let [node (build-node child-block)
             {child-nodes :nodes child-rels :relations}
             (parse-nodes current-node child-block)]
         (update-semantic-graph
          (update-semantic-graph sg child-nodes child-rels)
          [node]
          [[(:id current-node) :--> (:id node)]])))
     {:nodes #{parent-node current-node}
      :relations #{[(:id parent-node) :--> (:id current-node)]}}
     (block-children block))))

(defn parse-document-plan [dp]
  (reverse-modifier-data (parse-nodes {:type :ROOT :id "ROOT"} dp)))
