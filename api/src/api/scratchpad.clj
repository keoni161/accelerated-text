(ns api.scratchpad
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]))

(defn update-semantic-graph
  ([{:keys [nodes relations] :as sg}
    new-nodes new-relations]
   (assoc sg
          :nodes (set (concat nodes new-nodes))
          :relations (set (concat relations new-relations))))
  ([sg new-nodes] (update-semantic-graph sg new-nodes [])))

(defmulti process-node (fn [_ node] (-> node :type (string/lower-case) keyword)))

(defmethod process-node :cell [sg {field :name}]
  (update-semantic-graph
   sg
   [{:type :data :field (keyword field) :id (str "data-" field)}]
   [#_[(:id parent) :has (str "data-" field)]]))

(defmethod process-node :dictionary-item-modifier
  [sg {child :child word-id :name}]
  (let [modifier-node {:type :modifier :dictionary (keyword word-id)
                       :id (str "modifier-" word-id)}
        {child-nodes :nodes child-rels :relations}
        (process-node sg child)]
    (update-semantic-graph
     sg
     (conj child-nodes modifier-node)
     (conj child-rels
           [;; The way relation here is constructed is not good.
            ;; In Blockly we get MODIFIER(Adjective)->CELL(Noun) structure.
            ;; Which is inverse from what we should have in semantic graph:
            ;; DATA->MODIFIER. Thus a reversing of relation is needed.
            ;; Reversing itself depends on child-nodes being in a *vector* data structure.
            (-> child-nodes first :id) :modified-by (:id modifier-node)]))))

;;In Blockly structure this is root AMR node - event. `dictionaryItem` is an
;;unfortunate name.
(defmethod process-node :dictionary-item [sg {type :type name :name}]
  (let [amr-node {:type :amr
                  :name (keyword name)
                  :id (str "amr-" name)}]
    (update-semantic-graph sg
                           [amr-node]
                           [#_[(:id parent) :amr (:id amr-node)]])))

(defn process-amr-arg [sg parent arg-name roles]
  (reduce (fn [m child] (process-node m parent child))
          sg
          (:children (some #(when (= arg-name (:name %)) %) roles))))

(defmethod process-node :amr [sg {roles      :roles
                                         dictionary :dictionaryItem}]
  (let [amr  (process-node sg dictionary)
        ;;See comment on `process-node :dictionary-item-modifier`
        amr-node (-> amr :nodes first)
        arg0 (process-amr-arg amr amr-node "agent" roles)
        arg1 (process-amr-arg arg0 amr-node "co-agent" roles)]
    (update-semantic-graph
     arg1
     (concat (:nodes amr) (:nodes arg0) (:nodes arg1)))))

(defmethod process-node :segment [sg {children :children}]
  (let [segment {:type :segment :id "segment"}] ;TODO ids for segments
    (reduce (fn [m child]
              (let [new-sg (process-node m child)]
                (prn new-sg)
                new-sg
                ))

            (update-semantic-graph sg
                                   [segment]
                                   [])
            children)))

(defn parse-document-plan [{segments :segments}]
  (let [doc {:type :document :id "document-1"}
        sg (reduce (fn [m seg] (process-node m seg)) {} segments)]
    (update-semantic-graph sg
                           []
                           ;;Attach all doc->segment relations
                           (->> sg
                                (:nodes)
                                (filter #(= :segment (:type %)))
                                (map #(vector (:id doc) :has (:id %)))))))

(declare parse-nodes2)

(defmulti goo (fn [x] (-> x :type (string/lower-case) keyword)))

(defmethod goo :default [x] {:UNK true})

(defmethod goo :segment [x]
  {:type :segment :id "segment"})

(defmethod goo :document-plan [x]
  {:type :document-plan :id "document-plan"})

(defmethod goo :cell [{field :name}]
  {:type :data :field (keyword field) :id (str "data-" field)})

(defmethod goo :dictionary-item-modifier [{word-id :name}]
  {:type :modifier :dictionary (keyword word-id)
   :id (str "modifier-" word-id)})

(defn block-children [block]
  (cond
    (contains? block :child) [(:child block)]
    (contains? block :segments) (:segments block)
    :else (:children block)))

(defn modifier-linked [[from _ to]]
  (or (string/starts-with? from "modifier-")
      (string/starts-with? to "modifier-")))

(defn reverse-modifier-data [{relations :relations :as semantic-graph}]
  (let [mod-linked (group-by modifier-linked relations)
        [from1 rel1 to1 :as X]
        (some #(when (string/starts-with? (first %) "modifier-") %)
              (get mod-linked true))
        [from2 rel2 to2 :as Y]
        (some #(when (string/starts-with? (last %) "modifier-") %)
              (get mod-linked true))
        reversed [[to1 rel1 from1] [from2 rel1 to2]]]
    (prn X)
    (prn Y)
    (assoc
     semantic-graph
     :relations (set (concat reversed (get mod-linked false))))))

(defn parse-nodes2 [parent-node block]
  (let [current-node (goo block)]
    (reduce
     (fn [sg child-block]
       (let [node (goo child-block)
             {child-nodes :nodes child-rels :relations}
             (parse-nodes2 current-node child-block)]
         (update-semantic-graph
          (update-semantic-graph sg child-nodes child-rels)
          [node]
          [[(:id current-node) :--> (:id node)]])))
     {:nodes #{parent-node current-node}
      :relations #{[(:id parent-node) :--> (:id current-node)]}}
     (block-children block))))

(defn parse-document-plan2 [dp]
  (reverse-modifier-data (parse-nodes2 {:type :ROOT :id :ROOT} dp)))

