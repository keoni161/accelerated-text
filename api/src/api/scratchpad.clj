(ns api.scratchpad
  (:require [clojure.string :as string]))

(defn update-semantic-graph
  ([{:keys [nodes relations] :as sg}
    new-nodes new-relations]
   (assoc sg
          :nodes (set (concat nodes new-nodes))
          :relations (set (concat relations new-relations))))
  ([sg new-nodes] (update-semantic-graph sg new-nodes [])))

(defmulti process-node (fn [_ _ node] (-> node :type (string/lower-case) keyword)))

(defmethod process-node :cell [sg parent {field :name}]
  (update-semantic-graph
   sg
   [{:type :data :field (keyword field) :id (str "data-" field)}]
   [[(:id parent) :has (str "data-" field)]]))

(defmethod process-node :dictionary-item-modifier
  [sg parent {child :child word-id :name}]
  (let [modifier-node {:type :modifier :dictionary (keyword word-id)
                       :id (str "modifier-" word-id)}
        {child-nodes :nodes child-rels :relations}
        ;;`parent` here would be `modifier-node` but we are reversing
        ;; graph relation, see comment bellow
        (process-node sg parent child)]
    (update-semantic-graph
     sg
     (conj child-nodes modifier-node)
     (conj child-rels
           [;; The way relation here is constructed is not good.
            ;; In Blockly we get MODIFIER(Adjective)->CELL(Noun) structure.
            ;; Which is inverse from what we should have in semantic graph:
            ;; DATA->MODIFIER. Thus a reversing of relation is needed.
            ;; Reversing itself depends on child-nodes being in a *vector* data structure.
            (-> child-nodes first :id)
            :modifier (:id modifier-node)]))))

;;In Blockly structure this is root AMR node - event. `dictionaryItem` is an
;;unfortunate name.
(defmethod process-node :dictionary-item [sg parent {type :type name :name}]
  (let [amr-node {:type :amr
                  :name (keyword name)
                  :id (str "amr-" name)}]
    (update-semantic-graph sg
                           [amr-node]
                           [[(:id parent) :amr (:id amr-node)]])))

(defn process-amr-arg [sg parent arg-name roles]
  (reduce (fn [m child] (process-node m parent child))
          sg
          (:children (some #(when (= arg-name (:name %)) %) roles))))

(defmethod process-node :amr [sg parent {roles      :roles
                                         dictionary :dictionaryItem}]
  (let [amr  (process-node sg parent dictionary)
        ;;See comment on `process-node :dictionary-item-modifier`
        amr-node (-> amr :nodes first)
        arg0 (process-amr-arg amr amr-node "agent" roles)
        arg1 (process-amr-arg arg0 amr-node "co-agent" roles)]
    (update-semantic-graph
     arg1
     (concat (:nodes amr) (:nodes arg0) (:nodes arg1)))))

(defmethod process-node :segment [sg parent {children :children}]
  (let [segment {:type :segment :id "segment"}] ;TODO ids for segments
    (reduce (fn [m child] (process-node m segment child))
            (update-semantic-graph sg
                                   [segment]
                                   [[(:id parent) :has (:id segment)]])
            children)))

(defn parse-document-plan [{segments :segments}]
  (reduce (fn [m seg] (process-node m {:type :document :id "document-1"} seg))
          {} segments))
