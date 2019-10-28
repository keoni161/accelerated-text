(ns acc-text.nlg.ccg.morphology
  (:require [acc-text.nlg.dsl.core :as dsl]
            [clojure.string :as string]))

(defn placeholder [kw] (format "{{%s}}" (-> kw name string/upper-case)))

(defmulti entry (fn [node] (:type node)))

(defmethod entry :default [x]
  (prn "TODO morph for: " x))

(defmethod entry :event [{dictionary :dictionary}]
  (dsl/morph-entry (placeholder dictionary) :V))

(defmethod entry :data [{value :value}]
  (dsl/morph-entry (placeholder value) :NP))

(defmethod entry :dictionary-item [{{dic :name} :attributes}]
  (dsl/morph-entry (placeholder dic) :ADJ))
