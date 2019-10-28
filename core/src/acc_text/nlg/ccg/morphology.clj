(ns acc-text.nlg.ccg.morphology
  (:require [acc-text.nlg.dsl.core :as dsl]
            [clojure.string :as string]))

(defn placeholder [kw] (format "{{%s}}" (-> kw name string/upper-case)))

(defmulti entry (fn [node] (:type node)))

(defmethod entry :default [x]
  (prn "TODO morph for: " x))

(defmethod entry :event [{dictionary :dictionary}]
  (dsl/morph-entry (placeholder dictionary) :V {:stem (placeholder dictionary) :class "EVENT"}))

(defmethod entry :data [{field :field}]
  (dsl/morph-entry (placeholder field) :NP {:stem (placeholder field) :class (name field)}))

(defmethod entry :modifier [{dic :dictionary}]
  (dsl/morph-entry (placeholder dic) :ADJ {:stem (placeholder dic) :class "modifier"}))
