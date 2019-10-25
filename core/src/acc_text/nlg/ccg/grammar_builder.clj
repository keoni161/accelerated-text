(ns acc-text.nlg.ccg.grammar-builder
  (:import opennlp.ccg.grammar.Grammar
           opennlp.ccg.realize.Realizer)
  (:require [acc-text.nlg.dsl.core :as dsl]
            [acc-text.nlg.grammar :as ccg]
            [acc-text.nlg.grammar-generation.translate :as translate]
            [acc-text.nlg.spec.morphology :as morph-spec]
            [clojure.string :as string]
            [acc-text.nlg.ccg.base-en :as base-en]))

(defn placeholder [kw] (format "{{%s}}" (-> kw name string/upper-case)))

(defmulti morph-entry (fn [node] (:type node)))

(defmethod morph-entry :default [x]
  (prn "TODO morph for: " x))

(defmethod morph-entry :data [{field :field}]
  (dsl/morph-entry (placeholder field) :NP {:class (name field)}))

(defmethod morph-entry :modifier [{dic :dictionary}]
  (dsl/morph-entry (placeholder dic) :ADJ {:class "modifier"}))

(defn data-morphology [{nodes :nodes}]
  (->> nodes
       (filter (fn [{t :type}] (get  #{:data :modifier} t)))
       (map morph-entry)
       (set)))

(defn base-families [morphology]
  (map-indexed
    (fn [idx {word ::morph-spec/word pos ::morph-spec/pos class ::morph-spec/class}]
      (dsl/family
        (format "%s-%s" (name class) (name pos))
        pos
        true
        (dsl/entry "primary"
                   (dsl/lf "X")
                   (dsl/atomcat pos {:index idx} (dsl/fs-nomvar "index" "X")))
        (dsl/member word)))
    morphology))

(defn build-grammar [sem-graph]
  (let [morphology (data-morphology sem-graph)
        families (concat base-en/initial-families (base-families morphology))]

    (ccg/build-grammar (map translate/family->entry families)
                       (map translate/morph->entry morphology)
                       [])))

(defn realize-data-templates [grammar]
  (ccg/generate grammar "{{GOOD}}" "{{TITLE}}"))

(defn realize [grammar lf]
  (let [lf (Realizer/getLfFromElt lf)
        r (Realizer. grammar)]
    (.realize r lf nil)))
