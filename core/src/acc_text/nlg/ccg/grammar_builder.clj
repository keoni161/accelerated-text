(ns acc-text.nlg.ccg.grammar-builder
  (:import opennlp.ccg.grammar.Grammar
           opennlp.ccg.realize.Realizer
           opennlp.ccg.Realize)
  (:require [acc-text.nlg.grammar :as ccg]
            [acc-text.nlg.grammar-generation.translate :as translate]
            [acc-text.nlg.ccg.lexicon :as lex]
            [acc-text.nlg.ccg.morphology :as morph]))

(defn data-morphology [{concepts :concepts}]
  (->> concepts
       (filter (fn [{t :type}] (get  #{:event :data :dictionary-item} t)))
       (map morph/entry)
       (set)))

(defn build-grammar [sem-graph]
  (let [morphology (data-morphology sem-graph)
        families   (conj (lex/base-families morphology) (lex/modifier-family 1))
        #_(concat base-en/initial-families (base-families morphology))
        ]

    (ccg/build-grammar (map translate/family->entry families)
                       (map translate/morph->entry morphology)
                       [])))

