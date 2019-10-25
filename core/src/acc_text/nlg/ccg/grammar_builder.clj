(ns acc-text.nlg.ccg.grammar-builder
  (:import opennlp.ccg.grammar.Grammar
           opennlp.ccg.realize.Realizer
           opennlp.ccg.Realize)
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
  (dsl/morph-entry (placeholder field) :NP {:stem (placeholder field) :class (name field)}))

(defmethod morph-entry :modifier [{dic :dictionary}]
  (dsl/morph-entry (placeholder dic) :ADJ {:stem (placeholder dic) :class "modifier"}))

(defn data-morphology [{nodes :nodes}]
  (->> nodes
       (filter (fn [{t :type}] (get  #{:data :modifier} t)))
       (map morph-entry)
       (set)))

(defn base-families [morphology]
  (->> morphology
       (remove (fn [{pos ::morph-spec/pos}] (= :ADJ pos)))
       (map-indexed
         (fn [idx {word ::morph-spec/word pos ::morph-spec/pos class ::morph-spec/class}]
           (dsl/family
             (format "%s-%s" (name class) (name pos))
             pos
             true
             (dsl/entry "primary"
                        (dsl/lf word (dsl/prop "[*DEFAULT*]"))
                        (dsl/atomcat pos {:index 2} (dsl/fs-nomvar "index" "X")))
             (dsl/member word))))))

(defn adj-family []
  (dsl/family "Modifier"
              :ADJ
              false
              (dsl/entry
                "Primary"
                (dsl/lf "X"
                        (dsl/prop "[*DEFAULT*]")
                        (dsl/diamond "Mod" {:nomvar "M"
                                            :prop (dsl/prop "[*DEFAULT*]")
                                            }))
                (dsl/>F
                  \^
                  (dsl/atomcat :NP {:inherits-from 2} (dsl/fs-nomvar "mod-index" "M"))
                  (dsl/atomcat :NP {:index 2} (dsl/fs-nomvar "index" "X"))))))

(defn build-grammar [sem-graph]
  (let [morphology (data-morphology sem-graph)
        families   (conj (base-families morphology) (adj-family))
        #_(concat base-en/initial-families (base-families morphology))
        ]

    (ccg/build-grammar (map translate/family->entry families)
                       (map translate/morph->entry morphology)
                       [])))

(defn realize-data-templates [grammar]
  (ccg/generate grammar "{{GOOD}}" "{{TITLE}}"))

(defn realize [grammar lf]
  (let [lf (Realizer/getLfFromElt lf)
        r (Realizer. grammar)]
    (.realize r lf nil)))
