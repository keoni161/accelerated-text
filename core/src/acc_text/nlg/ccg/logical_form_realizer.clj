(ns acc-text.nlg.ccg.logical-form-realizer
  (:require [acc-text.nlg.grammar-generation.translate :as translate])
  (:import opennlp.ccg.realize.Realizer))

(defn realization-lf [grammar]
  (let [lf-xml (translate/build-custom-el {:name "lf"})
        lex (.lexicon grammar)]
    (doseq [word (.keySet (.getWords lex))]
      (doseq [sign (.getSignsSorted (.getSignsFromWord lex word))]
        (translate/add-content lf-xml (.toXml (.getLF (.getCategory sign))))))
    (Realizer/getLfFromElt lf-xml)))

(defn realize [grammar]
  ;;Realizer takes a scorer as a second parameter.
  ;;Currently we do not score realizations but it is
  ;;we need to look into this.
  (.realize (Realizer. grammar) (realization-lf grammar)))
