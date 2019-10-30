(ns acc-text.nlg.ccg.logical-form-realizer
  (:require [acc-text.nlg.grammar-generation.translate :as translate])
  (:import opennlp.ccg.realize.Realizer
           opennlp.ccg.synsem.Sign
           opennlp.ccg.hylo.HyloHelper
           ))

(defn realization-lf [grammar]
  (let [lf-xml (translate/build-custom-el {:name "lf"})
        lex (.lexicon grammar)]
    (doseq [word (.keySet (.getWords lex))]
      (doseq [sign (.getSignsSorted (.getSignsFromWord lex word))]
        (translate/add-content lf-xml (.toXml (.getLF (.getCategory sign))))
        #_(if (= "s" (.getSupertag (.getTarget (.getCategory sign))))
          (do
            (prn (.getSupertag (.getTarget (.getCategory sign))) " - " (.getCategory sign))
            (prn " -- " (.getLF (.getCategory sign)))
            (translate/add-content lf-xml (.toXml (.getLF (.getCategory sign))))))))
    (Realizer/getLfFromElt lf-xml)))

(defn root-sign [grammar]
  (let [lex (.lexicon grammar)
        signs (mapcat (fn [word]
                        (-> lex
                            (.getSignsFromWord word)
                            .getSignsSorted))
                      (.keySet (.getWords lex)))]
    (first (filter (fn [sign]
                     (= "s" (.getSupertag (.getTarget (.getCategory sign)))))
                   signs))))

(defn np-signs [grammar]
  (let [lex   (.lexicon grammar)
        signs (mapcat (fn [word]
                        (-> lex
                            (.getSignsFromWord word)
                            .getSignsSorted))
                      (.keySet (.getWords lex)))]
    (filter (fn [sign]
              (= "np" (.getSupertag (.getTarget (.getCategory sign)))))
            signs)))

(defn convert-lf
  "Converts Sign LF to proper one for realization"
  [^Sign sign]
  (let [cat   (.getCategory sign)
        index (.getIndexNominal cat)]
    (HyloHelper/compactAndConvertNominals (.getLF cat) index sign)))

(defn sign->lf [sign] (.getLF (.getCategory sign)))

(defn combined-lf [grammar]
  (let [main-lf (sign->lf (root-sign grammar))
        [np1 np2] (np-signs grammar)]
    (HyloHelper/append
      (HyloHelper/append main-lf (sign->lf np1))
      (sign->lf np2))))

(defn realize [grammar]
  ;;Realizer takes a scorer as a second parameter.
  ;;Currently we do not score realizations but it is
  ;;we need to look into this.
  (.realize (Realizer. grammar) (realization-lf grammar)))
