(ns acc-text.nlg.ccg.ccg-debug
  (:require [acc-text.nlg.grammar-generation.translate :as translate])
  (:import java.io.StringReader
           org.jdom.input.SAXBuilder
           [org.jdom.output Format XMLOutputter]))

;; All sorts of helper functions to debug and understand
;; how OpenCCG works

(defn parse [lf]
  (.getRootElement
   (.build (SAXBuilder.) (StringReader. lf))))

(defn xml-print [dom]
  (.outputString (XMLOutputter. (Format/getPrettyFormat) ) dom))

(defn construct-lf []
  (translate/build-lf
   (translate/build-satop "X")))

(defn lex-words [g]
  (let [lf-xml (translate/build-custom-el {:name "lf"})
        l (.lexicon g)
        words (.getWords l)]
    (doseq [word (.keySet words)]
      (prn "WORD: " (.getForm word))
      (doseq [sign (.getSignsSorted (.getSignsFromWord l word))]
        (let [lf (.getLF (.getCategory sign))]
          (translate/add-content lf-xml (.toXml lf)))))
    (print (xml-print lf-xml))
    lf-xml))

(defn family->xml [family]
  (print
   (-> family
       translate/family->entry
       (.getEntries) first .getCat .toXml
       xml-print)))
