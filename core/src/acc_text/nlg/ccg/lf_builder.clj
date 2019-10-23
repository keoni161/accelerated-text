(ns acc-text.nlg.ccg.lf-builder
  (:require [acc-text.nlg.grammar-generation.translate :as translate])
  (:import java.io.StringReader
           org.jdom.input.SAXBuilder
           [org.jdom.output Format XMLOutputter]))

(def she-buys-it-lf
  "<xml> <lf> <satop nom=\"w1:action\"> <prop name=\"buy\"/> <diamond mode=\"tense\"> <prop name=\"pres\"/> </diamond> <diamond mode=\"Actor\"> <nom name=\"w0:animate-being\"/> <prop name=\"pro3f\"/> <diamond mode=\"num\"> <prop name=\"sg\"/> </diamond> </diamond> <diamond mode=\"Patient\"> <nom name=\"w2:thing\"/> <prop name=\"pro3n\"/> <diamond mode=\"num\"> <prop name=\"sg\"/> </diamond> </diamond> </satop> </lf> </xml>")

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
