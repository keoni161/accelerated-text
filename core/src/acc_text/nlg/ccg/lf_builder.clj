(ns acc-text.nlg.ccg.lf-builder
  (:import java.io.StringReader
           org.jdom.input.SAXBuilder
           org.jdom.output.XMLOutputter)
  (:require [acc-text.nlg.grammar-generation.translate :as translate]))

(def she-buys-it-lf
  "<xml> <lf> <satop nom=\"w1:action\"> <prop name=\"buy\"/> <diamond mode=\"tense\"> <prop name=\"pres\"/> </diamond> <diamond mode=\"Actor\"> <nom name=\"w0:animate-being\"/> <prop name=\"pro3f\"/> <diamond mode=\"num\"> <prop name=\"sg\"/> </diamond> </diamond> <diamond mode=\"Patient\"> <nom name=\"w2:thing\"/> <prop name=\"pro3n\"/> <diamond mode=\"num\"> <prop name=\"sg\"/> </diamond> </diamond> </satop> </lf> </xml>")

(def tiny-grammar (Grammar. (-> "grammars/tiny/grammar.xml" java.io.File. .toURI .toURL)))

(defn parse [lf]
  (.build (SAXBuilder.) (StringReader. lf)))

(defn xml-print [dom]
  (.outputString (XMLOutputter.) dom))

(defn construct-lf []
  (translate/build-lf
   (translate/build-satop "X")))

(defn lex-words [g]
  (let [w (.getWords (.lexicon g))] (doseq [k (.keySet w)] (prn k ":" (.get w k)))))
