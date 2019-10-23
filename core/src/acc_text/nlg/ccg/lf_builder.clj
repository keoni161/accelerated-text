(ns acc-text.nlg.ccg.lf-builder
  (:import java.io.StringReader
           opennlp.ccg.grammar.Grammar
           opennlp.ccg.realize.Realizer
           org.jdom.input.SAXBuilder
           org.jdom.output.XMLOutputter))

(def she-buys-it-lf
  "<xml> <lf> <satop nom=\"w1:action\"> <prop name=\"buy\"/> <diamond mode=\"tense\"> <prop name=\"pres\"/> </diamond> <diamond mode=\"Actor\"> <nom name=\"w0:animate-being\"/> <prop name=\"pro3f\"/> <diamond mode=\"num\"> <prop name=\"sg\"/> </diamond> </diamond> <diamond mode=\"Patient\"> <nom name=\"w2:thing\"/> <prop name=\"pro3n\"/> <diamond mode=\"num\"> <prop name=\"sg\"/> </diamond> </diamond> </satop> </lf> </xml>")

(def grammar (Grammar. (-> "grammars/tiny/grammar.xml" java.io.File. .toURI .toURL)))

(defn parse [lf]
  (.build (SAXBuilder.) (StringReader. lf)))

(defn xml-print [dom]
  (.outputString (XMLOutputter.) dom))

(defn realize [lf]
  (let [lf (Realizer/getLfFromDoc (parse lf))
        r (Realizer. grammar)]
    (.realize r lf nil)))
