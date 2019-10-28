(ns acc-text.nlg.ccg.grammar-builder-test
  (:require [acc-text.nlg.ccg.grammar-builder :as sut]
            [acc-text.nlg.ccg.lexicon :as lex]
            [acc-text.nlg.ccg.logical-form-realizer :as lf-realizer]
            [acc-text.nlg.spec.lexicon :as lex-spec]
            [acc-text.nlg.spec.morphology :as morph-spec]
            [clojure.test :refer [deftest is]]))

(def single-fact-semantic-graph
  {:relations [{:from "01" :to "02" :role :segment}
               {:from "02" :to "03" :role :instance}]
   :concepts  [{:id "01" :type :document-plan}
               {:id "02" :type :segment}
               {:id "03" :type :data :value "title"}]})

(def modifier-semantic-graph
  {:relations [{:from "01" :to "02" :role :segment}
               {:from "02" :to "03" :role :instance}
               {:from "03" :to "04" :role :modifier}]
   :concepts  [{:id "01" :type :document-plan}
               {:id "02" :type :segment}
               {:id "03" :type :data :value "title"}
               {:id         "04"
                :type       :dictionary-item
                :value      "NN-good"
                :attributes {:name "good"}}]})

(def verb-semantic-graph
  {:nodes     #{{:type :ROOT :id "ROOT"}
                {:type :document-plan :id "document-plan"}
                {:type :segment :id "segment"}
                {:type :data :field :author :id "data-author"}
                {:type :data :field :title :id "data-title"}
                {:type :event :dictionary :write :id "modifier-good"}}
   :relations #{["ROOT" :--> "document-plan"]
                ["document-plan" :--> "segment"]
                ["segment" :--> "data-title"]
                ["event" :--> "data-author"]
                ["event" :--> "title-author"]}})

;; LF for the above graph used for realization
;; <lf><satop nom=\"w1\">
;;   <prop name=\"{{TITLE}}\"/>
;;   <diamond mode=\"Mod\">
;;     <nom name=\"w0\"/>
;;     <prop name=\"{{GOOD}}\"/>
;;   </diamond>
;; </satop></lf>


(deftest morphology-builder
  (is (= #{#::morph-spec{:word "{{TITLE}}" :stem  "{{TITLE}}" :predicate "{{TITLE}}"
                         :pos  :NP         :class nil     :macros    nil}
           #::morph-spec{:word "{{GOOD}}" :predicate "{{GOOD}}" :stem   "{{GOOD}}"
                         :pos  :ADJ       :class     nil :macros nil}}
         (sut/data-morphology modifier-semantic-graph))))

(deftest lexicon-builder
  (is (= #{"base-NP"}
         (set (map ::lex-spec/name
                   (lex/base-families (sut/data-morphology modifier-semantic-graph)))))))

(deftest logical-form-realization-for-single-fact-sg
  (let [g (sut/build-grammar single-fact-semantic-graph)
        sign (.getSign (lf-realizer/realize g))]
    (is (= "NP" (.getPOS sign)))
    (is (="{{TITLE}}" (.getOrthography sign)))))

(deftest logical-form-realization-for-modifier-sg
  (let [g (sut/build-grammar modifier-semantic-graph)
        sign (.getSign (lf-realizer/realize g))]
    (is (= "ADJ" (.getPOS sign)))
    (is (="{{GOOD}} {{TITLE}}" (.getOrthography sign)))))

#_(deftest logical-form-realization-for-verb-sg
  (let [g    (sut/build-grammar verb-semantic-graph)
        sign (.getSign (lf-realizer/realize g))]
    (is (= "NP" (.getPOS sign)))
    (is (="{{AUTHOR}} {{WRITE}} {{TITLE}}" (.getOrthography sign)))))
