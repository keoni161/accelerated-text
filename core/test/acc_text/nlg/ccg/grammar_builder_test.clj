(ns acc-text.nlg.ccg.grammar-builder-test
  (:require [acc-text.nlg.ccg.grammar-builder :as sut]
            [acc-text.nlg.ccg.logical-form-realizer :as lf-realizer]
            [clojure.test :refer [deftest is]]
            [acc-text.nlg.spec.lexicon :as lex-spec]
            [acc-text.nlg.spec.morphology :as morph-spec]))

(def single-fact-semantic-graph
  {:nodes #{{:type :data :field :title :id "data-title"}}
   :relations #{}})

(def tiny-semantic-graph
  {:nodes     #{{:type :ROOT :id "ROOT"}
                {:type :document-plan :id "document-plan"}
                {:type :segment :id "segment"}
                {:type :data :field :title :id "data-title"}
                {:type :modifier :dictionary :good :id "modifier-good"}}
   :relations #{["ROOT" :--> "document-plan"]
                ["document-plan" :--> "segment"]
                ["segment" :--> "data-title"]
                ["data-title" :--> "modifier-good"]}})

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
                         :pos  :NP         :class "title"     :macros    nil}
           #::morph-spec{:word "{{GOOD}}" :predicate "{{GOOD}}" :stem   "{{GOOD}}"
                         :pos  :ADJ       :class     "modifier" :macros nil}}
         (sut/data-morphology tiny-semantic-graph))))

(deftest lexicon-builder
  (is (= #{"title-NP"}
         (set (map ::lex-spec/name
                   (sut/base-families (sut/data-morphology tiny-semantic-graph)))))))

(deftest logical-form-realization-for-single-fact-sg
  (let [g (sut/build-grammar single-fact-semantic-graph)
        sign (.getSign (lf-realizer/realize g))]
    (is (= "NP" (.getPOS sign)))
    (is (="{{TITLE}}" (.getOrthography sign)))))

(deftest logical-form-realization-for-tiny-sg
  (let [g (sut/build-grammar tiny-semantic-graph)
        sign (.getSign (lf-realizer/realize g))]
    (is (= "ADJ" (.getPOS sign)))
    (is (="{{GOOD}} {{TITLE}}" (.getOrthography sign)))))
