(ns acc-text.nlg.ccg.grammar-builder-test
  (:require [api.scratchpad :as sut]
            [clojure.test :refer [deftest is]]
            [acc-text.nlg.spec.lexicon :as lex-spec]
            [acc-text.nlg.spec.morphology :as morph-spec]))

(def tiny-semantic
  {:nodes     #{{:type :ROOT :id "ROOT"}
                {:type :document-plan :id "document-plan"}
                {:type :segment :id "segment"}
                {:type :data :field :title :id "data-title"}
                {:type :modifier :dictionary :good :id "modifier-good"}}
   :relations #{["ROOT" :--> "document-plan"]
                ["document-plan" :--> "segment"]
                ["segment" :--> "data-title"]
                ["data-title" :--> "modifier-good"]}})

(deftest morphology-builder
  (is (= #{#::morph-spec{:word      "{{TITLE}}"
                         :pos       :NP
                         :class     "title"
                         :predicate nil :macros nil}
           #::morph-spec{:word      "{{GOOD}}"
                         :pos       :ADJ
                         :class     "modifier"
                         :predicate nil :macros nil}}
         (sut/data-morphology tiny-semantic))))

(deftest lexicon-builder
  (is (= #{"title-NP" "modifier-ADJ"}
         (set (map ::lex-spec/name
                   (sut/base-families (sut/data-morphology tiny-semantic)))))))
