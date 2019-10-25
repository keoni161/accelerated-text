(ns acc-text.nlg.ccg.grammar-builder-test
  (:require [acc-text.nlg.ccg.grammar-builder :as sut]
            [clojure.test :refer [deftest is]]
            [acc-text.nlg.spec.lexicon :as lex-spec]
            [acc-text.nlg.ccg.lf-builder :as lf-builder]
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

(def good-title-lf
  "<lf><satop nom=\"w1\">
    <prop name=\"{{TITLE}}\"/>
    <diamond mode=\"Mod\">
      <nom name=\"w0\"/>
      <prop name=\"{{GOOD}}\"/>
    </diamond>
  </satop></lf>")

(deftest morphology-builder
  (is (= #{#::morph-spec{:word "{{TITLE}}" :stem  "{{TITLE}}" :predicate "{{TITLE}}"
                         :pos  :NP         :class "title"     :macros    nil}
           #::morph-spec{:word "{{GOOD}}" :predicate "{{GOOD}}" :stem   "{{GOOD}}"
                         :pos  :ADJ       :class     "modifier" :macros nil}}
         (sut/data-morphology tiny-semantic))))

(deftest lexicon-builder
  (is (= #{"title-NP"}
         (set (map ::lex-spec/name
                   (sut/base-families (sut/data-morphology tiny-semantic)))))))

(deftest logical-form-realization
  (let [g (sut/build-grammar tiny-semantic)
        sign (.getSign (sut/realize g (lf-builder/parse good-title-lf)))]
    (is (= "ADJ" (.getPOS sign)))
    (is (="{{GOOD}} {{TITLE}}" (.getOrthography sign)))))
