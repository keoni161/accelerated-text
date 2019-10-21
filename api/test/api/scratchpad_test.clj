(ns api.scratchpad-test
  (:require [api.scratchpad :as sut]
            [clojure.test :refer [deftest is]]))

(def amr-dp {:type "Document-plan"
             :segments
             [{:children
               [{:roles [{:children [{:name   "good"
                                      :type   "Dictionary-item-modifier"
                                      :child  {:name "authors" :type "Cell"}
                                      :itemId "good"}]
                          :name     "agent"}
                         {:children [{:name "title" :type "Cell"}]
                          :name     "co-agent"}
                         {:children [] :name "theme"}]
                 :dictionaryItem {:name   "written"
                                  :type   "Dictionary-item"
                                  :itemId "written"}
                 :type           "AMR"
                 :conceptId      "author"}]
               :type     "Segment"}]})

(def tiny-dp
  {:segments
   [{:children
     [{:child  {:name "title" :type "Cell"}
       :name   "good"
       :type   "Dictionary-item-modifier"
       :itemId "NN-good"}]
     :type  "Segment"}]
   :type  "Document-plan"})

(def tiny-semantic
  {:nodes     #{{:type :ROOT :id "ROOT"}
                {:type :document-plan :id"document-plan"}
                {:type :segment :id "segment"}
                {:type :data :field :title :id "data-title"}
                {:type :modifier :dictionary :good :id "modifier-good"}}
   :relations #{["ROOT" :--> "document-plan"]
                ["document-plan" :--> "segment"]
                ["segment" :--> "data-title"]
                ["data-title" :--> "modifier-good"]}})

(def amr-semantic
  {:nodes     #{{:type :segment :id "segment"}
               {:type :dictionary-item :name :written :id "dictionary-written"}
               {:type :modifier :dictionary :good :id "modifier-good"}
               {:type :data :field :authors :id "data-authors"}
               {:type :data :field :title :id "data-title"}}
   :relations #{["document-1" :has "segment"]}})

(deftest dp->semantic
  (is (= tiny-semantic (sut/parse-document-plan tiny-dp)))
  #_(is (= amr-semantic (sut/parse-document-plan amr-dp))))
