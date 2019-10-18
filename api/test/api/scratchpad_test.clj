(ns api.scratchpad-test
  (:require [api.scratchpad :as sut]
            [clojure.test :refer [deftest is]]))


(def amr-dp {:type "Document-plan"
             :segments
             [{:children [{:roles
                           [{:children [{:name   "good"
                                         :type   "Dictionary-item-modifier"
                                         :child  {:name "authors" :type "Cell"}
                                         :itemId "good"}]
                             :name     "agent"}
                            {:children [{:name "title" :type "Cell"}]
                             :name     "co-agent"}
                            {:children [nil] :name "theme"}]
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
  {:nodes     [{:type :segment :id :s1}
               {:type :data :filed :title :id :t1}
               {:type :modifier :dictionary :good1 :id :g1}]
   :relations [[:s1 :has :t1]
               [:p1 :ancillary :g1]]})

(def amr-semantic
  {:nodes     [{:type :segment :id :s1}
               {:type :data :filed :title :id :t1}
               {:type :data :filed :author :id :p1}
               {:type :modifier :dictionary :good1 :id :g1}
               {:type :event :dictionary :authorship1 :id :a1}]
   :relations [[:s1 :has :a1]
               [:a1 :arg0 :p1]
               [:a1 :arg1 :t1]
               [:p1 :ancillary :g1]]})

(deftest dp->semantic
  (is (= tiny-semantic (sut/parse-document-plan tiny-dp)))
  (is (= amr-semantic (sut/parse-document-plan amr-dp))))
