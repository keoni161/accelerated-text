(ns acc-text.nlg.grammar
  (:require [acc-text.nlg.combinator :as combinator]
            [acc-text.nlg.realizer :as realizer]
            [acc-text.nlg.utils :as utils]
            [clojure.tools.logging :as log])
  (:import [org.jdom Element]
           [opennlp.ccg.grammar Grammar ForwardApplication BackwardApplication] 
           [opennlp.ccg.builders LexiconBuilder RulesBuilder TypesBuilder GrammarBuilder]
           [opennlp.ccg.lexicon Family DataItem EntriesItem MorphItem MacroItem]
           [opennlp.ccg.synsem Sign]))

(defn element->EntriesItem [family el] (new EntriesItem el family))

(defn element->MorphItem [el] (new MorphItem el))

(defn element->MacroItem [el] (new MacroItem el))

(defn build-types [types]
  (let [builder (TypesBuilder/builder)]
    (doseq [{:keys [name parents]} types]
      (if-not (nil? parents)
        (.addType builder name parents)
        (.addType builder name)))
    (.build builder)))

(defn build-rules [rules]
  (let [builder (RulesBuilder/builder)]
    (doseq [r rules] (.addRule builder r))
    (.build builder)))

(defn build-default-rules []
  (build-rules [(ForwardApplication.) (BackwardApplication.)]))

(defn lexicon-builder [families morph macros]
  (let [builder (LexiconBuilder/builder)]
    (doseq [f families] (.addFamily builder f))
    (doseq [m morph] (.addMorph builder (element->MorphItem m)))
    (doseq [m macros] (.addMacro builder (element->MacroItem m)))
    builder))

(defn build-grammar [families morph macros]
  (let [types           (build-types [{:name "sem-obj"} {:name "phys-obj" :parents "sem-obj"}])
        rules           (build-default-rules)
        grammar-builder (-> (GrammarBuilder/builder)
                            (.withRules rules)
                            (.withTypes types))
        lexicon-builder (lexicon-builder families morph macros)]

    (.withLexicon grammar-builder (.ref lexicon-builder)) ;; Link Lexicon with Grammar
    (.build lexicon-builder)
    (.build grammar-builder)))

(def max-depth
  (or (utils/str->int (System/getenv "MAX_DEPTH")) 7))

(defn get-tag [^Sign s]
  (.getSupertag (.getCategory s)))

(defn generate
  "Generates multiple of valid sentences from given array of tokens"
  [^Grammar grammar & tokens]
  (combinator/reset-indices)
  (let [signs (mapcat #(utils/str->sign grammar %) tokens)
        combinations (combinator/combinate grammar signs max-depth)
        ;;If we have one token in then we are likely dealing with partial sentences,
        ;;generated for a simple statement plans. Like {{TITLE}}
        ;;FIXME
        sentences (cond
                    (= 1 (count tokens)) (filter utils/partial-sentence? combinations)
                    (and (= 2 (count tokens))
                         (= "adj" (get-tag (first signs)))) (filter #(= "np" (get-tag %)) combinations)
                    :else (filter utils/sentence? combinations))
        ;; sentences combinations
        results (flatten (map (partial realizer/realize-sign grammar) sentences))
        strs (map utils/sign->str results)]
    (log/debugf "Combination count: %d. Sentences count: %d Results: %d"
                (count combinations) (count sentences) (count results))
    (map utils/clean-sentence strs)))
