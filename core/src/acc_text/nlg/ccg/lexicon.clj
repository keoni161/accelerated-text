(ns acc-text.nlg.ccg.lexicon
  (:require [acc-text.nlg.dsl.core :as dsl]
            [acc-text.nlg.spec.morphology :as morph-spec]))

(defn modifier-family [index]
  (dsl/family "Modifier"
              :ADJ false
              (dsl/entry
               "Primary"
               (dsl/lf "X"
                       (dsl/prop "[*DEFAULT*]")
                       (dsl/diamond "Mod" {:nomvar "M" :prop (dsl/prop "[*DEFAULT*]")}))
               (dsl/>F
                \^
                (dsl/atomcat :NP {:inherits-from index} (dsl/fs-nomvar "mod-index" "M"))
                (dsl/atomcat :NP {:index index} (dsl/fs-nomvar "index" "X"))))))

(defn base-families [morphology]
  (->> morphology
       (remove (fn [{pos ::morph-spec/pos}] (= :ADJ pos)))
       (map
        (fn [{word ::morph-spec/word pos ::morph-spec/pos class ::morph-spec/class}]
          (dsl/family
           (format "%s-%s" (name class) (name pos))
           pos true
           (dsl/entry "primary"
                      (dsl/lf word (dsl/prop "[*DEFAULT*]"))
                      (dsl/atomcat pos {:index 1} (dsl/fs-nomvar "index" "X")))
           (dsl/member word))))))

(defn event-family []
  (dsl/family "Event"
              :V false
              (dsl/entry
               "Primary"
               (dsl/lf "E"
                       (dsl/prop "[*DEFAULT*]")
                       (dsl/diamond "ARG0" {:nomvar "X" :prop (dsl/prop "[*DEFAULT*]")})
                       (dsl/diamond "ARG1" {:nomvar "Y" :prop (dsl/prop "[*DEFAULT*]")}))
               (dsl/>F
                \>
                (dsl/<B
                 (dsl/atomcat :S {} (dsl/fs-nomvar "index" "E"))
                 (dsl/atomcat :NP {} (dsl/fs-nomvar "index" "X")))
                (dsl/atomcat :NP {} (dsl/fs-nomvar "index" "Y"))))))
