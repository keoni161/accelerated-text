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
       (group-by ::morph-spec/pos)
       (remove (fn [[pos _]] (get #{:ADJ :V} pos)))
       (mapcat (fn [[_ items]] items))
       (map-indexed
        (fn [idx {word ::morph-spec/word pos ::morph-spec/pos}]
          (dsl/family
           (format "%s-%s" "base" (name pos))
           pos true
           (dsl/entry "primary"
                      (dsl/lf (str "X" idx) (dsl/prop "[*DEFAULT*]"))
                      (dsl/atomcat pos {:index 1} (dsl/fs-nomvar "index" "X")))
           (dsl/member word))))))

(defn event-family []
  (dsl/family
    "Event" :V false
    (dsl/entry
      "Primary"
      (dsl/lf "E"
              (dsl/prop "[*DEFAULT*]")
              (dsl/diamond "ARG0" {:nomvar "X0" :prop (dsl/prop "[*DEFAULT*]")})
              (dsl/diamond "ARG1" {:nomvar "X1" :prop (dsl/prop "[*DEFAULT*]")}))
      (dsl/>F
        \>
        (dsl/<B (dsl/atomcat :S {} (dsl/fs-nomvar "index" "E"))
                (dsl/atomcat :NP {} (dsl/fs-nomvar "index" "X0")))
        (dsl/atomcat :NP {} (dsl/fs-nomvar "index" "X1"))))))
