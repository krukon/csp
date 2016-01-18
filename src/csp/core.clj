(ns csp.core
  (:gen-class))

(defn arc-reduce
  [sln f u v]
  (let [u (get-in sln [:values u])]
    (update-in sln
               [:domains v]
               (partial filter (partial f u)))))

(defn ac-3-step
  [sln [f u v]]
  (let [has-u (contains? (:values sln) u)
        has-v (contains? (:values sln) v)]
    (cond
      (and has-u has-v) sln
      has-u (arc-reduce sln f u v)
      has-v (arc-reduce sln f v u)
      :else sln)))

(defn ac-3
  [sln]
  (loop [sln sln
         constraints (:constraints sln)]
    (if (empty? constraints)
      sln
      (recur (ac-3-step sln (first constraints))
             (rest constraints)))))

(defn check-domains
  [{domains :domains
    :as sln}]
  (if (->> domains
           (map second)
           (filter empty?)
           empty?)
      sln
      nil))

(defn backtrack
  [sln variable-fn value-fn]
  (if (empty? (:domains sln))
    (:values sln)
    (let [[v domain] (variable-fn (:domains sln))
          sub-sln (update-in sln [:domains] dissoc v)]
      (loop [values domain]
        (if (empty? values)
          nil
          (let [value (value-fn values)
                sub-sln (-> sub-sln
                            (assoc-in [:values v] value)
                            check-domains
                            ac-3
                            (backtrack variable-fn value-fn))]
            (or sub-sln
                (recur (remove #{value} values)))))))))

;;; TEST AUSTRALIA

(def colors
  [:red :green :blue])

(def australia-csp
  {:assigned {}
   :domains {:wa colors
             :nt colors
             :sa colors
             :q colors
             :v colors
             :nsw colors
             :t colors}
   :constraints [[not= :wa :nt]
                 [not= :wa :sa]
                 [not= :nt :sa]
                 [not= :nt :q]
                 [not= :sa :q]
                 [not= :sa :nsw]
                 [not= :sa :v]
                 [not= :q :nsw]
                 [not= :nsw :v]]})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (backtrack australia-csp last first)))
