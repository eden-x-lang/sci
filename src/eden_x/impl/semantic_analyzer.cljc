(ns eden-x.impl.semantic-analyzer
  (:require [clojure.walk :refer [postwalk]]
            [clojure.string :as s]))

(defn ^:private homogenize-entry [entry]
  (let [function? (fn? entry)
        {:eden-x/keys [semantic-analysis]} (meta entry)]
    (if function?
      (or semantic-analysis (-> entry .getClass str (s/split #"\$") last symbol))
      entry)))

(defn ^:private homogenize-symbol [sym fixed-args var-arg-name]
  (if (= var-arg-name sym)
    '$args
    (let [indexed-map (reduce-kv (fn [m i n] (assoc m n (symbol (str "$" i))))
                                 {} (vec fixed-args))
          param (get indexed-map sym)]
      (or param sym))))

#_(defn ^:private match-fn-signature [form ])

(defn ^:private entry-homogenizer [fixed-args var-arg-name]
  (fn [entry]
    (cond
      (fn? entry)
      (let [{:eden-x/keys [semantic-analysis]} (meta entry)]
        (or semantic-analysis
            (-> entry .getClass str (s/split #"\$") last symbol)))

      (symbol? entry)
      (homogenize-symbol entry fixed-args var-arg-name)
      
      :else
      entry)))

(defn ^:private homogenize-forms [entry]
  (if (and (vector? entry)
           (fn? (first entry)))
    (seq entry)
    entry))

(defn ^:private homogenize-body
  [{:sci.impl/keys [body fixed-args fixed-arity var-arg-name] :as f}]
  (->> body first (drop 2) vec
       (postwalk homogenize-forms)
       (postwalk (entry-homogenizer fixed-args var-arg-name))))

(defn ^:private homogenize-arity
  [{:sci.impl/keys [fixed-arity] :as f}]
  fixed-arity)

(defn analyze [{:sci.impl/keys [fn-bodies]}]
  (mapv (fn [{:sci.impl/keys [var-arg-name] :as f}]
          {:eden-x/body (homogenize-body f)
           :eden-x/fixed-arity (homogenize-arity f)
           :eden-x/variadic? (-> var-arg-name nil? not)})
        fn-bodies))
