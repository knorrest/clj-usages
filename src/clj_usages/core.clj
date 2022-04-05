(ns clj-usages.core
  (:require [clj-kondo.core :as clj-kondo]
            [clojure.string :as str]
            [loom.graph :as graph]
            [loom.io :as io]
            [loom.label :as label]))

(defn get-analysis [paths]
  (:analysis (clj-kondo/run! {:lint paths
                              :config {:output {:analysis true}}})))

(defn- shorten-ns [s]
  (let [[ns var] (str/split s #"/")
        ns-parts (str/split ns #"\.")]
    (str (str/join "." (map first (drop-last ns-parts)))
         (format ".%s" (last ns-parts))
         (when var (format "/%s" var)))))

(defn- include-ns? [ns-includes ns-excludes ns]
  (and (or (empty? ns-includes) (some #(str/starts-with? ns %) ns-includes))
       (not-any? #(str/starts-with? ns %) ns-excludes)))

(defn- include-usage? [ns-includes ns-excludes usage]
  (and (include-ns? ns-includes ns-excludes (str (:from usage)))
       (include-ns? ns-includes ns-excludes (str (:to usage)))))

(defn- update-uses [uses used-by usage-map]
  (if (contains? usage-map uses)
    (update-in usage-map
               [uses :used-by]
               (fn [s] (conj s used-by)))
    (assoc usage-map
           uses
           {:uses #{}
            :used-by #{used-by}})))

(defn- update-used-by [uses used-by usage-map]
  (if (contains? usage-map used-by)
    (update-in usage-map
               [used-by :uses]
               (fn [s] (conj s uses)))
    (assoc usage-map
           used-by
           {:uses #{uses}
            :used-by #{}})))
                       
(defn get-usages* [usages ns-includes ns-excludes usage->from usage->to]
  (->> usages
       (filter (partial include-usage? ns-includes ns-excludes))
       (reduce (fn [acc usage]
                 (let [uses   (usage->to usage)
                       used-by (usage->from usage)]
                   (->> acc
                        (update-uses uses used-by)
                        (update-used-by uses used-by))))
               {})))

(defn- var-usage->from [usage]
  (str (:from usage) "/" (:from-var usage)))

(defn- var-usage->to [usage]
  (str (:to usage) "/" (:name usage)))

(defn get-var-usages [analysis ns-includes ns-excludes]
  (get-usages* (:var-usages analysis) ns-includes ns-excludes var-usage->from var-usage->to))

(defn- ns-usage->from [usage]
  (str (:from usage)))

(defn- ns-usage->to [usage]
  (str (:to usage)))

(defn- get-ns-usages [analysis ns-includes ns-excludes]
  (get-usages* (:namespace-usages analysis) ns-includes ns-excludes ns-usage->from ns-usage->to))

(defn- get-uses-edges [usage-map k]
  (let [uses (:uses (get usage-map k))]
    (map (fn [v] [k v]) uses)))

(defn- get-all-uses-edges [usage-map max-depth start-node]
  (loop [current-depth 0
         current-keys  #{start-node}
         acc           '()]
    (if (>= current-depth max-depth)
      acc
      (let [all-uses (->> current-keys
                                (map (fn [k] (get-uses-edges usage-map k)))
                                (apply concat))
            next-keys (map second all-uses)]
        (recur (inc current-depth)
               next-keys
               (concat acc all-uses))))))

(defn- get-used-by-edges [usage-map k]
  (let [used-by (:used-by (get usage-map k))]
    (map (fn [v] [v k]) used-by)))

(defn- get-all-used-by-edges [usage-map max-depth start-node]
  (loop [current-depth 0
         current-keys  #{start-node}
         acc           '()]
    (if (>= current-depth max-depth)
      acc
      (let [all-uses (->> current-keys
                                (map (fn [k] (get-used-by-edges usage-map k)))
                                (apply concat))
            next-keys (map first all-uses)]
        (recur (inc current-depth)
               next-keys
               (concat acc all-uses))))))

(defn- create-digraph
  [start-node uses-depth used-by-depth usages]
  (let [reference-edges (get-all-uses-edges usages
                                            uses-depth
                                            start-node)
        used-by-edges   (get-all-used-by-edges usages
                                               used-by-depth
                                               start-node)
        all-edges       (concat reference-edges used-by-edges)
        all-nodes       (mapcat identity all-edges)
        labels          (map shorten-ns all-nodes)]
    (-> (graph/digraph)
        (#(apply label/add-labeled-nodes % (interleave all-nodes labels)))
        (#(apply graph/add-edges % all-edges)))))

(defn var-usages-graph
  [analysis var uses-depth used-by-depth ns-includes ns-excludes]
  (let [usages (get-var-usages analysis ns-includes ns-excludes)]
  (create-digraph var uses-depth used-by-depth usages)))

(defn ns-usages-graph
  [analysis ns uses-depth used-by-depth ns-includes ns-excludes]
  (let [usages (get-ns-usages analysis ns-includes ns-excludes)]
  (create-digraph ns uses-depth used-by-depth usages)))

(defn view [graph]
  (io/view graph))

