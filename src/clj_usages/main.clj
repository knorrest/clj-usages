(ns clj-usages.main
  (:require [clj-usages.core :as core]
            [clojure.string :as str]
            [clojure.tools.cli :as cli])
  (:gen-class))

(def cli-options
  [[nil "--source-paths PATHS" "Comma-separated list of paths to src dirs"
    :default ["src"]
    :parse-fn #(str/split % #",")]
   [nil "--var NODE" "Namespace-qualified var to center the graph on."]
   [nil "--ns NODE" "Namespace to center the graph on."]
   [nil "--ns-includes PATHS" "Comma-separated list of namespaces to include (all if not set)"
    :default []
    :parse-fn #(str/split % #",")]
   [nil "--ns-excludes PATHS" "Comma-separated list of namespaces to exclude (none if not set)"
    :default ["clojure"]
    :parse-fn #(str/split % #",")]
   [nil "--uses-depth DEPTH" "Depth of graph downwards from the starting node"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [nat-int? "Must be >= 0"]]
   [nil "--used-by-depth DEPTH" "Depth of graph upwards from the staring node"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [nat-int? "Must be >= 0"]]
   ["-h" "--help"]])

(defn- usage [options-summary]
  (->> ["Generates a var or ns usage grap centered on a given var or ns"
        ""
        "Usage: clj-usage-graph [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn- exit [status msg]
  (println msg)
  (System/exit status))

(defn- validate-args [args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      (exit 0 (usage summary))

      errors
      (exit 1 (str/join \newline errors))

      :else
      options)))

(defn -main [& args]
  (let [options (validate-args args)
        {:keys [var
                ns
                source-paths
                uses-depth
                used-by-depth
                ns-includes
                ns-excludes]} options
        analysis (core/get-analysis source-paths)
        graph  (if var
                 (core/var-usages-graph analysis var uses-depth used-by-depth ns-includes ns-excludes)
                 (core/ns-usages-graph analysis ns uses-depth used-by-depth ns-includes ns-excludes))]
    (core/view graph)))