(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.tools.deps :as t]))

(def lib 'org.clojars.quoll/spike)
(def version (format "0.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; boilerplate - see https://clojure-doc.org/articles/cookbooks/cli_build_projects/
;; clojure -T:build run :aliases '[:test]'
(defn run [{:keys [aliases] :as opts}]
  (let [basis      (b/create-basis opts)
        alias-data (t/combine-aliases basis aliases)
        cmd-opts   (merge {:basis     basis
                           :main      'clojure.main
                           :main-args ["-e" "(clojure-version)"]}
                          opts
                          alias-data)
        cmd        (b/java-command cmd-opts)]
    (when-not (zero? (:exit (b/process cmd)))
      (throw (ex-info (str "run failed for " aliases) opts)))
    opts))

;; clojure -T:build run-tests
(defn run-tests "Run the tests." [opts]
  (run (update opts :aliases conj :test)))

;; clojure -T:build ci
(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (run-tests)
      (b/clean)
      (b/jar)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

