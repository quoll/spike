(ns quoll.rdf.spike
  "Simple SPARQL utility"
  {:author "Paula Gearon"}
  (:require [babashka.http-client :as http]
            [tiara.data :as t]
            [quoll.rdf :as rdf :refer [iri blank-node lang-literal typed-literal]]
            [clj-yaml.core :as yaml]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.instant :as inst])
  (:import [java.util Properties]
           [java.net URLEncoder URI URL]
           [java.io Writer File]))

(def SPARQL-FILE "The file containing SPARQL defaults" "default")
(def SPARQL-CFG-DIR "The directory containing SPARQL configuration" ".sparql")
(def CREDENTIALS-FILE "The file containing SPARQL credentials" "credentials.yml")
(def SPARQL "The environment variable for the default SPARQL endpoint URL" "SPARQL_URL")

(def sparql-dir (str (System/getProperty "user.home") File/separator SPARQL-CFG-DIR))
(def sparql-config-file (str sparql-dir File/separator SPARQL-FILE))
(def credentials-file (str sparql-dir File/separator CREDENTIALS-FILE))

(defn default-service
  []
  (or (System/getenv SPARQL)
      (let [sparql-config (io/file sparql-config-file)
            props (and (.exists sparql-config)
                       (with-open [r (io/reader sparql-config)]
                         (into {} (doto (Properties.) (.load r)))))
            cfg (and props (reduce (fn [c [k v]]
                                     (case (s/lower-case k)
                                       ("endpoint" "sparql" "url") (assoc c :url v)))
                                   {} props))]
        (:url cfg))))

(defn load-credentials
  [path]
  (let [credentials-file (io/file path)]
    (when (.exists credentials-file)
      (with-open [r (io/reader credentials-file)]
        (yaml/parse-stream r {:key-fn :key})))))

(def ^:dynamic *service* (default-service))

(def credentials (load-credentials credentials-file))

(defn context-iri
  "Returns an IRI function that takes a string, and calls a 3-argument version of the iri-fn that
   accepts the original IRI, the detected namespace, and the local value. If no namespace is detected,
   then a 1 argument version will be called."
  [context iri-fn]
  (fn [u]
    (if-let [[p nms] (first (filter (fn [[k v]] (s/starts-with? u v)) context))]
      (let [prefix (name p)]
        (iri-fn u prefix (subs u (count nms))))
      (iri-fn u))))

(defn uri [u] (URI. u))
(defn url [u] (URL. u))

(defn rdf-resource
  ([term] (rdf-resource iri term))
  ([iri-fn {:keys [type value] :as term}]
   (case type
     "uri" (iri-fn value)
     "bnode" (blank-node value)
     "literal" (let [{lang :xml:lang datatype :datatype} term]
                 (cond
                   lang (lang-literal value lang)
                   datatype (letfn [(dt? [x] (= datatype (rdf/as-str x)))]
                              (cond
                                (dt? rdf/XSD-STRING) value
                                (dt? rdf/XSD-INTEGER) (parse-long value)
                                (dt? rdf/XSD-FLOAT) (parse-double value)
                                (dt? rdf/XSD-DATETIME) (inst/read-instant-date value)
                                (dt? rdf/XSD-BOOLEAN) (parse-boolean value)
                                (dt? rdf/XSD-QNAME) (apply keyword (s/split value #":" 2))
                                (dt? rdf/XSD-ANYURI) (URI. value)
                                :default (typed-literal value (iri datatype))))
                   :default value))
     (throw (ex-info "Unknown datatype" {:term term :type type :value value})))))

(defn decode-json-results
  "Decodes JSON results from a SPARQL query."
  [{{:keys [vars link] :as head} :head {:keys [bindings] :as results} :results} iri-fn]
  (when-let [short-head (seq (dissoc head :vars :link))]
    (println "Extra headers: " (map first short-head)))
  (when-let [short-results (seq (dissoc results :bindings))]
    (println "Extra results " (map first short-results)))
  (let [cols (mapv keyword vars)
        header-data (cond-> {:cols cols}
                      link (assoc :link link))]
    (with-meta
      (mapv (fn [bndg]
              (persistent! (reduce (fn [m c] (assoc! m c (rdf-resource iri-fn (get bndg c))))
                                   (transient t/EMPTY_MAP)
                                   cols)))
            bindings)
       header-data)))

(defn get-auth*
  "Returns the authorization header for a service, if credentials are available."
  [service credentials]
  (let [domains (keys credentials)]
    (when-let [domain (first (filter #(s/starts-with? service %) domains))]
      (when-let [{:strs [username password]} (get credentials domain)]
        (when (and username password)
          {:basic-auth [username password]})))))

(def get-auth (memoize get-auth*))

(defn- do-query
  [service q]
  (let [url-with-params (str service "?query=" (URLEncoder/encode q))
        auth (get-auth service credentials)]
     (-> {:method :get
                        :uri url-with-params
          :headers {"Accept" "application/sparql-results+json"}}
         (merge auth)
         http/request 
         :body
         (json/read-str :key-fn keyword))))

(defn keyword3
  "A helper function for keyword being called with 3 args for a curie.
   Single arguments will return the argument (a string)"
  ([s] s)
  ([_ nms n] (keyword nms n)))

(defn string3
  "A helper function for str being called with 3 args for a curie."
  ([s] s)
  ([_ nms n] (str nms \: n)))

(defn process-args
  "Query args are flexible, with the following options:
   service: Optional. The URL of the SPARQL service. This is first to allow for easy partial binding.
   q: The query to execute.
   named vars: Optional.
      :iri-fn The function to construct IRI values. Defaults to quoll.rdf/iri
      :context The context for IRI creation. Only useful when the iri-fn can accept both 1 and 3 arguments.
               A keyword map of prefixes or :default."
  [[f & r :as args]]
  (let [[service [q & {:keys [iri-fn context]
                       :or {iri-fn iri}}]] (if (s/starts-with? f "http")
                                             [f r]
                                             [*service* args])
        context (if (= :default context) rdf/common-prefixes context)
        iri-fn (if (fn? iri-fn)
                 (let [f (cond
                           (= keyword iri-fn) keyword3
                           (= str iri-fn) string3
                           :default iri-fn)]
                   (if context (context-iri context f) iri-fn))
                 (case iri-fn
                   (:uri "uri") uri
                   (:url "url") url
                   (:iri "iri") (if context (context-iri context iri) iri)
                   (:qname :curie "qname" "curie") (if context (context-iri context iri)
                                                       (throw (ex-info "CURIEs cannot be created without a context")))
                   (:str :string "str" "string") (if context (context-iri context string3) identity)
                   (:keyword "keyword") (if context (context-iri context keyword3)
                                            (throw (ex-info "CURIEs cannot be created without a context")))
                   (throw (ex-info (str "Unsupported IRI function value: " iri-fn) {:iri-fn iri-fn}))))]
    [service q iri-fn]))

(defn query
  "Issues a SPARQL query. See process-args for the argument values"
  [& args]
  (let [[service q iri-fn] (process-args args)]
    (decode-json-results (do-query service q) iri-fn)))

(defn query-table
  "Issues a SPARQL query, returning a seq of vectors. See process-args for the argument values"
  ([& args]
   (let [results (apply query args)]
     (map #(vec (vals %)) results))))

