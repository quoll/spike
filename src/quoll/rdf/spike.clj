(ns quoll.rdf.spike
  "Simple SPARQL utility"
  {:author "Paula Gearon"}
  (:require [babashka.http-client :as http]
            [tiara.data :as t]
            [quoll.rdf :as rdf :refer [iri blank-node lang-literal typed-literal]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.instant :as inst])
  (:import [java.util Properties]
           [java.net URLEncoder URI]
           [java.io Writer]))

(def SPARQL-FILE "The file containing SPARQL defaults" ".sparql")
(def SPARQL "The environment variable for the default SPARQL endpoint URL" "SPARQL_URL")

(defn default-service
  []
  (or (System/getenv SPARQL)
      (let [home-dir (System/getProperty "user.home")
            sparql-config (io/file home-dir SPARQL-FILE)
            props (and (.exists sparql-config)
                       (with-open [r (io/reader sparql-config)]
                         (into {} (doto (Properties.) (.load r)))))
            cfg (reduce (fn [c [k v]]
                          (case (s/lower-case k)
                            ("endpoint" "sparql" "url") (assoc c :url v)))
                        {} props)]
        (:url cfg))))

(def ^:dynamic *service* (default-service))

(defn rdf-resource
  [{:keys [type value] :as term}]
  (case type
    "uri" (iri value) ;; TODO: add prefix detection and string option
    "bnode" (blank-node value)
    "literal" (let [{lang :xml:lang datatype :datatype} term]
                (cond
                  lang (lang-literal value lang)
                  datatype (letfn [(dt? [x] (= datatype (rdf/as-str x)))]
                             (cond
                               (dt? rdf/XSD-STRING) value
                               (dt? rdf/XSD-INTEGER) (parse-long value)
                               (dt? rdf/XSD-FLOAT) (parse-double value)
                               (dt? rdf/XSD-DATE) (inst/read-instant-date value)
                               (dt? rdf/XSD-BOOLEAN) (parse-boolean value)
                               (dt? rdf/XSD-QNAME) (apply keyword (s/split value #":" 2))
                               (dt? rdf/XSD-ANYURI) (URI. value)
                               :default (typed-literal value (iri datatype))))
                  :default value))
    (throw (ex-info "Unknown datatype" {:term term :type type :value value}))))

(defn decode-json-results
  "Decodes JSON results from a SPARQL query."
  [{{:keys [vars link] :as head} :head {:keys [bindings] :as results} :results}]
  (when-let [short-head (seq (dissoc head :vars :link))]
    (println "Extra headers: " (map first short-head)))
  (when-let [short-results (seq (dissoc results :bindings))]
    (println "Extra results " (map first short-results)))
  (let [cols (mapv keyword vars)
        header-data (cond-> {:cols cols}
                      link (assoc :link link))]
    (with-meta
      (mapv (fn [bndg]
              (persistent! (reduce (fn [m c] (assoc! m c (rdf-resource (get bndg c))))
                                   (transient t/EMPTY_MAP)
                                   cols)))
            bindings)
       header-data)))

(defn- do-query
  [service q]
  (let [url-with-params (str service "?query=" (URLEncoder/encode q))]
     (-> (http/request {:method :get
                        :uri url-with-params
                        :headers {"Accept" "application/sparql-results+json"}})
         :body
         (json/read-str :key-fn keyword))))

(defn query
  "Issues a SPARQL query"
  ([q] (query *service* q))
  ([service q]
   (decode-json-results (do-query service q))))

(defn query-table
  "Issues a SPARQL query, returning a seq of vectors"
  ([q] (query-table *service* q))
  ([service q]
   (let [results (query service q)]
     (map #(vec (vals %)) results))))

