(ns quoll.spike
  "Simple SPARQL utility"
  {:author "Paula Gearon"}
  (:require [babashka.http-client :as http]
            [tiara.data :as t]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [java.util Properties]
           [java.net URLEncoder URI]
           [java.io Writer]))

(def SPARQL-FILE "The file containing SPARQL defaults" ".sparql")
(def SPARQL "The environment variable for the default SPARQL endpoint URL" "SPARQL_URL")

(defrecord IRIRef [iri prefix local]
  Object
  (toString [this]
    (if local
      (if prefix (str prefix \: local) (str \: local))
      (str \< iri \>))))

(defn iri
  ([i] (if (instance? IRIRef i) i (->IRIRef i nil nil)))
  ([i p l] (->IRIRef i p l)))

(def RDF-LANG-STRING-STR "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")
(def XSD-STRING-STR "http://www.w3.org/2001/XMLSchema#string")

(def RDF-LANG-STRING (->IRIRef RDF-LANG-STRING-STR "rdf" "langString"))
(def XSD-STRING (->IRIRef XSD-STRING-STR "xsd" "string"))

(defn as-str
  [u]
  (if (instance? IRIRef u)
    (:iri u)
    (str u)))

(defrecord Literal [value datatype lang]
  Object
  (toString [this]
    (cond
      lang (str \" value "\"@" lang)
      (and datatype (not= (as-str datatype) XSD-STRING-STR)) (str \" value "\"^^" datatype)
      :default value)))

(defmethod clojure.core/print-method quoll.spike.IRIRef [o, ^Writer w]
  (.write w (str o)))

(defmethod clojure.core/print-method quoll.spike.Literal [o, ^Writer w]
  (.write w (str o)))

(defn lang-literal
  [value lang]
  (->Literal value RDF-LANG-STRING lang))

(defn typed-literal
  [value datatype]
  (->Literal value datatype nil))

(defrecord BlankNode [id]
  Object
  (toString [this]
    (str "_:" id)))

(defn blank-node
  [id]
  (->BlankNode (if (s/starts-with? id "_:") (subs id 2) id)))

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
    "uri" (iri value)
    "bnode" (blank-node value)
    "literal" (let [{lang :xml:lang datatype :datatype} term]
                (cond
                  lang (->Literal value lang nil)
                  (and datatype (not= (as-str datatype) XSD-STRING-STR)) (->Literal value nil (iri datatype))
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

(defn query
  "Issues a SPARQL query"
  ([q] (query *service* q))
  ([service q]
   (let [url-with-params (str service "?query=" (URLEncoder/encode q))]
     (-> (http/request {:method :get
                        :uri url-with-params
                        :headers {"Accept" "application/sparql-results+json"}})
         :body
         (json/read-str :key-fn keyword)
         decode-json-results))))

