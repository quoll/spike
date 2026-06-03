(ns quoll.rdf.data
  "Data management for SPARQL protocols"
  {:author "Paula Gearon"}
  (:require [tiara.data :as t]
            [quoll.rdf :as rdf :refer [iri blank-node lang-literal typed-literal]]
            [quoll.raphael.core :as ttl]
            [clojure.string :as s]
            [clojure.xml :as xml]
            [clojure.data.json :as json]
            [clojure.data.csv :as csv]
            [clojure.instant :as inst]
            [clojure.tools.logging :as log])
  (:import [java.net URI]
           [java.io StringReader]
           [org.xml.sax InputSource]))

(defn rdf-resource
  "Converts a SPARQL result object into an appropriate RDF resource object"
  ([term] (rdf-resource rdf/iri term))
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
                                :else (typed-literal value (iri datatype))))
                   :else value))
     (throw (ex-info "Unknown datatype" {:term term :type type :value value})))))

(defn unpack-json-results
  "Decodes JSON results from a SPARQL query."
  [{{:keys [vars link] :as head} :head {:keys [bindings] :as results} :results :as response} iri-fn]
  (if-let [[_ value] (find response :boolean)]
    (do
      (when results
        (println "Unexpected results with boolean response:" results))
      value)
    (do
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
          header-data)))))

(defn new-generator
  "Wraps a TTL node generator, reimplementing only the new-iri function."
  [iri-fn]
  (let [generator (ttl/new-generator)]
    (reify ttl/NodeGenerator
      (new-iri [_ iri] (iri-fn iri))
      (new-node [_] (ttl/new-node generator))
      (new-node [_ label] (ttl/new-node generator label))
      (add-base [_ iri] (ttl/add-base generator iri))
      (add-prefix [_ prefix iri] (ttl/add-prefix generator prefix iri))
      (iri-for [_ prefix] (ttl/iri-for generator prefix))
      (get-namespaces [_] (ttl/get-namespaces generator))
      (get-base [_] (ttl/get-base generator))
      (new-qname [_ prefix local] (ttl/new-qname generator prefix local))
      (new-literal [_ s] (ttl/new-literal generator s))
      (new-literal [_ s t] (ttl/new-literal generator s t))
      (new-lang-string [_ s l] (ttl/new-lang-string generator s l))
      (rdf-type [_] (ttl/rdf-type generator))
      (rdf-first [_] (ttl/rdf-first generator))
      (rdf-rest [_] (ttl/rdf-rest generator))
      (rdf-nil [_] (ttl/rdf-nil generator)))))

(defn ttl-parser
  [iri-fn]
  (if iri-fn
    #(ttl/parse % (new-generator iri-fn))
    ttl/parse))

(defn json-result-parser
  [iri-fn]
  (fn [text]
    (-> text
        (json/read-str :key-fn keyword)
        (unpack-json-results iri-fn))))

(defn csv-parser [_] csv/read-csv)

(defn tsv-parser [_] #(csv/read-csv % :separator \tab))

(defn xml-result-parser
  [iri-fn]
  (fn [text]
    (let [input (InputSource. (StringReader. text))
          {:keys [tag content] :as doc} (xml/parse input)]
      (when-not (= :sparql tag)
        (throw (ex-info (str "Malformed SPARQL XML result. Root tag should be <sparql> but is​ <"
                             (name tag) ">") doc)))
      (let [[{:keys [tag content]} {rtag :tag :as results}] content]
        (when-not (= :head tag)
          (throw (ex-info "Malformed SPARQL XML result​. Missing <head> section." doc)))
        (if (= rtag :boolean)
          (-> results :content first parse-boolean)
          (do
            (when-not (= :results rtag)
              (throw (ex-info "Malformed SPARQL XML result. No <results> nor <boolean> section." doc)))
            (let [rvars (into [] (comp (filter #(= (:tag %) :variable))
                                       (map (comp keyword :name :attrs))) content)
                  known-vars (set (map name rvars))
                  link (->> content (filter #(= (:tag %) :link)) first :attrs :href)
                  header (cond-> {:variables rvars}
                           link (assoc :link (URI. link)))
                  bindings-list (mapv (fn [{bindings :content}]
                                        (into {}
                                              (map (fn [{{var :name} :attrs btag :tag [data] :content :as binding}]
                                                     (when-not (= :binding btag)
                                                       (log/warn "Unexpected entry in bindings" binding))
                                                     (when-not (known-vars var)
                                                       (log/warn (str "Binding for unknown variable: " var)
                                                                 binding))
                                                     (let [{ntype :tag
                                                            {:keys [xml:lang datatype]} :attrs
                                                            [value] :content} data
                                                           term (cond-> {:type (name ntype)
                                                                         :value value}
                                                                  xml:lang (assoc :xml:lang xml:lang)
                                                                  datatype (assoc :datatype datatype))]
                                                       [var (rdf-resource iri-fn term)])))
                                              bindings))
                                      (:content results))]
              (with-meta bindings-list header))))))))
