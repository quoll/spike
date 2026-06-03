(ns quoll.rdf.spike
  "Simple SPARQL utility"
  {:author "Paula Gearon"}
  (:require [quoll.rdf.data :as data]
            [babashka.http-client :as http]
            [quoll.rdf :as rdf :refer [iri]]
            [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [java.util Properties]
           [java.net URLEncoder URI URL]
           [java.nio.charset Charset StandardCharsets]
           [java.io File]))

(def SPARQL-FILE "The file containing SPARQL defaults" "default")
(def SPARQL-CFG-DIR "The directory containing SPARQL configuration" ".sparql")
(def CREDENTIALS-FILE "The file containing SPARQL credentials" "credentials.yml")
(def SPARQL "The environment variable for the default SPARQL endpoint URL" "SPARQL_URL")

(def json-result "application/sparql-results+json")
(def xml-result "application/sparql-results+xml")
(def csv-result "text/csv")
(def tsv-result "text/tsv")
(def ttl-result "text/turtle")
(def rdfxml-result "application/rdf+xml")

(def sparql-dir (str (System/getProperty "user.home") File/separator SPARQL-CFG-DIR))
(def sparql-config-file (str sparql-dir File/separator SPARQL-FILE))
(def credentials-file (str sparql-dir File/separator CREDENTIALS-FILE))

(defn url-encode
  [v]
  (URLEncoder/encode v ^Charset StandardCharsets/UTF_8))

(defn default-service
  "Returns the service endpoint to use when none have been specified. This allows users to set up
  a default for easy access. Uses the `SPARQL` environment variable, if set, and otherwise falls
  back on a properties file with the path ~/.sparql/default"
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
  "When the provided filename exists, then reads it as a YAML file."
  [path]
  (let [credentials-file (io/file path)]
    (when (.exists credentials-file)
      (with-open [r (io/reader credentials-file)]
        (yaml/parse-stream r {:key-fn :key})))))

(def ^:dynamic *service* "The default service, when configured" (default-service))

(def credentials (load-credentials credentials-file))

(defn context-iri
  "Returns an IRI function that takes a string, and calls a 3-argument version of the iri-fn that
   accepts the original IRI, the detected namespace, and the local value. If no namespace is detected,
   then a 1 argument version will be called."
  [context iri-fn]
  (fn [u]
    (if-let [[p nms] (first (filter (fn [[_ v]] (s/starts-with? u v)) context))]
      (let [prefix (name p)]
        (iri-fn u prefix (subs u (count nms))))
      (iri-fn u))))

(defn uri [u] (URI. u))
(defn url [u] (URL. u))

(defn get-auth*
  "Returns the authorization header for a service, if credentials are available."
  [service credentials]
  (let [domains (keys credentials)]
    (when-let [domain (first (filter #(s/starts-with? service %) domains))]
      (when-let [{:strs [username password]} (get credentials domain)]
        (when (and username password)
          {:basic-auth [username password]})))))

(def get-auth (memoize get-auth*))

(defn param-str
  "Converts a map into a set of URL encoded parameters"
  [params]
  (->> params
       (map (fn [[k v]] (str (name k) "=" (url-encode (str v)))))
       (interpose "&")
       (apply str)))

(defn normalize-headers
  "Ensure headers are consistent, and already set for use in http calls"
  [header]
  (into {} (map (fn [[k v]] [(s/capitalize (name k)) v])) header))

(def response-handlers
  {json-result data/json-result-parser
   ttl-result data/ttl-parser
   csv-result data/csv-parser
   tsv-result data/tsv-parser
   xml-result data/xml-result-parser
   rdfxml-result (fn [_] (fn [_] (throw (ex-info "No, I don't want to load Jena" {:reason "no"}))))})

(defn response-handler
  "Processes an HTTP response, throwing an exception of a non 2xx result"
  ([response] (response-handler response identity))
  ([{:keys [status body headers] :as response} iri-fn]
   (if (and (>= status 200) (< status 300))
     (let [content-type (get headers "Content-type")
           handler (if-let [h (response-handlers content-type)]
                     (h iri-fn)
                     identity)]
       (handler body))
     (throw (ex-info "Unexpected response from server" response)))))

(defn- do-query
  "Executes a SPARQL query as an HTTP GET request, sending the query as a URL-encoded parameter.
  Optionally accepts a additional argument map. A `:headers` map will be merged with the headers.
  A `:params` map will be appended as URL-encoded arguments in the URL. All other elements of the
  map will be merged with the structure sent to the `http/request` function."
  [service q iri-fn & [{:keys [params headers] :as args}]]
  (let [url-with-params (cond-> (str service (if (s/includes? service "?") \& \?) "query=" (url-encode q))
                          params (str \& (param-str params)))
        auth (get-auth service credentials)
        request-header (merge {"Accept" json-result} (normalize-headers headers))]
    (-> {:method :get
         :uri url-with-params
         :headers request-header}
        (merge auth)
        (merge (dissoc args :params :headers))
        http/request
        (response-handler iri-fn))))

(defn- do-update
  "Executes a SPARQL UPDATE as an HTTP POST request, sending the update statement as the request
  body with content type `application/sparql-update`. Optionally accepts an additional argument map.
  A `:headers` map will be merged with the headers. A `:params` map will be appended as URL-encoded
  arguments in the URL. All other elements of the map will be merged with the structure sent to the
  `http/request` function. Returns the HTTP result."
  [service q & [{:keys [params headers] :as args}]]
  (let [url-with-params (cond-> service
                          params (str (if (s/includes? service "?") \& \?) (param-str params)))
        auth (get-auth service credentials)]
    (-> {:method :post
         :uri url-with-params
         :headers (merge {"Content-Type" "application/sparql-update"} headers)
         :body q
         :throw false}
        (merge auth)
        (merge (dissoc args :params :headers))
        http/request)))

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
  "args are flexible, with the following options:
   service: Optional. The URL of the SPARQL service. This is first to allow for easy partial binding.
   q: The query to execute.
   named vars: Optional.
      :iri-fn The function to construct IRI values. Defaults to quoll.rdf/iri
      :context The context for IRI creation. Only useful when the iri-fn can accept both 1 and 3 arguments.
               A keyword map of prefixes or :default."
  [[f & r :as args]]
  (let [[service [q & arg-pairs]] (if (s/starts-with? f "http")
                                    [f r]
                                    [*service* args])
        {:keys [iri-fn context] :or {iri-fn rdf/iri} :as arg-map} (apply hash-map arg-pairs)
        context (if (= :default context) rdf/common-prefixes context)
        iri-fn (if (fn? iri-fn)
                 (let [f (cond
                           (= keyword iri-fn) keyword3
                           (= str iri-fn) string3
                           :else iri-fn)]
                   (if context (context-iri context f) iri-fn))
                 (case iri-fn
                   (:uri "uri") uri
                   (:url "url") url
                   (:iri "iri") (if context (context-iri context iri) iri)
                   (:qname :curie "qname" "curie") (if context (context-iri context iri)
                                                       (throw (ex-info "CURIEs cannot be created without a context"
                                                                       {:query q})))
                   (:str :string "str" "string") (if context (context-iri context string3) identity)
                   (:keyword "keyword") (if context (context-iri context keyword3)
                                            (throw (ex-info "CURIEs cannot be created without a context"
                                                            {:query q})))
                   (throw (ex-info (str "Unsupported IRI function value: " iri-fn) {:iri-fn iri-fn}))))]
    [service q iri-fn (dissoc arg-map :iri-fn :context)]))

(defn query
  "Issues a SPARQL query. See process-args for the argument values"
  [& args]
  (let [[service q iri-fn extra-args] (process-args args)]
    (do-query service q iri-fn extra-args)))

(defn query-table
  "Issues a SPARQL query, returning a seq of vectors. See process-args for the argument values"
  ([& args]
   (let [results (apply query args)]
     (map #(vec (vals %)) results))))

(def ^:private no-context-iri-fns
  "iri-fn values that require a `:context` to be meaningful."
  #{:qname :curie :keyword "qname" "curie" "keyword"})

(defn- neutralize-iri-fn
  "Rewrites any `:iri-fn` value in `args` that would require a `:context` to `:iri`. Used when the
  caller's iri-fn choice cannot affect output (e.g. UPDATE operations have no result data to decode)
  but would otherwise cause process-args to reject the arguments."
  [args]
  (loop [acc [] xs args]
    (if-let [[a & r] (seq xs)]
      (if (and (= :iri-fn a) (no-context-iri-fns (first r)))
        (recur (conj acc :iri-fn :iri) (rest r))
        (recur (conj acc a) r))
      acc)))

(defn update!
  "Issues a SPARQL UPDATE. See process-args for the argument values. The result-decoding args
  (`:iri-fn`, `:context`) are accepted for API consistency but unused, since UPDATE returns no
  result data. Returns the query response."
  [& args]
  (let [[service q _ extra-args] (process-args (neutralize-iri-fn args))]
    (do-update service q extra-args)))

