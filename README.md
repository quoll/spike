# Spike

A simple SPARQL client.
<img src="https://github.com/quoll/spike/assets/358875/50575440-6f81-47c7-bd4e-794770d7280f" alt="Spike the Dragon" align="right"/>

## Usage
### Leiningen/Boot
```clojure
[org.clojars.quoll/spike "0.0.2"]
```

### Clojure CLI/deps.edn
```clojure
org.clojars.quoll/spike {:mvn/version "0.0.2"}
```

The only supported functions right now are `query` and `query-table`:

```clojure
(:require '[quoll.rdf.spike] :refer [query query-table])

(query "http://localhost:7200/repositories/data" "SELECT ?types WHERE { ?s a ?types }")
```

`query-table` returns the same result as a seq of vectors.

### Service

A default service can be set, allowing the first argument to be removed.

A file named `default` in the `$HOME/.sparql` directory can also keep a default endpoint:

```bash
$ cat ~/.sparql
url=http://localhost:7200/repositories/data
```

Alternately, the an environment variable `SPARQL` can also be used:
```bash
$ env SPARQL=http://localhost:7200/repositories/data clj
```

With these set, then a query can be called without the service argument:

```clojure
(:require '[quoll.rdf.spike] :refer [query-table])

(query-table "SELECT ?types WHERE { ?s a ?types }")
```

### Named arguments
Queries can also accept named arguments to customize the result data.

`:context`
A map of prefixes as keywords to namespace strings. For instance:
```clojure
{:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 :owl "http://www.w3.org/2002/07/owl#"}
```
These are used with IRIs to detect namespaces, and potentially return then in a CURIE form.

Instead of a map, the keyword `:default` may be used. This will use a map with prefixes for common namespaces: rdf, rdfs, xsd, skos, owl, dcterms.

`:iri-fn`
Specifies a function to use to create IRIs. Accepts strings or keywords for well known types: uri, url, str, keyword, qname, curie. QNames and CURIEs are the same as IRI, but report an error when no context is available.

Keyword (the name or the function) will also report an error if no context is available, and will return a string if no namespace in a context can be matched to an IRI.

Using strings or keywords will return an equivalent CURIE for an IRI, but the original IRI string can only be reconstructed with the context. Conversely, the default IRI objects contain all of the original data, even though they serialize in shortened form.

## Roadmap

* Add Update support.
* Basic SPARQL parsing to autodetect query PREFIXes as a context.
* Structured SPARQL construction.

## License

Copyright © 2023 Paula Gearon

Distributed under the Eclipse Public License version 2.0.

