# Spike

A simple SPARQL client.

## Usage
### Leiningen/Boot
```clojure
[org.clojars.quoll/spike "0.0.1"]
```

### Clojure CLI/deps.edn
```clojure
org.clojars.quoll/spike {:mvn/version "0.0.1"}
```

The only supported function right now is `query`:

```clojure
(:require '[quoll.rdf.spike] :refer [query])

(query "http://localhost:7200/repositories/data" "SELECT ?types WHERE { ?s a ?types }")
```

A file named `.sparql` in the user's home directory can also keep a default endpoint:

```bash
$ cat ~/.sparql
url=url=http://localhost:7200/repositories/data
```

Alternately, the an environment variable `SPARQL` can also be used:
```bash
$ env SPARQL=http://localhost:7200/repositories/data clj
```

## License

Copyright © 2023 Paula Gearon

Distributed under the Eclipse Public License version 2.0.

