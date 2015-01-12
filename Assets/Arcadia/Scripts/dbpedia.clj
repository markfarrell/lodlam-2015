(ns dbpedia
    (:require [muninn])
    (:require [clojure.data.json :as json]))

(def sparql-url "http://dbpedia.org/sparql")
(def sparql-default-graph-uri "http://dbpedia.org")
(def sparql-format "json")
(def sparql-timeout 30000)
(def sparql-debug "on")

(defn make-url
      "Expects a SPARQL query; produces a DBpedia URL."
      [query]
      (str sparql-url
           "?"
           "default-graph-uri="
           (muninn/url-encode sparql-default-graph-uri)
           "&query="
           (muninn/url-encode query)
           "&format="
           (muninn/url-encode sparql-format)
           "&timeout="
           sparql-timeout
           "&debug="
           sparql-debug))

(defn search
      "Execute SPARQL query; produces a JSON object."
      [sparql-query]
      (let [url (make-url sparql-query)]
        (json/read-str (. (muninn/GET url) text))))

(defn thumbnail-urls
      "Expects a search result; produces thumbnail urls."
      [search-result]
      (let [bindings (second (find (second (find search-result "results")) "bindings"))]
        (map (fn [binding]
                 (second (find (second (find binding "thumb")) "value")))
             bindings)))

(defn thumbnails
      "Expects a search result; produces Unity3d textures."
      [search-result]
      (map (fn [url] (. (muninn/GET url) texture))
           (thumbnail-urls search-result)))

