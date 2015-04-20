(ns dbpedia
    (:require [muninn])
    (:require [clojure.data.json :as json]))

;; (: sparql-url String)
(def sparql-url "http://dbpedia.org/sparql")

;; (: sparql-default-graph-uri String)
(def sparql-default-graph-uri "http://dbpedia.org")

;; (: sparql-format String)
(def sparql-format "json")

;; (: sparql-timeout Number)
(def sparql-timeout 30000)

;; (: sparql-debug String)
(def sparql-debug "on")

;; (: make-url (-> String String))
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

;; (: search (-> String JSExpr))
(defn search
      "Execute SPARQL query; produces a JSON object."
      [sparql-query]
      (let [url (make-url sparql-query)
            search-result (json/read-str (. (muninn/GET url) text))]
        (do (UnityEngine.Debug/Log search-result)
            search-result)))

;; (: thumbnail-urls (-> JSExpr (Listof String)))
(defn thumbnail-urls
      "Expects a search result; produces thumbnail urls."
      [search-result]
      (let [bindings (second (find (second (find search-result "results")) "bindings"))]
        (map (fn [binding]
                 (second (find (second (find binding "thumb")) "value")))
             bindings)))

;; (: thumbnails (-> JSExpr (Listof Texture2D)))
(defn thumbnails
      "Expects a search result; produces Unity3d textures."
      [search-result]
      (map (fn [url] (. (muninn/GET url) texture))
           (thumbnail-urls search-result)))

