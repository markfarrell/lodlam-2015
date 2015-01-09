(ns family-photos
    (:use arcadia.core)
    (:require [muninn])
    (:require [clojure.data.json :as json])
    (:import [UnityEngine Debug]))

(def sparql-url "http://dbpedia.org/sparql")
(def sparql-default-graph-uri "http://dbpedia.org")
(def sparql-format "json")
(def sparql-timeout 30000)
(def sparql-debug "on")

(def sparql-query
     (str "select distinct ?parents ?thumb where {"
          "{ <http://dbpedia.org/resource/Anne_Frank> <http://dbpedia.org/property/parents> ?parents . }"
          "UNION"
          "{ <http://dbpedia.org/resource/Anne_Frank> <http://dbpedia.org/property/spouse> ?parents . }"
          "UNION"
          "{ <http://dbpedia.org/resource/Anne_Frank> <http://dbpedia.org/ontology/relative> ?parents . }"
          "UNION"
          "{ <http://dbpedia.org/resource/Anne_Frank> <http://dbpedia.org/property/children> ?parents . }"
          "UNION"
          "{ <http://dbpedia.org/resource/Anne_Frank> <http://dbpedia.org/ontology/child> ?parents . }"
          "UNION"
          "{ ?parents <http://dbpedia.org/property/parents> <http://dbpedia.org/resource/Anne_Frank> . }"
          "UNION"
          "{ ?parents <http://dbpedia.org/ontology/relative> <http://dbpedia.org/resource/Anne_Frank> . }"
          "UNION"
          "{ ?parents <http://dbpedia.org/property/children> <http://dbpedia.org/resource/Anne_Frank> . }"
          "UNION"
          "{ ?parents <http://dbpedia.org/ontology/child> <http://dbpedia.org/resource/Anne_Frank> . }"
          "UNION"
          "{ ?parents <http://dbpedia.org/property/spouse> <http://dbpedia.org/resource/Anne_Frank> . }"
          "UNION"
          "{ ?alt <http://dbpedia.org/property/children> <http://dbpedia.org/resource/Anne_Frank> ."
          "  ?alt <http://dbpedia.org/property/children> ?parents ."
          "}"
          "UNION"
          "{ ?alt <http://dbpedia.org/ontology/child> <http://dbpedia.org/resource/Anne_Frank> ."
          "  ?alt <http://dbpedia.org/ontology/child> ?parents . }"
          "  ?parents <http://dbpedia.org/ontology/thumbnail> ?thumb ."
          "}"))

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
      []
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

(defcomponent FamilyPhoto
              [^UnityEngine.Texture2D painting]
              (Start [this]
                     (do
                       (set! (. this painting)
                         (rand-nth (thumbnails (search))))
                       (muninn/set-main-texture! this
                                                 (. this painting))))
              (Update [this] ()))


