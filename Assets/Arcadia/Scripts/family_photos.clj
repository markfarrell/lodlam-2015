(ns family-photos
  (:use arcadia.core)
  (:require [muninn])
  (:require [dbpedia])
  (:import [UnityEngine Debug]))

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

(defcomponent FamilyPhoto
  [^UnityEngine.Texture2D painting]
  (Start [this]
         (do
           (set! (. this painting)
                 (rand-nth (dbpedia/thumbnails (dbpedia/search sparql-query))))
           (muninn/set-main-texture! this
                                     (. this painting))))
  (Update [this] ()))
