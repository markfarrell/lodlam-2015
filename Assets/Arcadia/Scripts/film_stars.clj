(ns film-stars
    (:use arcadia.core)
    (:require [muninn])
    (:require [dbpedia])
    (:import [UnityEngine Debug]))

(defn sparql-query
  [year-from year-to]
  (str "PREFIX wordnet: <http://www.w3.org/2006/03/wn/wn20/instances/>"
       "SELECT DISTINCT ?actor ?thumb ?start {"
       "  {"
       "    ?actor dbpprop:wordnet_type wordnet:synset-actor-noun-1 ."
       "  } UNION {"
       "    ?actor dbpedia-owl:occupation dbpedia:Actor ."
       "  }"
       "  ?actor dbpedia-owl:thumbnail ?thumb ."
       "  ?actor dbpedia-owl:activeYearsStartYear ?start ."
       "  FILTER (?start > \"" year-from "-01-01\"^^xsd:date)"
       "  FILTER (?start < \"" year-to "-01-01\"^^xsd:date)"
       "  FILTER EXISTS {"
       "    {"
       "     ?film dbpedia-owl:starring ?actor ."
       "    } UNION {"
       "      ?film dbpprop:starring ?actor ."
       "    }"
       "  }"
       "}"))

(defn rand-thumbnail-url
  "Get a random thumbnail URL."
  [year-from year-to]
  (let [random-thumbnail-url
        (rand-nth
          (dbpedia/thumbnail-urls
            (dbpedia/search (sparql-query year-from year-to))))]
    (do
      (UnityEngine.Debug/Log random-thumbnail-url)
      random-thumbnail-url)))

(defn instantiate-grid!
  "Spawn a grid of GameObject clones."
  [game-object width height]
  (map (fn [[x y]]
         (instantiate game-object
                      (UnityEngine.Vector3. (* 3.0 x)
                                            (* 3.0 y)
                                            0.0)
                      (UnityEngine.Quaternion/Euler 15.0 0 0)))
       (muninn/grid width height)))

(defcomponent FilmStars
  [^UnityEngine.Texture2D painting ^int year-from ^int year-to]
  (Start [this]
         (do
           (set! (. this painting)
                 (. (muninn/GET (rand-thumbnail-url year-from year-to))
                    texture))
           (muninn/set-main-texture! this
                                     (. this painting)))))




