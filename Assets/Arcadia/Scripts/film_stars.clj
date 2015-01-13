(ns film-stars
    (:use arcadia.core)
    (:require [muninn])
    (:require [dbpedia])
    (:import [UnityEngine Debug]))

(def sparql-query
     (str "select ?actor ?thumb ?start ?end {"
          "?actor dbpedia-owl:occupation dbpedia:Actor ."
          "?actor dbpedia-owl:activeYearsStartYear ?start ."
          "?actor dbpedia-owl:activeYearsEndYear ?end ."
          "?actor dbpedia-owl:thumbnail ?thumb ."
          "FILTER (?start > \"1929-01-01\"^^xsd:date)"
          "FILTER (?end < \"1945-01-01\"^^xsd:date)"
          "}"))

(defn rand-thumbnail-url
      "Get a random thumbnail URL."
      []
      (rand-nth
        (dbpedia/thumbnail-urls
          (dbpedia/search sparql-query))))

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
              [^UnityEngine.Texture2D painting]
              (Start [this]
                     (do
                       (set! (. this painting)
                         (. (muninn/GET (rand-thumbnail-url))
                            texture))
                       (muninn/set-main-texture! this
                                                 (. this painting)))))




