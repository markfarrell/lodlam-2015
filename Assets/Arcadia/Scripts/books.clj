(ns books
  (:require [muninn])
  (:require [clojure.data.json :as json]))

(def sparql-url "http://wifo5-04.informatik.uni-mannheim.de/gutendata/sparql")
(def sparql-format "json")
(def sparql-timeout 30000)
(def sparql-debug "on")

(def sparql-query
  (str "PREFIX dc: <http://purl.org/dc/elements/1.1/>"
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>"
       "PREFIX dcterms: <http://purl.org/dc/terms/>"
       "SELECT DISTINCT ?resource ?title ?creator ?version {"
       "  ?resource dc:title ?title ."
       "  ?resource dc:creator ?creator ."
       "  ?creator foaf:name ?name ."
       "  ?resource dcterms:version ?version ."
       "  FILTER regex(?name, \"18\\\\d\\\\d-19\\\\d\\\\d\") ."
       "  FILTER regex(str(?version), \"\\\\.txt$\") ."
       "}"))

;; (: text->texture (-> String Texture2D))
(defn text->texture
  [text]
  (let [font
        (UnityEngine.Resources/Load "font")
        atlas
        (get (get (clojure.data.json/read-str (.text (UnityEngine.Resources/Load "font_atlas"))) "chars") "char")
        text-atlas
        (map (fn [c]
               (first (filter (fn [a]
                                (= (int c)
                                   (int (get a "id"))))
                              atlas)))
             (seq text))
        new-texture-width
        (reduce +
                (map (fn [a]
                       (+ (int (get a "xoffset"))
                          (int (get a "xadvance"))
                          (int (get a "width"))))
                     text-atlas))
        new-texture-height
        (apply max
               (map (fn [a]
                      (+ (int (get a "yoffset"))
                         (int (get a "height"))))
                    text-atlas))
        new-texture
        (Texture2D. new-texture-width new-texture-height)
        cursor-positions
        (reductions
          (fn [[x y]
               [xoffset xadvance yoffset]]
            [(+ x xoffset xadvance)
             (+ y yoffset)])
          [0 0]
          (map (fn [a]
                 [(int (get a "xoffset"))
                  (int (get a "xadvance"))
                  (int (get a "yoffset"))])
               text-atlas))
        pixels-list
        (map (fn [a]
               [(int (get a "width"))
                (int (get a "height"))
                (.GetPixels font
                            (int (get a "x"))
                            (int (get a "y"))
                            (int (get a "width"))
                            (int (get a "height")))])
             text-atlas)]
    (do
      (doseq
        [cursor-position cursor-positions
         [width height pixels] pixels-list]
        (.SetPixels
          new-texture
          (first cursor-position)
          (second cursor-position)
          width height pixels))
      (.Apply new-texture)
      new-texture)))

(defn make-url
  "Expects a SPARQL query; produces a Gutenberg URL."
  [query]
  (str sparql-url
       "?"
       "query="
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

(defn books
  []
  (search sparql-query))
