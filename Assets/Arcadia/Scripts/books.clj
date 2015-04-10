(ns books
  (:use arcadia.core)
  (:require [muninn])
  (:require [clojure.data.json :as json]))

;; (: sparql-url String)
(def sparql-url "http://wifo5-04.informatik.uni-mannheim.de/gutendata/sparql")

;; (: sparql-format String)
(def sparql-format "json")

;; (: sparql-timeout Int)
(def sparql-timeout 30000)

;; (: sparql-debug String)
(def sparql-debug "on")

;; (: sparql-query String)
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

;; (: font Texture2D)
(def font
  (UnityEngine.Resources/Load "font"))

;; (: atlas (Listof JSExpr))
(def atlas
  (get (get (clojure.data.json/read-str (.text (UnityEngine.Resources/Load "font_atlas"))) "chars") "char"))

;; (: text->atlas (-> String (Listof JSExpr)))
(defn text->atlas
  [text]
  (map (fn [c]
         (first (filter (fn [a]
                          (= (int c)
                             (int (get a "id"))))
                        atlas)))
       (seq text)))

;; (: text->cursor-positions (-> String (Listof (Vector Int Int))))
(defn text->cursor-positions
  [text]
  (drop-last
    (reductions
      (fn [[x y] xadvance]
        [(+ x xadvance) y])
      [0 0]
      (map (fn [a]
             (int (get a "xadvance")))
           (text->atlas text)))))

;; (: text->new-texture-width (-> String Int))
(defn text->new-texture-width
  [text]
  (reduce +
          (map (fn [a]
                 (int (get a "xadvance")))
               (text->atlas text))))

;; (: text->new-texture-height (-> String Int))
(defn text->new-texture-height
  [text]
  (apply max
         (map (fn [a]
                (+ (int (get a "yoffset"))
                   (int (get a "height"))))
              (text->atlas text))))

;; (: text->pixels-lists (-> String (Listof (Vector Int Int (Listof Color)))))
(defn text->pixels-lists
  [text]
  (map (fn [a]
         [(int (get a "width"))
          (int (get a "height"))
          (.GetPixels font
                      (int (get a "x"))
                      (- (.height font)
                         (+ (int (get a "y"))
                            (int (get a "height"))))
                      (int (get a "width"))
                      (int (get a "height")))])
       (text->atlas text)))

;; (: text->offsets (-> String (Listof (Vector Int Int))))
(defn text->offsets
  [text]
  (map (fn [a]
         [(int (get a "xoffset"))
          (int (get a "yoffset"))])
       (text->atlas text)))

;; (: text->texture (-> String Texture2D))
(defn text->texture
  [text]
  (let [new-texture
        (UnityEngine.Texture2D.
          (text->new-texture-width text)
          (text->new-texture-height text))]
    (do
      (doseq [x (range (.width new-texture))
              y (range (.height new-texture))]
        (.SetPixel new-texture x y UnityEngine.Color/white))
      (doseq
        [[[x y] [xoffset yoffset] [width height pixels]]
         (map vector
              (text->cursor-positions text)
              (text->offsets text)
              (text->pixels-lists text))]
        (.SetPixels
          new-texture
          (+ x xoffset)
          (+ y yoffset)
          width height pixels))
      (.Apply new-texture)
      new-texture)))

;; (: text->plane (-> String GameObject))
(defn text->plane
  [text]
  (let [plane (create-primitive :quad)
        texture (text->texture text)]
    (do
      (set! (.. plane renderer material mainTexture)
            texture)
      (set! (.. plane transform localScale)
            (Vector3. (.width texture)
                      (.height texture)
                      1.0))
      plane)))

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
