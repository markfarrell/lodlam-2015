(ns rijksmuseum
    (:use arcadia.core)
    (:require [clojure.data.json :as json])
    (:require [muninn])
    (:import [UnityEngine Debug]))

(def api-key "WfTGhlrw")

(def query "")
(def results-per-page 1)
(def img-only "True")
(def art-type "print")

(defn make-url
      "Make url to search for art."
      [year-from year-to page]
      (str "https://www.rijksmuseum.nl/api/en/collection"
           "?key="
           api-key
           "&q="
           query
           "&yearfrom="
           year-from
           "&yearto="
           year-to
           "&p="
           page
           "&ps="
           results-per-page
           "&imgonly="
           img-only
           "&type="
           art-type
           "&format=json"))

(defn search
      "Search for art; wait until download is finished; produce JSON object."
      [year-from year-to page]
      (let [url (make-url year-from year-to page)]
        (json/read-str (. (muninn/GET url) text))))

(defn thumbnail-urls
      "Expects a search result; extracts thumbnail urls."
      [search-result]
      (let [art-with-thumbnails
             (filter (fn [art] (true? (second (find art "hasImage"))))
                     (second (find search-result "artObjects")))]
        (map (fn [art] (second (find (second (find art "webImage")) "url")))
             art-with-thumbnails)))

(defn get-texture-2d
      "Get a Unity texture from the web."
      [url]
      (. (muninn/GET url) texture))

(defn thumbnails
      "Expects a search result; fetches art thumbnails; produces Unity textures."
      [search-result]
      (map get-texture-2d (thumbnail-urls search-result)))

(defn aspect-ratio
      "Expects a texture; produces its aspect ratio as a float."
      [texture]
      (float
        (/ (. texture width)
           (. texture height))))

(defcomponent Painting
              [^UnityEngine.Texture2D painting ^int year-from ^int year-to]
              (Start [this]
                     (let [page 1
                           search-result (search year-from year-to page)
                           count (second (find search-result "count"))]
                       (do
                         (set! (. this painting)
                               (get-texture-2d (first (thumbnail-urls
                                 (search year-from
                                         year-to
                                         (rand-int (+ 1 count)))))))
                         (muninn/set-main-texture! this (. this painting))))))
