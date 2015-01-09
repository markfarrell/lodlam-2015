(ns rijksmuseum
    (:use arcadia.core)
    (:require [clojure.data.json :as json])
    (:require [muninn])
    (:import [UnityEngine Debug]))

(def api-key "WfTGhlrw")

(def query "")
(def results-per-page 100)
(def imgonly true)

(defn make-url
      "Make url to search for art."
      [query year-from year-to page]
      (str "https://www.rijksmuseum.nl/api/nl/collection"
           "?key="
           api-key
           "&q="
           (muninn/url-encode query)
           "&yearfrom="
           year-from
           "&yearto="
           year-to
           "&p="
           page
           "&ps="
           results-per-page
           "&imgonly="
           imgonly
           "&format=json"))

(defn search
      "Search for art; wait until download is finished; produce JSON object."
      [query year-from year-to page]
      (let [url (make-url query year-from year-to page)]
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

(defn random-thumbnail
      "Expects a search result; produces a random texture."
      [search-result]
      (get-texture-2d
        (rand-nth (thumbnail-urls search-result))))

(defcomponent Painting
              [^UnityEngine.Texture2D painting ^int year-from ^int year-to ^int page]
              (Start [this]
                     (do
                       (set! (. this painting)
                         (random-thumbnail
                           (search query year-from year-to page)))
                       (muninn/set-main-texture! this (. this painting))))
              (Update [this] ()))
