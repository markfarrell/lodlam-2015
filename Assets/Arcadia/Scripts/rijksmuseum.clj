(ns rijksmuseum
   (:use arcadia.core)
   (:require [clojure.data.json :as json])
   (:import [UnityEngine Debug]))

(def api-key "WfTGhlrw")

; Search params 
(def year-from 1800)
(def year-to 1943)
(def art-type "painting")

(defn make-url
   "Make url to search for art."
   [query]
   (str "https://www.rijksmuseum.nl/api/nl/collection"
        "?key="
        api-key
        "&q="
        query
        "&yearfrom"
        year-from
        "&yearto="
        year-to
        "&type="
        art-type
        "&format=json"))

(defn search
   "Search for art; wait until download is finished; produce JSON object."
   [query]
   (let [client (WWW. (make-url query))]
      (do
         (some true? (repeatedly #(. client isDone)))) ; Wait to finish
         (json/read-str (. client text))))

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
   (let [client (WWW. url)]
     (do 
       (some true? (repeatedly #(. client isDone)))) ; Wait to finish
       (. client texture)))

(defn thumbnails
   "Expects a search result; fetches art thumbnails; produces Unity textures."
   [search-result]
   (map get-texture-2d (thumbnail-urls search-result)))

(defn make-painting!
   "Expects a thumbnail; spawns a game object in the scene; produces the game object."
   [thumbnail]
   (let [painting (create-primitive :plane)]
     (do 
       (set! (. (. (. painting renderer) material) mainTexture)
             thumbnail)
       painting)))




