(ns rijksmuseum
  (:use arcadia.core)
  (:require [clojure.data.json :as json])
  (:require [muninn])
  (:import [UnityEngine Debug]))

;; (: api-key String)
(def api-key "WfTGhlrw")

;; (: query String)
(def query "")

;; (: results-per-page Int)
(def results-per-page 1)

;; (: img-only String)
(def img-only "True")

;; (: art-type String)
(def art-type "print")

;; (: make-url (-> Int Int Int String))
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

;; (: search (-> Int Int JSExpr))
(defn search
  "Search for art; wait until download is finished; produce JSON object."
  [year-from year-to page]
  (let [url (make-url year-from year-to page)]
    (json/read-str (. (muninn/GET url) text))))

;; (: thumbnail-urls (-> JSExpr (Listof String)))
(defn thumbnail-urls
  "Expects a search result; extracts thumbnail urls."
  [search-result]
  (let [art-with-thumbnails
        (filter (fn [art] (true? (second (find art "hasImage"))))
                (second (find search-result "artObjects")))]
    (map (fn [art] (second (find (second (find art "webImage")) "url")))
         art-with-thumbnails)))

;; (: get-texture-2d (-> String Texture2D))
(defn get-texture-2d
  "Get a Unity texture from the web."
  [url]
  (. (muninn/GET url) texture))

;; (: thumbnails (-> String (Listof Texture2D)))
(defn thumbnails
  "Expects a search result; fetches art thumbnails; produces Unity textures."
  [search-result]
  (map get-texture-2d (thumbnail-urls search-result)))

;; (: aspect-ratio (-> Texture2D Real))
(defn aspect-ratio
  "Expects a texture; produces its aspect ratio as a float."
  [texture]
  (float
    (/ (. texture width)
       (. texture height))))

;; Painting Component - with a workaround due to coroutine support currently missing in Clojure.
(defcomponent Painting
  [^WWW count-www ^WWW selection-www ^boolean count-www-done ^boolean selection-www-done ^int year-from ^int year-to]
  (Start [this]
         (do
           (muninn/set-main-texture! this
                                     (UnityEngine.Resources/Load "muninn"))
           (set! count-www
                 (WWW. (make-url year-from year-to 1)))))
  (Update [this]
          (do
            (if (and (not count-www-done)
                     (.isDone count-www))
              (let [search-result (json/read-str (. count-www text))
                    count (second (find search-result "count"))]
                (do  (set! count-www-done (boolean true))
                    (set! selection-www (WWW. (make-url year-from year-to
                                                        (rand-int (+ 1 count))))))))
            (if (and count-www-done
                     (not selection-www-done)
                     (.isDone selection-www))
              (let [search-result (json/read-str (. selection-www text))
                    new-texture (get-texture-2d (first (thumbnail-urls search-result)))]
                (do (UnityEngine.Debug/Log search-result)
                    (set! selection-www-done (boolean true))
                    (muninn/set-main-texture! this new-texture)))))))

