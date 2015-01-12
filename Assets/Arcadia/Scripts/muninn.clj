(ns muninn
    (:use arcadia.core)
    (:import [UnityEngine Debug]))

(defn url-encode
      "Encodes a URL string."
      [str]
      (System.Web.HttpUtility/UrlEncode str))

(defn GET
      "Start a download from the URL; wait to finish; produce UnityEngine.WWW object."
      [url]
      (let [client (WWW. url)]
        (do
          (some true? (repeatedly #(. client isDone)))) ; Wait to finish
        client))

(defn grid
      "Create a width by height grid of points. e.g. (grid 2 2) => ([0 0] [0 1] [1 0] [1 1])"
      [width height]
      (mapcat (fn [i]
                  (map (fn [j] [i j])
                       (range height)))
              (range width)))

(defn set-main-texture!
      "Expects a GameObject; sets main texture."
      [game-object texture]
      (set! (. (. (. game-object renderer) material) mainTexture)
        texture))
