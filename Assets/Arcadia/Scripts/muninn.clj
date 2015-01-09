(ns muninn
    (:use arcadia.core)
    (:import [System.Web HttpUtility])
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

(defn set-main-texture!
      "Expects a GameObject; sets main texture."
      [game-object texture]
      (set! (. (. (. game-object renderer) material) mainTexture)
        texture))
