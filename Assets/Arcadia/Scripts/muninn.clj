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
  [width length]
  (mapcat (fn [j]
            (map (fn [i] [i j])
                 (range width)))
          (range length)))

(defn plane-triangle-indices
  [width length]
  (mapcat
    (fn [[x z]]
      (list (+ (* z width) x)
            (+ (* (inc z) width) x)
            (+ (* z width)
               (inc x))
            (+ (* (inc z) width) x)
            (+ (* (inc z) width)
               (inc x))
            (+ (* z width)
               (inc x))))
    (grid (dec width)
          (dec length))))

(defn plane-vertices
  [width length]
  (map (fn [[x z]]
           (Vector3. x 0.0 z))
       (grid width length)))

(defn plane-uvs
  [width length]
    (map (fn [[x z]]
           (Vector2. (* x
                        (/ 1
                           (max (dec width) 1)))
                     (*  z
                        (/ 1
                           (max (dec length) 1)))))
         (grid width length)))

(defn create-plane
  [width length]
  (let [mesh  (Mesh.)
        plane (GameObject.)]
    (do (set! (.vertices mesh)
              (into-array (plane-vertices width length)))
        (set! (.uv mesh)
              (into-array (plane-uvs width length)))
        (set! (.triangles mesh)
              (int-array (plane-triangle-indices width length)))
        (.RecalculateNormals mesh)
        (.RecalculateBounds mesh)
        (set! (.mesh (.AddComponent plane "MeshFilter")) mesh)
        (.AddComponent plane "MeshRenderer")
        plane)))

(defn set-main-texture!
      "Expects a GameObject; sets main texture."
      [game-object texture]
      (set! (. (. (. game-object renderer) material) mainTexture)
        texture))
