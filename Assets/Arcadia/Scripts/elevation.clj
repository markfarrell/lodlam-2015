(ns elevation
  (:use arcadia.core)
  (:require muninn)
  (:import JaggedToMultidimensional)
  (:import [UnityEngine Debug]))

(def base-url "http://localhost:8000/")

(defn get-height-map
  "Takes values for min/max longitude and latitudes. Produces a 2d texture of height data."
  [min-long min-lat max-long max-lat]
  (. (muninn/GET (str base-url
                      "?minlong="
                      min-long
                      "&minlat="
                      min-lat
                      "&maxlong="
                      max-long
                      "&maxlat="
                      max-lat))
     texture))

(defn texture->vertices
  "Takes a 2d texture; produces a list of vertices."
  [texture width length]
  (let [y-scale (* 145.0 10.0)
        x-scale 90
        z-scale 90]
    (map (fn [[x z]]
             (Vector3. (* x x-scale)
                       (* (.grayscale (.GetPixel texture x z)) y-scale)
                       (* z z-scale)))
         (muninn/grid width
                      length))))

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
  [width length height-at]
  (map (fn [[x z]]
           (Vector3. x
                     (height-at x z)
                     z))
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

(defn create-terrain
  "Takes collections of vertices, uvs and triangle indices; produces a game object."
  [vertices uvs triangle-indices]
  (let [mesh (Mesh.)
        plane (GameObject.)]
    (do (set! (.vertices mesh)
              (into-array vertices))
        (set! (.uv mesh)
              (into-array uvs))
        (set! (.triangles mesh)
              (int-array triangle-indices))
        (.RecalculateNormals mesh)
        (.RecalculateBounds mesh)
        (set! (.mesh (.AddComponent plane "MeshFilter")) mesh)
        (set! (.material (.AddComponent plane "MeshRenderer"))
              (UnityEngine.Resources/Load "Terrain" UnityEngine.Material))
        (.AddComponent plane "MeshCollider")
        plane)))

(defn texture->terrain
  "Takes a 2d texture; produces a game object."
  [texture]
  (let [width (min (.width texture) 254)
        length (min (.height texture) 254)
        vertices (texture->vertices texture width length)
        uvs (muninn/plane-uvs width length)
        triangle-indices (muninn/plane-triangle-indices width length)]
    (create-terrain vertices uvs triangle-indices)))

(defcomponent Elevation [^UnityEngine.Texture2D height-map ^float min-long ^float min-lat ^float max-long ^float max-lat]
  (Start [this]
         (do (set! (. this height-map)
                   (get-height-map min-long min-lat max-long max-lat))
             (texture->terrain (. this height-map)))))
