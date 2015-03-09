(ns elevation
  (:use arcadia.core)
  (:require muninn)
  (:import JaggedToMultidimensional)
  (:import [UnityEngine Debug]))

(def base-url "http://localhost:8000/")

(def partition-width 254)
(def partition-length 254)

(def x-scale 90.0)
(def y-scale (* 145.0 20))
(def z-scale 90.0)

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
  (map (fn [[x z]]
         (Vector3. (* x x-scale)
                   (* (.grayscale (.GetPixel texture x z)) y-scale)
                   (* z z-scale)))
       (muninn/grid width
                    length)))

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
    (muninn/grid (dec width)
                 (dec length))))

(defn plane-vertices
  [width length height-at]
  (map (fn [[x z]]
           (Vector3. x
                     (height-at x z)
                     z))
       (muninn/grid width length)))

(defn plane-uvs
  [width length]
    (map (fn [[x z]]
           (Vector2. (* x
                        (/ 1
                           (max (dec width) 1)))
                     (*  z
                        (/ 1
                           (max (dec length) 1)))))
         (muninn/grid width length)))

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
        (set! (.name plane) "Terrain")
        plane)))

(defn texture->terrain
  "Takes a 2d texture; produces a game object."
  [texture]
  (let [width (min (.width texture) partition-width)
        length (min (.height texture) partition-length)
        vertices (texture->vertices texture width length)
        uvs (plane-uvs width length)
        triangle-indices (plane-triangle-indices width length)]
    (create-terrain vertices uvs triangle-indices)))

(defn create-terrains
  [min-long min-lat max-long max-lat]
  (let [height-map (get-height-map min-long min-lat max-long max-lat)
        texture-partitions (muninn/texture-partitions height-map partition-width partition-length)
        terrain-objects (map texture->terrain texture-partitions)
        terrain-positions (muninn/grid-partitions-positions (.width height-map)
                                                            (.height height-map)
                                                            partition-width
                                                            partition-length)]
    (map (fn [[terrain-object [x z]]]
           (do (.Translate (.transform terrain-object)
                           (* x x-scale)
                           0
                           (* z z-scale))
               terrain-object))
         (map vector terrain-objects terrain-positions))))

(defcomponent Elevation [^float min-long ^float min-lat ^float max-long ^float max-lat]
  (Start [this]
         (create-terrains min-long min-lat max-long max-lat)))
