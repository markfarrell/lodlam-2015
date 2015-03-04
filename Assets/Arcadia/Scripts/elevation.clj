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

(defn texture->grayscales
  "Takes a 2d texture; produces a list of grayscale values of its colors (floats)."
  [texture]
  (map (fn [[x z]]
           (.grayscale (.GetPixel texture x z)))
       (muninn/grid (.width texture)
                    (.height texture))))

(defn texture->vertices
  "Takes a 2d texture; produces a list of vertices."
  [texture]
  (map (fn [[x z]]
           (Vector3. x
                     (* (.grayscale (.GetPixel texture x z)) 145)
                     z))
       (muninn/grid (.width texture)
                    (.height texture))))

(defn texture->uvs
  "Takes a 2d texture; produces a list of normals."
  [texture]
  (map (fn [[x z]]
         (Vector2. (* x
                      (/ 1
                         (max (dec (.width texture)) 1)))
                   (*  z
                      (/ 1
                         (max (dec (.height texture)) 1)))))
       (muninn/grid (.width texture)
                    (.height texture))))

(defn texture->triangle-indices
  "Takes a 2d texture; produces a list of triangle indices."
  [texture]
  (muninn/plane-triangle-indices (.width texture)
                                 (.height texture)))

(defn texture->plane
  "Takes a 2d texture; produces a plane mesh."
  [texture]
  (let [mesh (Mesh.)
        plane (GameObject.)]
    (do (set! (.vertices mesh)
              (into-array (texture->vertices texture)))
        (set! (.uv mesh)
              (into-array (texture->uvs texture)))
        (set! (.triangles mesh)
              (int-array (texture->triangle-indices texture)))
        (.RecalculateNormals mesh)
        (.RecalculateBounds mesh)
        (set! (.mesh (.AddComponent plane "MeshFilter")) mesh)
        (.AddComponent plane "MeshRenderer")
        plane)))

(defcomponent Elevation [^UnityEngine.Texture2D height-map ^float min-long ^float min-lat ^float max-long ^float max-lat]
  (Start [this]
         (do (set! (. this height-map)
                   (get-height-map min-long min-lat max-long max-lat))
             (texture->plane (. this height-map)))))
