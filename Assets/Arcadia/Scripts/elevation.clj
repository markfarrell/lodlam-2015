(ns elevation
  (:use arcadia.core)
  (:require muninn)
  (:import JaggedToMultidimensional)
  (:import [UnityEngine Debug]))

(def base-url "http://localhost:8000/")

(defn get-height-map
  "Takes values for min/max longitude and latitudes. Produces a texture 2d of height data."
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
  (map (fn [[x y]]
           (.grayscale (.GetPixel texture x y)))
       (muninn/grid (.width texture)
                    (.height texture))))

(defn texture->terrain-data
  "Takes a 2d texture; produces a terrain data object."
  [texture]
  (let [terrain-data (TerrainData.)
        height-map-resolution 512
        base-map-resolution 1024
        width (.width texture)
        length (.height texture)
        height 256]
    (do (set! (.size terrain-data)
              (Vector3. width height length))
        (set! (.heightmapResolution terrain-data)
              height-map-resolution)
        (set! (.baseMapResolution terrain-data)
              base-map-resolution)
        (.SetHeights terrain-data 0 0
                     (JaggedToMultidimensional/ConvertFloats
                       (into-array
                         (map float-array
                              (partition (.width texture)
                                         (texture->grayscales texture))))))
        terrain-data)))

(defn get-terrain
  "Takes values for min/max longitude and latitudes. Produces a terrain game object."
  [min-long min-lat max-long max-lat]
  (let [height-map (get-height-map min-long min-lat max-long max-lat)
        terrain-data (texture->terrain-data height-map)]
    (Terrain/CreateTerrainGameObject terrain-data)))

(defcomponent Elevation [^float min-long ^float min-lat ^float max-long ^float max-lat]
  (Start [this]
         (get-terrain min-long min-lat max-long max-lat)))
