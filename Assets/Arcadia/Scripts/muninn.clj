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

(defn grid-width
  [grid-instance]
  (let [xs (map (fn [[x z]] x)
                grid-instance)]
    (inc (- (apply max xs)
            (apply min xs)))))

(defn grid-length
  [grid-instance]
  (let [zs (map (fn [[x z]] z)
                grid-instance)]
    (inc (- (apply max zs)
            (apply min zs)))))

(defn grid-partitions
  [grid-width grid-length partition-width partition-length]
  (let [x-partitions (partition-all partition-width
                                    (range grid-width))
        z-partitions (partition-all partition-length
                                    (range grid-length))]
    (mapcat (fn [z-partition]
              (map (fn [x-partition]
                     (mapcat (fn [j]
                               (map (fn [i] [i j])
                                    x-partition))
                             z-partition))
                   x-partitions))
            z-partitions)))

(defn texture-partition
  [texture grid]
  (let [width (grid-width grid)
        length (grid-length grid)
        new-texture (UnityEngine.Texture2D. width length)]
    (do (.SetPixels new-texture
                    (into-array (map (fn [[x z]]
                                       (.GetPixel texture x z))
                                     grid)))
        (.Apply new-texture false)
        new-texture)))

(defn expand-grid
  [original-grid]
  (let [width (grid-width original-grid)
        length (grid-length original-grid)
        min-x (apply min (map (fn [[x z]] x) original-grid))
        min-z (apply min (map (fn [[x z]] z) original-grid))]
    (map (fn [[x z]]
           [(+ min-x x)
            (+ min-z z)])
         (grid (inc width)
               (inc length)))))

(defn crop-grid
  [grid min-x min-z max-x max-z]
  (filter (fn [[x z]]
            (and (>= x min-x)
                 (>= z min-z)
                 (<= x max-x)
                 (<= z max-z)))
          grid))

(defn texture-partitions
  [texture partition-width partition-length]
  (map (fn [grid]
         (texture-partition texture grid))
       (grid-partitions (.width texture)
                        (.height texture)
                        partition-width
                        partition-length)))

(defn grid-partitions-positions
  [grid-width grid-length partition-width partition-length]
  (map
    (fn [grid-partition]
      (let [xs (map (fn [[x z]] x) grid-partition)
            zs (map (fn [[x z]] z) grid-partition)]
        [(max (dec (apply min xs)) 0)
         (max (dec (apply min zs)) 0)]))
    (grid-partitions grid-width grid-length partition-width partition-length)))

(defn set-main-texture!
  "Expects a GameObject; sets main texture."
  [game-object texture]
  (set! (. (. (. game-object renderer) material) mainTexture)
        texture))
