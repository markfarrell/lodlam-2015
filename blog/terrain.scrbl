#lang scribble/sigplan

@section{Loading and Displaying Elevation Data from the Shuttle Radar Topography Mission}

We want to stream in high-resolution elevation data and display it as we move around
in our scenes. I managed to obtain a @hyperlink["http://geojson.org/" "GeoJSON"]
file describing the bounding boxes of the chunks of elevation data provided by the
@hyperlink["http://www2.jpl.nasa.gov/srtm/" "Shuttle Radar Topography Mission (SRTM)"]. I'd
like to provide a function to retrieve chunks of elevation data nearest to a point:
the geographical location of the camera in our scenes.

@(require racket/sandbox
          net/url
          scribble/eval)
@(define ev
   (call-with-trusted-sandbox-configuration
     (lambda ()
       (parameterize ([sandbox-output 'string]
                      [sandbox-error-output 'string])
         (make-evaluator 'racket)))))

@interaction-eval[
  #:eval ev
  (begin
    (require racket/pretty)
    (current-print pretty-print-handler))
]

@interaction-eval[
  #:eval ev
  (begin
    (require json)
    (require net/url)
    (require file/unzip))
]

@hyperlink["https://github.com/dwtkns/srtm-tile-grabber/tree/master/data" "Darek Watkins"]
provides a @hyperlink["https://github.com/mbostock/topojson" "TopoJSON"] file describing the
bounding boxes of SRTM chunks. I converted this file back to
@hyperlink["http://jeffpaine.github.io/geojson-topojson/" "GeoJSON"] format to be able to
retrieve chunks of SRTM data nearest to a geographical coordinate. By default, GeoJSON files
describe geographical coordinates, i.e. longitude-latitude pairs, in decimal
degrees; they use the WGS84 datum as well. Let's load the file and extract the names of SRTM chunks,
as well as their bounding boxes.

@interaction-eval[
  #:eval ev
  (define features
    (hash-ref (call-with-input-file "world.json" read-json) 'features))
]

@interaction-eval[
  #:eval ev
  (struct SRTM (file-name x-min x-max y-min y-max) #:transparent)
]

Now, we need to be able to do two things: efficiently find chunks of SRTM data
in our camera's field of view, and retrieve the chunks of SRTM data in a format that
can be displayed as terrain in Unity3d. @hyperlink[
 "https://alastaira.wordpress.com/2013/11/12/importing-dem-terrain-heightmaps-for-unity-using-gdal/"
 "GDAL"
] let's us convert GeoTIFF files into a raster format supported by Unity3d. @italic{But,
how do we efficiently find chunks of SRTM data in our camera's field of view?} Let's consider
an example. According to DBpedia, the @hyperlink[
  "http://dbpedia.org/describe/?url=http%3A%2F%2Fdbpedia.org%2Fresource%2FBattle_of_Vimy_Ridge&sid=115511"
  "Battle of Vimy Ridge"
] took place at (2.774 50.379). If the camera in our scene was placed at this coordinate, we could
certainly load all chunks of terrain within a fixed radius around it; we might optimize this by
then only loading chunks of terrain that is visible to the camera, i.e. unloading chunks
behind the camera. It would be efficient to use a @hyperlink[
  "http://en.wikipedia.org/wiki/Quadtree"
  "QuadTree"
] data structure to find chunks of SRTM data near the camera.

@interaction-eval[
  #:eval ev
  (define SRTMs
    (map (λ (feature)
            (let* ([coordinates (first (hash-ref (hash-ref feature 'geometry) 'coordinates))]
                   [x-values (map first coordinates)]
                   [y-values (map second coordinates)]
                   [x-min (apply min x-values)]
                   [x-max (apply max x-values)]
                   [y-min (apply min y-values)]
                   [y-max (apply max y-values)])
            (SRTM (hash-ref (hash-ref feature 'properties) 'filename)
                  x-min x-max y-min y-max)))
         (filter (λ (feature)
                    (hash-has-key? (hash-ref feature 'properties) 'filename))
                 features)))
]

@interaction-eval[
  #:eval ev
  (define (make-gdal-translate srtm)
    (λ (dir)
       (let ([geotiff (string-append (path->string dir)
                                     "/"
                                    (SRTM-file-name srtm)
                                    ".tif ")])
            (begin
               (system (string-append "cp -f "
                                      geotiff
                                      "data/"))
               (system (string-append "gdal_translate -q -ot UInt16 -scale -of ENVI -outsize 1025 1025 "
                                      "data/"
                                      (SRTM-file-name srtm)
                                      ".tif "
                                      "data/"
                                      (SRTM-file-name srtm)
                                      ".raw"))))))
]

@interaction-eval[
  #:eval ev
  (define (SRTM-download srtm)
    (let* ([url-prefix "http://gis-lab.info/data/srtm-tif/"]
           [url-path (string-append url-prefix (SRTM-file-name srtm) ".zip")]
           [url (string->url url-path)]
           [gdal-translate (make-gdal-translate srtm)])
    (call-with-unzip (get-pure-port url #:redirections 1)
                     gdal-translate)))
]

@interaction-eval[
 #:eval ev
 (SRTM-download (first SRTMs))
]

@interaction-eval[
  #:eval ev
  (define (SRTM-contains? x y)
    (λ (srtm)
       (and (< (SRTM-x-min srtm) x)
            (> (SRTM-x-max srtm) x)
            (< (SRTM-y-min srtm) y)
            (> (SRTM-y-max srtm) y))))
]

@interaction[
  #:eval ev
  (map SRTM-download
       (filter (SRTM-contains? 2.774 50.379)
              SRTMs))
]

