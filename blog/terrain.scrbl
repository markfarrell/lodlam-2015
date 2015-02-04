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
degrees; they use the WGS84 spheroid as well. Let's load the file and extract the names of SRTM chunks,
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
              (system (string-append "gdal_translate -q -ot Byte -of BMP "
                                     " -scale 0 256 "
                                     "-outsize 513 513 "
                                     "data/"
                                     (SRTM-file-name srtm)
                                     ".tif "
                                     "data/"
                                     (SRTM-file-name srtm)
                                     ".bmp"))))))
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
  (define (SRTM-contains? x y)
    (λ (srtm)
       (and (< (SRTM-x-min srtm) x)
            (> (SRTM-x-max srtm) x)
            (< (SRTM-y-min srtm) y)
            (> (SRTM-y-max srtm) y))))
]

@interaction-eval[
  #:eval ev
  (map SRTM-download
       (filter (SRTM-contains? 2.774 50.379)
               SRTMs))
]

Whenever we import terrain into Unity3D, we need to make sure that it is
scaled correctly: i.e. Vimy Ridge should look correctly-scaled when
viewed through the camera in our scene. Our camera has a perspective projection;
it nas near and far clipping planes on the z-axis in Unity3d. Let's define
values for the near and far clipping planes of our camera:

@interaction[
  #:eval ev
  (define-values (near far)
    (values 0.3 1000))
]

These values are defined with respect to Unity3d's gridded coordinate system,
the origin of its z-axis having the value 0; this is not the same coordinate
system as WGS84 and geographical coordinates in decimal degrees. We'll
have to project our elevation data to fit Unity3d's coordinate system, before
we fix its scale and orientation relative to the size of our camera's view.
The @hyperlink[
  "http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system"
  "Universal Transverse Mercator coordinate system"
] let's us do just that: project from geographical coordinates, i.e. longitude
-latitude coordinates, to two-dimensional Cartesian coordinates. It's possible to
use GDAL to do the projection, but we could just do the math ourselves. If we want
to setup a service that let's us request elevation data for any bounding box, not
just predefined SRTM chunks, then we should probably do the projection ourselves
after retrieving elevation data inside our bounding box.

@section{A Service for Retrieving Elevation Data in a Bounding Box}

As mentioned, we'd like to have a service to retrieve the elevation data inside
a bounding box. Not only do we want to retrieve elevation data inside a bounding box,
but we also want to scale, rotate and project it so that our terrain in Unity3d looks
the same as it would in real life. @italic{Ok}, so SRTM chunks are
6000 by 6000 pixels wide, where each point is 90 metres apart. Ideally, we want elevation
data that is accurate within one metre; this happens to be the minimum width of a
trench. However, SRTM is the best we have right now, but we'll scale it to 90 times
its original resolution, so that elevation data that is returned from a request
appears to be to scale.

@section{Testing that Chunks of Elevation Data are in our Camera's Field of View}

We can use @hyperlink["http://en.wikipedia.org/wiki/Vincenty%27s_formulae" "Vincenty's Formula"] to
compute the ellipsoidal distance between two geographical coordinates on the Earth, the WGS84 spheroid,
as well as their azimuths; our azimuths are the forward horizontal angle between a geographical coordinate
and @hyperlink["http://en.wikipedia.org/wiki/True_north" "true north"]. Distances are measured in metres,
and all angles are measured in degrees. Let's measure the distance and compute the azimuths for
an example pair of coordinates:

@interaction-eval[
  #:eval ev
  (begin
    (define a 6378137.0)
    (define ƒ (/ 1 298.257223563))
    (define b 6356752.314245))
]

@interaction-eval[
  #:eval ev
  (define (reduced-latitude φ)
    (atan (* (- 1 ƒ)
             (tan φ))))
]

@hyperlink[
  "http://dbpedia.org/page/Battle_of_Vimy_Ridge"
  "The Battle of Vimy Ridge"
]:

@interaction[
  #:eval ev
  (define-values (φ1 L1)
    (values 50.379 2.774))
]

@hyperlink[
  "http://dbpedia.org/page/Battle_of_the_Somme"
  "The Battle of the Somme"
]:

@interaction[
  #:eval ev
  (define-values (φ2 L2)
    (values 50.016666 2.683333))
]

@interaction-eval[
  #:eval ev
  (define L (degrees->radians (- L1 L2)))
]

@interaction-eval[
  #:eval ev
  (define precision 0.000000000001)
]

@interaction-eval[
  #:eval ev
  (define (λ-prime λ)
    (let* ([U1 (reduced-latitude φ1)]
           [U2 (reduced-latitude φ2)]
           [sinσ (sqrt (+ (expt (* (cos U2)
                                   (sin λ))
                                2)
                          (expt (- (* (cos U1)
                                      (sin U2))
                                   (* (sin U1)
                                      (cos U2)
                                      (cos λ)))
                                2)))]
            [cosσ (+ (* (sin U1)
                        (sin U2))
                     (* (cos U1)
                        (cos U2)
                        (cos λ)))]
            [σ (atan (/ sinσ cosσ))]
            [sinα (/ (* (cos U1)
                        (cos U2)
                        (sin λ))
                     sinσ)]
            [cosα2 (- 1
                      (expt sinα 2))]
            [cos2σm (- cosσ
                       (/ (* 2
                             (sin U1)
                             (sin U2))
                           cosα2))]
            [C (* (/ ƒ 16)
                  cosα2
                  (+ 4
                     (* ƒ
                        (- 4
                           (* 3 cosα2)))))])
          (+ λ
             (* (- 1 C)
                ƒ
                sinα
                (+ σ
                   (* C
                      sinσ
                      (+ cos2σm
                         (* C
                            cosσ
                            (+ -1
                               (* 2
                                  (expt cos2σm 2)))))))))))
]

@interaction-eval[
  #:eval ev
  (begin
    (require racket/generator)
    (define stop-value (gensym))
    (define λ-generator
      (generator ()
        (let loop
             ([diff precision]
              [λ L])
             (if (< diff precision)
                 stop-value
                 (let ([λ-prime (λ-prime λ)])
                      (begin
                        (yield λ-prime)
                        (loop  (- λ-prime λ)
                               λ-prime))))))))

]

@interaction-eval[
  #:eval ev
  (define (vincenty-inverse)
    (let* ([λ (for/last ([i (in-producer λ-generator stop-value)]) i)]
           [U1 (reduced-latitude (degrees->radians φ1))]
           [U2 (reduced-latitude (degrees->radians φ2))]
           [sinσ (sqrt (+ (expt (* (cos U2)
                                   (sin λ))
                                2)
                          (expt (- (* (cos U1)
                                      (sin U2))
                                   (* (sin U1)
                                      (cos U2)
                                      (cos λ)))
                                   2)))]
           [cosσ (+ (* (sin U1)
                       (sin U2))
                    (* (cos U1)
                       (cos U2)
                       (cos λ)))]
           [σ (atan (/ sinσ cosσ))]
           [sinα (/ (* (cos U1)
                       (cos U2)
                       (sin λ))
                    sinσ)]
           [cosα2 (- 1
                     (expt sinα 2))]
           [cos2σm (- cosσ
                      (/ (* 2
                            (sin U1)
                            (sin U2))
                            cosα2))]
           [u2 (* cosα2
                  (/ (- (expt a 2)
                        (expt b 2))
                        (expt b 2)))]
           [A (+ 1 (* (/ u2 16384)
                      (+ 4096
                         (* u2
                            (+ -768
                               (* u2
                                  (- 320
                                     (* 175 u2))))))))]
           [B (* (/ u2 1024)
                 (+ 256
                  (* u2
                     (+ -128
                        (* u2
                           (- 74
                              (* 47 u2)))))))]
           [Δσ (* B
                  sinσ
                  (+ cos2σm
                     (* (/ 1 4)
                        B
                        (- (* cosσ
                              (+ -1
                                 (* 2
                                    (expt cos2σm 2))))
                           (* (/ 1 6)
                              B
                              cos2σm
                              (+ -3
                                 (* 4
                                    (expt sinσ 2)))
                              (+ -3
                                 (* 4
                                    (expt cos2σm 2))))))))]
           [s (* b
                 A
                 (- σ Δσ))]
           [α1 (atan (/ (* (cos U2)
                           (sin λ))
                        (- (* (cos U1)
                              (sin U2))
                           (* (sin U1)
                              (cos U2)
                              (cos λ)))))]
           [α2 (atan (/ (* (cos U1)
                           (sin λ))
                        (+ (* -1
                              (sin U1)
                              (cos U2))
                           (* (cos U1)
                              (sin U2)
                              (cos λ)))))])
          (values s
                  (radians->degrees α1)
                  (radians->degrees α2))))
]

@interaction[
  #:eval ev
  (vincenty-inverse)
]

