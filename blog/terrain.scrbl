#lang scribble/sigplan

@section{Loading and Displaying Elevation Data from the Shuttle Radar Topography Mission}

We want to stream in high-resolution elevation data and display it as we move around
in our scenes. I managed to obtain a GeoJSON file describing the bounding boxes of
the chunks of elevation data provided by the Shuttle Radar Topography Mission. I'd
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

@interaction[
  #:eval ev
  (define features
    (hash-ref (call-with-input-file "world.json" read-json) 'features))
]

@interaction[
  #:eval ev
  (struct SRTM (filename coordinates) #:transparent)
]

@interaction[
  #:eval ev
  (define SRTMs
    (map (λ (feature)
            (SRTM (hash-ref (hash-ref feature 'properties) 'filename)
                  (hash-ref (hash-ref feature 'geometry) 'coordinates)))
         (filter (λ (feature)
                    (hash-has-key? (hash-ref feature 'properties) 'filename))
                 features)))
]

@interaction[
  #:eval ev
  (define (make-gdal-translate srtm)
    (λ (dir)
       (system (string-append "gdal_translate -ot UInt16 -scale -of ENVI -outsize 1025 1025 "
                      (path->string dir)
                      "/"
                      (SRTM-filename srtm)
                      ".tif "
                      "data/"
                      (SRTM-filename srtm)
                      ".raw"))))
]

@interaction[
  #:eval ev
  (define (SRTM-download srtm)
    (let* ([url-prefix "http://gis-lab.info/data/srtm-tif/"]
           [url-path (string-append url-prefix (SRTM-filename srtm) ".zip")]
           [url (string->url url-path)]
           [gdal-translate (make-gdal-translate srtm)])
    (call-with-unzip (get-pure-port url #:redirections 1)
                     gdal-translate)))
]

@interaction[
  #:eval ev
  (map SRTM-download SRTMs)
]

