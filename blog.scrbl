#lang scribble/manual

@title{Muninn Project}

@section{Rijksmuseum}

@(require racket/sandbox
          net/url
          scribble/eval)
@(define ev
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket)))))

@(interaction-eval 
  #:eval ev
  (begin 
    (require json)
    (require net/url)
    (require net/uri-codec)
    (require racket/pretty)
    (require 2htdp/image)
     
    (define api-key "WfTGhlrw")
    (define img-only "true")
    (define query "")
    
    (define img-scale 0.1)
    
    ;; http://stackoverflow.com/questions/4583224
    (define select-random
      (lambda (ls)
        (let ((len (length ls)))
          (list-ref ls (random len)))))
                              
    (define (rijksmuseum/make-url year-from year-to results-per-page page)
      (string-append "https://www.rijksmuseum.nl/api/en/collection"
                     "?key="
                     api-key
                     "&q="
                     (uri-encode query)
                     "&yearfrom="
                     (number->string year-from)
                     "&yearto="
                     (number->string year-to)
                     "&p="
                     (number->string page)
                     "&ps="
                     (number->string results-per-page)
                     "&imgonly="
                     img-only
                     "&format=json"))
  
    (define (rijksmuseum/search/json year-from year-to results-per-page page)
      (call/input-url (string->url (rijksmuseum/make-url year-from year-to results-per-page page))
                      get-pure-port
                      read-json))

    (define (rijksmuseum/search year-from year-to results-per-page page)
      (apply (curry above/align "left")
             (map (lambda (artObject)
                    (beside (scale img-scale
                                   (bitmap/url (hash-ref (hash-ref artObject 'webImage)
                                                         'url)))
                            (text (hash-ref artObject 'longTitle)
                                  12 
                                  "Black")))
                  (hash-ref (rijksmuseum/search/json year-from year-to results-per-page page)
                            'artObjects))))))

@(interaction-eval 
  #:eval ev
  (begin
    (require racket/pretty)
    (current-print pretty-print-handler)))

@(interaction
  #:eval ev     
  (rijksmuseum/search 1850 1945 5 1))

@section{Film Stars}
@section{Family Photos}

