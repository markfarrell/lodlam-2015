#lang scribble/manual

@title{Muninn Project}

This post covers: retrieving art from the Rijksmuseum archives, retrieving photos
of film stars from DBpedia & retrieving family photos from DBpedia as well. 

Install @hyperlink["http://unity3d.com/" "Unity3d"]. 
Install @hyperlink["http://arcadia-unity.tumblr.com/" "Arcadia"].

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
    (require racket/draw)
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
    
    (define (get/json str)
      (call/input-url (string->url str)
                      get-pure-port
                      read-json))
    
    (define (bitmap/url+redirection string)
      ;; the rotate does a coercion to a 2htdp/image image
      (rotate
       0
       (call/input-url (string->url string)
                       (lambda (u [h '()]) (get-pure-port u h #:redirections 5))
                       (λ (port)
                         (make-object bitmap% port 'unknown/alpha #f #t)))))
                              
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
      (get/json (rijksmuseum/make-url year-from year-to results-per-page page)))
    
    (define (rijksmuseum/search/display-art-object art-object)
      (above/align "left"
                   (scale img-scale
                          (bitmap/url (hash-ref (hash-ref art-object 'webImage)
                                                'url)))
                   (text (hash-ref art-object 'longTitle)
                         12 
                         "Black")))

    (define results-per-page 5)
    (define page 1)
    
    (define (rijksmuseum/sample year-from year-to)
      (rijksmuseum/search/display-art-object
       (select-random (hash-ref (rijksmuseum/search/json year-from year-to results-per-page page)
                                'artObjects))))))

@(interaction-eval 
  #:eval ev
  (begin
    (require racket/pretty)
    (current-print pretty-print-handler)))

@(interaction
  #:eval ev     
  (rijksmuseum/sample 1850 1945))

@section{Film Stars}

@(interaction-eval
  #:eval ev
  (begin 
    (define dbpedia/sparql-url "http://dbpedia.org/sparql")
    (define dbpedia/sparql-default-graph-uri "http://dbpedia.org")
    (define dbpedia/sparql-format "json")
    (define dbpedia/sparql-timeout 30000)
    (define dbpedia/sparql-debug "on")
    
    (define (dbpedia/make-url query)
      (string-append dbpedia/sparql-url
                     "?"
                     "default-graph-uri="
                     (uri-encode dbpedia/sparql-default-graph-uri)
                     "&query="
                     (uri-encode query)
                     "&format="
                     (uri-encode dbpedia/sparql-format)
                     "&timeout="
                     (number->string dbpedia/sparql-timeout)
                     "&debug="
                     dbpedia/sparql-debug))))

@(interaction-eval
  #:eval ev 
  (begin
    
    (define (film-stars/sparql-query year-from year-to)
      (string-append "select ?actor ?name ?thumb ?start ?end ?active {"
                     "?actor dbpedia-owl:occupation dbpedia:Actor ."
                     "?actor foaf:name ?name ."
                     "?actor dbpedia-owl:thumbnail ?thumb ."
                     "?actor dbpedia-owl:activeYearsStartYear ?start ."
                     "?actor dbpedia-owl:activeYearsEndYear ?end ."
                     "?actor dbpprop:yearsActive ?active ."
                     "FILTER (?start >= \""
                     (number->string year-from)
                     "-01-01\"^^xsd:date)"
                     "FILTER (?end <= \""
                     (number->string year-to)
                     "-01-01\"^^xsd:date)"
                     "FILTER (?active <= \""
                     (number->string year-to)
                     "\"^^xsd:integer)"
                     "}"))
    
    (define (film-stars/actors year-from year-to)
      (hash-ref (hash-ref (get/json (dbpedia/make-url (film-stars/sparql-query year-from year-to)))
                          'results)
                'bindings))
    
    (define (film-stars/display-actor actor-object)
      (above/align "left"
                   (bitmap/url+redirection 
                    (hash-ref (hash-ref actor-object 'thumb)
                              'value))                
                   (text (hash-ref (hash-ref actor-object 'name)
                                   'value)
                         12
                         "Black")
                   (text (string-append "Active "
                                        (hash-ref (hash-ref actor-object 'active)
                                                  'value))
                         12
                         "Black")))
    
    (define (film-stars/sample year-from year-to)
      (film-stars/display-actor
       (select-random (film-stars/actors year-from year-to))))))

@interaction[
  #:eval ev
  (film-stars/sample 1929 1945)
]

@section{Family Photos}

@section{Flickr}

