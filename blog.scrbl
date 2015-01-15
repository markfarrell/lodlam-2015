#lang scribble/manual

@title{Muninn Project}

@section{Rijksmuseum}

The @hyperlink["https://www.rijksmuseum.nl/" "Rijksmuseum"] is a national historical art museum
located in Amsterdam. They provide a public API for retrieving art objects in their collection,
which you can read more about @hyperlink["https://www.rijksmuseum.nl/en/api" "here"].
There's close to one million art objects made available by the Rijksmuseum. We'd like to procedurally
generate historical art exhibits with the Rijksmuseum collection.

Let's narrow down our search. As an example, we'll look at art produced between 1845 and 1945.
Also, we're only interested in displaying prints in our scenes, even though the Rijksmuseum offers historical photos.
We could send a @italic{GET} request with the following URL to retrieve the first piece of art that satisfies
these constraints:

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
    (define img-only "True")
    (define art-type "print")
    (define results-per-page 1)
    (define page 1)
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
                     "&type="
                     art-type
                     "&format=json"))

    (define (rijksmuseum/search/json year-from year-to page)
      (get/json (rijksmuseum/make-url year-from year-to results-per-page page)))

    (define (rijksmuseum/search/display-art-object art-object)
      (freeze (above/align "left"
                           (scale img-scale
                                  (bitmap/url (hash-ref (hash-ref art-object 'webImage)
                                                        'url)))
                           (text (hash-ref art-object 'longTitle)
                                 12
                                 "Black"))))

    (define (rijksmuseum/count year-from year-to)
      (hash-ref (rijksmuseum/search/json year-from year-to page)
                'count))

    (define (rijksmuseum/art year-from year-to page)
      (hash-ref (rijksmuseum/search/json year-from year-to page)
                'artObjects))

    (define (rijksmuseum/sample year-from year-to)
      (let ([page (random (rijksmuseum/count year-from year-to))])
        (rijksmuseum/search/display-art-object
          (first (rijksmuseum/art year-from year-to page)))))))

@(interaction-eval
  #:eval ev
  (begin
    (require racket/pretty)
    (current-print pretty-print-handler)))

@(interaction
  #:eval ev
  (rijksmuseum/make-url 1845 1945 1 1))

You might be wondering, how many of the Rijkmuseum's art objects were produced between these years?

@(interaction
  #:eval ev
  (rijksmuseum/count 1845 1945))

There is a fair volume of historical art to examine between those years. We'd like
to show samples of this historical art when the scene is played:

@(interaction
  #:eval ev
  (rijksmuseum/sample 1845 1945))

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

