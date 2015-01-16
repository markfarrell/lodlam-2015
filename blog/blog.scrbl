#lang scribble/manual

@title{Muninn Project}

@section{Retrieving Historical Art from the Rijksmuseum}

The @hyperlink["https://www.rijksmuseum.nl/" "Rijksmuseum"] is a national historical art museum
located in Amsterdam. They provide a public API for retrieving art objects in their collection,
which you can read more about @hyperlink["https://www.rijksmuseum.nl/en/api" "here"].
There's close to one million art objects made available by the Rijksmuseum. We'd like to procedurally
generate historical art exhibits with the Rijksmuseum collection.

Let's narrow down our search. As an example, we'll look at art produced between 1845 and 1945.
Also, we're only interested in displaying prints in our exhibits, even though the Rijksmuseum offers historical photos.
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
                       (lambda (port)
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

@verbatim{
  https://www.rijksmuseum.nl/api/en/collection?key=WfTGhlrw&q=&yearfrom=1845&yearto=1945&p=1&ps=1&imgonly=True&type=print&format=json
}

You might be wondering, how many of the Rijkmuseum's art objects were produced between these years?

@(interaction
  #:eval ev
  (rijksmuseum/count 1845 1945))

There is a fair volume of historical art to examine between those years. We'd like
to display samples of this historical art in our exhibits:

@(interaction
  #:eval ev
  (rijksmuseum/sample 1845 1945))

We've been playing with @hyperlink["http://unity3d.com/" "Unity3d"], a game engine; we've made
a picture frame asset that retrieves and displays a sample of historical art when the scene that
it is placed in is played. A repository containing that asset can be found
@hyperlink["https://github.com/markfarrell/muninn" "here"].

@image["unity_rijksmuseum.png" #:scale 0.5]
@image["unity_rijksmuseum_2.png"]

@section{Retrieving Historical Photos of Film Stars using DBpedia}

We can use @hyperlink["http://dbpedia.org/About" "DBpedia"] to retrieve
historical photos of film stars - and display them in procedurally generated
historical exhibits. This @hyperlink[
  "http://www.annefrank.org/en/Museum/Collecties/Movie-star-pictures/"
  "article"
] from Anne Frank's House motivates us to be able to retrieve film star photos
and display them in procedurally generated exhibits of family houses.

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
       (string-append "PREFIX wordnet: <http://www.w3.org/2006/03/wn/wn20/instances/>"
                      "SELECT DISTINCT ?actor ?thumb ?start {"
                      "  {"
                      "    ?actor dbpprop:wordnet_type wordnet:synset-actor-noun-1 ."
                      "  } UNION {"
                      "    ?actor dbpedia-owl:occupation dbpedia:Actor ."
                      "  }"
                      "  ?actor dbpedia-owl:thumbnail ?thumb ."
                      "  ?actor dbpedia-owl:activeYearsStartYear ?start ."
                      "  FILTER (?start > \"" (number->string year-from) "-01-01\"^^xsd:date)"
                      "  FILTER (?start < \"" (number->string year-to) "-01-01\"^^xsd:date)"
                      "  FILTER EXISTS {"
                      "    {"
                      "     ?film dbpedia-owl:starring ?actor ."
                      "    } UNION {"
                      "      ?film dbpprop:starring ?actor ."
                      "    }"
                      "  }"
                      "}"))

    (define (film-stars/actors year-from year-to)
      (hash-ref (hash-ref (get/json (dbpedia/make-url (film-stars/sparql-query year-from year-to)))
                          'results)
                'bindings))

    (define (film-stars/count year-from year-to)
      (length (film-stars/actors year-from year-to)))

    (define (film-stars/display-actor actor-object)
      (above/align "left"
                   (bitmap/url+redirection
                    (hash-ref (hash-ref actor-object 'thumb)
                              'value))
                   (text (hash-ref (hash-ref actor-object 'actor)
                                   'value)
                         12
                         "Black")))

    (define (film-stars/sample year-from year-to)
      (film-stars/display-actor
       (select-random (film-stars/actors year-from year-to))))))

Let's look at film stars who were active while Anne Frank was alive (1929 - 1945).
Here's a @hyperlink[
  "http://www.w3.org/TR/2013/REC-sparql11-query-20130321/SPARQL"
  "SPARQL"
] query to retrieve film stars who were active during that time period:

@verbatim{

  PREFIX wordnet: <http://www.w3.org/2006/03/wn/wn20/instances/>

  SELECT DISTINCT ?actor ?thumb ?start {

    {
      ?actor dbpprop:wordnet_type wordnet:synset-actor-noun-1 .
    } UNION {
      ?actor dbpedia-owl:occupation dbpedia:Actor .
    }

    ?actor dbpedia-owl:thumbnail ?thumb .
    ?actor dbpedia-owl:activeYearsStartYear ?start .

    FILTER (?start > "1900-01-01"^^xsd:date)
    FILTER (?start < "1945-01-01"^^xsd:date)

    FILTER EXISTS {
      {
        ?film dbpedia-owl:starring ?actor .
      } UNION {
        ?film dbpprop:starring ?actor .
      }
    }

  }

}

We can execute that SPARQL query and retrieve the results by sending a @italic{GET}
request to DBpedia's SPARQL endpoint:

@verbatim{
  http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&format=json&timeout=30000&debug=on&query=???
}

@italic{Note: we'd set the query parameter to be our SPARQL query.}

How many film stars were active during that time period?

@interaction[
  #:eval ev
  (film-stars/count 1900 1945)
]

That's quite a number of film stars. Let's have a look at example of a film star
photo that we could place in our procedurally generated historical exhibit:

@interaction[
  #:eval ev
  (film-stars/sample 1900 1945)
]

One problem is that some photos of film stars are modern, and would look
out of place in our exhibits. We could check if the photos are greyscale
before displaying them.

@image{unity_film_stars.png}

