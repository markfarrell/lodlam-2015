#lang scribble/manual

@title{Muninn Project}

@section{Retrieving Historical Art from the Rijksmuseum}

The Muninn Project aims to statistically recreate scenes of historical events using
@hyperlink["http://lod-cloud.net/" "Linked Open Data"]. Historical art is rich with
information important to the study of politics and human culture - but there is so
much historical art to examine that it becomes difficult to devote sufficient attention
to each individual piece of art. @italic{So, how might we resolve this problem of "information
overload"?} If we statistically recreate scenes of historical events, and retrieve relevant
art to display in them, we argue that analysis of the art becomes easier with the additional
historical context provided by the scene. @italic{Let's try this.}

The @hyperlink["https://www.rijksmuseum.nl/" "Rijksmuseum"] is a national historical art museum
located in Amsterdam. They provide a public API for retrieving art objects in their collection,
which you can read more about @hyperlink["https://www.rijksmuseum.nl/en/api" "here"].
There's close to one million art objects made available by the Rijksmuseum. We can narrow down
our search for historical art: we'll try to sample only the art that best fits the context
of our recreated historical scenes.

As an example, we'll retrieve art from the Rijksmuseum produced between 1845 and 1945 to display
in a scene of a historical event that took place during that time period. We could send a @italic{GET}
request with the following URL to retrieve the first piece of art that satisfies this constraint:

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

@italic{Note that:} we've also restricted the type of art that we retrieve; we're only retrieving prints, even though
the Rijksmuseum also provides photos. We'll begin to look at historical photos to display in our scenes in a future blog post.
The Rijksmuseum may or may not be the best source for photos, though they certainly provide very high quality images of prints
that are important to Dutch history. Calls to the Rijksmuseum API return a @hyperlink["http://www.json.org/" "JSON"] object, listing
the titles of art that match our search parameters, and links to images of them.

@italic{Let's look at the art objects that match our search parameters.}
You might be wondering, how many of the Rijkmuseum's art objects were produced between these years?

@(interaction
  #:eval ev
  (rijksmuseum/count 1845 1945))

There is a fair volume of historical art to examine between those years. @italic{So, what might
a sample of that art look like?}

@(interaction
  #:eval ev
  (rijksmuseum/sample 1845 1945))

This sample of historical art is of Japanese origin. Most of the Rijkmuseum art is not of East Asian origin,
though they keep a small collection. We might wish to restrict the art we sample by the geographical location
that our historical scenes take place in. This is possible with the Rijksmuseum API, but only approximately, by
modern or historic country.

We've been playing with @hyperlink["http://unity3d.com/" "Unity3d"], a game engine; we've made
a picture frame asset that retrieves and displays a sample of historical art when the scene that
it is placed in is played. A repository containing that asset can be found
@hyperlink["https://github.com/markfarrell/muninn" "here"]. The script that retrieves art from the
Rijksmuseum was written in @hyperlink["http://clojure.org/" "Clojure"], with the new
@hyperlink["http://arcadia-unity.tumblr.com/" "Arcadia"] plugin; we hope to explore how the
functional programming paradigm might help us further progress on the Muninn project.

@image["unity_rijksmuseum.png" #:scale 0.5]
@image["unity_rijksmuseum_2.png"]

We hope to retrieve art from the Rijksmuseum and place it in our statistically recreated scenes of historical events.
The Unity3d asset that we have created facilitates this: we need to make more progress on statistically building the structure
of our scenes, but we are now able to populate picture frames with art in our scenes once we have determined where they
should be placed. It would be nice to display a description of the art to the viewer, to aid the analysis of the art
in the context of the scene. Stay tuned for more information on this endeavour!

@section{Retrieving Historical Photos of Film Stars using DBpedia}

This is a follow-up to the previous blog post on retrieving historical art from the Rijksmuseum.
Like historical art, film star photos inform us about politics and human culture at particular
times throughout history. We can use @hyperlink["http://dbpedia.org/About" "DBpedia"] to retrieve
historical photos of film stars and display them in our statistically generated scenes of historical events.

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

As an example, let's look at film stars who were active while Anne Frank was alive (1929 - 1945).
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

There are a couple of problems with retrieving film star photos usings DBpedia.

One problem is that some photos of film stars are modern, and would look out of place in our exhibits.
We could check if the photos are greyscale before displaying them.

Another problem is that there isn't information available about when some film stars were active,
e.g. @hyperlink["http://dbpedia.org/page/Sonja_Henie" "Sonja Henie"] as mentioned in this
@hyperlink["http://www.annefrank.org/en/Museum/Collecties/Movie-star-pictures/" "article"]
from the Anne Frank House; this means that they are
not included in the result of our SPARQL query. It might be possible to use the release dates of
the films that actors/actresses starred in as a proxy for information on when they were active.

@image{unity_film_stars.png}

We've made a Unity3d picture frame asset that retrieves a random film star photo from DBpedia
and displays it when the scene that it is placed in is played. The asset is available in this
repository. Stay tuned for more on statistically generated historical scenes!
