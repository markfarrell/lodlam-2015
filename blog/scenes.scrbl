#lang scribble/manual

@title{Generating Scenes of Historical Events}

The Muninn Project aims to statistically generate scenes of historical events.
@italic{How might we generate these scenes?} We'll start by generating scenes of
house interiors. First, we need to know what objects should be placed in a
scene that we are generating: e.g. there must be rooms in a house, in those
rooms are walls and on the walls are paintings and family photos. Then, once we
decide upon what objects should be in a scene, and the hierarchical structure of
a scene, we must decide upon the spatial relationships between objects: e.g.
the farthest wall in the living room has a painting, and the painting is to
the left of the family photo centered on the wall. Lastly, choose art
assets to represent these objects, e.g. retrieve family photos from DBpedia and
historical art that best fit the context of the scene. On that note: we need to
decide the historical context of a scene before generating, e.g. date and
geographical location.

We'll look at Flickr photos, and determine the possible structures of house
scenes; but before finding examples of house interiors on Flickr, let's
just talk about how we might statistically construct a wall with objects on it.
A wall is rectangular, and we'll arrange objects that have rectangular bounding
boxes on it. We need to know the dimensions of our wall, how many objects we wish
to place on it, and their dimensions. Let's try packing our rectangular objects, and
scale them apart from the center of the wall, to best fit the dimensions of the
larger wall. Following a previous post titled @italic{Retrieving Historical Art from
the Rijksmuseum}: we'll retrieve two samples of historical art from the Rijksmuseum,
pack them, and then illustrate packing four samples, and so on.

@bibliography[
  @bib-entry[
    #:key "1"
    #:title "Semantic Parsing for Text to 3D Scene Generation"
    #:author "Angel X. Chang, Manolis Savva and Christopher D. Manning"
    #:location "Baltimore, Maryland USA"
    #:date "June 26 2014"
    #:url "http://yoavartzi.com/sp14/pub/csm-sp14-2014.pdf"
  ]
  @bib-entry[
    #:key "2"
    #:title "Inducing Ontology from Flickr Tags"
    #:author "Patrick Schmitz"
    #:location "University of California, Berkeley"
    #:date "2006"
    #:url "http://www.academia.edu/3894221/Inducing_Ontology_from_Flickr_Tags"
  ]
  @bib-entry[
    #:key "3"
    #:title "A Game-Theoretic Approach to Generating Spatial Descriptions"
    #:author "Dave Golland, Piercy Liang and Dan Klein"
    #:location "EMNLP"
    #:date "2010"
    #:url "http://www.cs.berkeley.edu/~dsg/papers/pragmatics-emnlp2010.pdf"
  ]
]
