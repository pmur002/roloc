\name{colourMatch}
\alias{colourMatch}
\alias{colorMatch}
\title{
  Match Colour Specifications to Colour Names
}
\description{
  Compare colour specifications to a list of colour names
  and return distance information for each comparison.
}
\usage{
colourMatch(colour, colourList, colourMetric, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colour}{
    An R colour specification
  }
  \item{colourList}{
    A list of colours
  }
  \item{colourMetric}{
    A colour metric function.  See the Details Section.
  }
  \item{\dots}{
    Arguments passed to the colour metric function.
  }
}
\details{
  The colour metric function must take at least two arguments,
    both of which are \code{"RGB"} colour objects (as created by
    funcions in
    the \pkg{colorspace} package such as \code{RGB}):
    \code{colour}, which is the colour specification to find a match for;
    and \code{colourList}, which represents the list of colour names to
    find a match within.
    Any additional arguments must have default values (so that they
    do not HAVE to be specified in a call to \code{colourName}).
    The colour metric function must return either a vector or
    a list, the same length as the \code{colour} argument, containing
    integer indices into the colour list.
    \code{NA} means the colour could not be represented in the
    colour space used by the colour metric.
    \code{"unknown"} means that the colour 
}
\value{
  A \code{"colourMatch"} object with components for the original
  colour specification, the original colour list object, and a
  matric of distances between each colour specification and each
  colour name in the colour list.
}
\seealso{
  This function underlies both \code{\link{colourName}} and
  \code{\link{colourSwatch}}.
}
\author{
  Paul Murrell
}
\examples{
colourMatch("red", colourList=HTMLcolours)
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
