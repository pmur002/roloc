\name{colourName}
\alias{colourName}
\alias{colorName}
\alias{colourNames}
\alias{colorNames}
\title{
  Convert a colour specification to a colour name
}
\description{
  Return a colour name from a colour specification.
}
\usage{
colourNames(colour, colourList, colourMetric, ...)
colourName(...)
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
    For \code{colourNames},
    arguments passed to the colour metric function.
    For \code{colourName},
    arguments passed to \code{colourNames}
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
  \code{colourNames} returns a list of colour names (possibly more than one
  per colour specification).
  \code{colourName} returns a vector of colour names (only the first
  match per colour specification).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Paul Murrell
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Some colours() are identical to each other
newColours <- colourName(rgb(t(col2rgb(colours())), maxColorValue=255))
repeats <- newColours != colours()
cbind(colours()[repeats], newColours[repeats])
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
