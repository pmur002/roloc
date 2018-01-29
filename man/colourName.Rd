\name{colourName}
\alias{colourName}
\alias{colorName}
\alias{colourNames}
\alias{colourNames.default}
\alias{colourNames.colourMatch}
\alias{colorNames}
\title{
  Convert a colour specification to a colour name
}
\description{
  Return a colour name from a colour specification.
}
\usage{
\method{colourNames}{colourMatch}(x,
                             \dots)
\method{colourNames}{default}(x,
                             colourList=getOption("roloc.colourList"),
                             colourMetric=getOption("roloc.colourMetric"),
                             \dots)
colourName(\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A \code{"colourMatch"} object or an R colour specification
  }
  \item{colourList}{
    A list of colours
  }
  \item{colourMetric}{
    A colour metric function.  See \code{\link{colourMatch}} for details.
  }
  \item{\dots}{
    For \code{colourNames},
    arguments passed to the colour metric function.
    For \code{colourName},
    arguments passed to \code{colourNames}
  }
}
\value{
  \code{colourNames} returns a list of colour names (possibly more than one
  per colour specification).
  \code{colourName} returns a vector of colour names (only the first
  match per colour specification).
}
\author{
  Paul Murrell
}
\examples{
# Some colours() are identical to each other
newColours <- colourName(rgb(t(col2rgb(colours())), maxColorValue=255))
repeats <- newColours != colours()
cbind(colours()[repeats], newColours[repeats])
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
