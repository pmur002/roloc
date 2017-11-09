\name{colourList}
\alias{colourList}
\alias{colorList}
\title{
  Define a list of colour names
}
\description{
  Define a list of colour names, which includes both colour names
  and a set of sRGB specifications corresponding to the names.
}
\usage{
colourList(names, colours)
colorList(names, colours)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{
    A character vector of colour names.
  }
  \item{colours}{
    An \code{"sRGB"} colour object (which provides the sRGB
    specifications corresponding to the colour names).
  }
}
\value{
  A \code{"colourList"} object.
}
\author{
  Paul Murrell
}
\examples{
colourList("red", colorspace::RGB(1, 0, 0))
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
