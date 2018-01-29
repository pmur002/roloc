\name{colourSwatch}
\alias{colourSwatch}
\alias{colourSwatch.default}
\alias{colourSwatch.colourMatch}
\alias{colorSwatch}
\alias{colourSwatches}
\alias{colourSwatches.default}
\alias{colourSwatches.colourMatch}
\alias{colorSwatches}
\title{
  Colour Conversion Diagram
}
\description{
  Draw a diagram of a colour conversion with four columns:
  original colour specification, plus swatch, matching colour name, plus
  swatch.  \code{colourSwatches} can show multiple matches per colour
  specification.
}
\usage{
colourSwatch(x, \dots, newpage = TRUE)
\method{colourSwatch}{colourMatch}(x, \dots, newpage = TRUE)
\method{colourSwatch}{default}(x,
                               colourList=getOption("roloc.colourList"),
                               colourMetric=getOption("roloc.colourMetric"),
                               \dots, newpage = TRUE)
colourSwatches(x, \dots, newpage = TRUE)
\method{colourSwatches}{colourMatch}(x, \dots, newpage = TRUE)
\method{colourSwatches}{default}(x,
                               colourList=getOption("roloc.colourList"),
                               colourMetric=getOption("roloc.colourMetric"),
                               \dots, newpage = TRUE)
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
    Arguments passed to the colour metric function.
  }
  \item{newpage}{Whether to start a new page on the current graphics device.}
}
\value{
  These functions are used for their side-effect (a colour match diagram).
}
\author{
  Paul Murrell
}
\examples{
colourSwatch("red")
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
