\name{euclideanLUV}
\alias{euclideanLUV}
\alias{euclideanRGB}
\alias{euclideanDistance}
\title{
  Euclidean colour metrics
}
\description{
  Colour metrics that calculate euclidean distance between colours
  in different colour spaces.
}
\usage{
euclideanLUV(colour, colourList, tolerance = Inf)
euclideanRGB(colour, colourList, tolerance = Inf)
euclideanDistance(spec, list, tolerance) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colour}{
    An \code{"sRGB"} colour object representing colours to find a match for.
  }
  \item{colourList}{
    An \code{"sRGB"} colour object representing a list of colours to
    find a match within.
  }
  \item{tolerance}{
    A numeric value indicating a range beyond which matches will not
    be accepted.
  }
  \item{spec}{
    A single colour specification, as an RGB vector of three values.
  }
  \item{list}{
    A list of colours, as an RGB matrix with three columns.
  }
}
\details{
  \code{euclideanLUV()} calculates euclidean distance in the CIE Luv
  colour space.

  \code{euclideanRGB()} calculates euclidean distance in the sRGB
  colour space.

  \code{euclideanDistance()} is provided as a helper for defining
  euclidean metrics in other colour spaces.
}
\value{
  The colour metrics return either a vector or a list of integer
  indices into the colour list.
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
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
