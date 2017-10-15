\name{colourName}
\alias{colourName}
\alias{colorName}
\title{
  Convert a colour specification to a colour name
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colourName(colour, colourList, colourspace)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colour}{
    An R colour specification
  }
  \item{colourList}{
    A list of colours
  }
  \item{colourspace}{
    The name of a colour space
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
