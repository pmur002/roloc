\name{NBScolours}
\docType{data}
\alias{NBScolours}
\alias{NBScolors}
\title{
  ISCC-NBS colour dictionary
}
\description{
  The ISCC-NBS list of colour names 
}
\usage{
NBScolours
}
\format{ A \code{"colourList"} object. }
\source{
  http://people.csail.mit.edu/jaffer/Color/NBS-ISCC-rgb.txt
}
\warning{
  Some of the sRGB centroids are approximations because some ISCC-NBS
  colours cannot be represented within the sRGB gamut.
  This means that some ISCC-NBS colour names will not \dQuote{round
    trip}; the sRGB centroid will not return the ISCC-NBS colour
  name.
}
\references{
  http://tx4.us/nbs-iscc.htm
  http://www.munsellcolourscienceforpainters.com/ColourSciencePapers/sRGBCentroidsForTheISCCNBSColourSystem.pdf
}
\keyword{ datasets }% use one of  RShowDoc("KEYWORDS")
