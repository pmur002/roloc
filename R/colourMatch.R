
## Calculate colour match information

colourMatch <- function(colour,
                        colourList=getOption("roloc.colourList"),
                        colourMetric=getOption("roloc.colourMetric"),
                        ...) {
    if (!inherits(colourList, "colourList"))
        stop("Invalid colourList")
    ## Convert to character
    if (is.numeric(colour)) {
        colour <- col2char(colour)
    }
    ## Convert "#RRGGBB[AA]" specification to name
    colourRGB <- hex2RGB(col2hex(colour))
    colourDist <- colourMetric(colourRGB, colourList$colours, ...)
    missing <- is.na(colourDist)
    unknown <- !missing & colourDist == Inf
    match <- list(colour=colour, colourList=colourList,
                  colourDist=colourDist)
    class(match) <- "colourMatch"
    match
}

colorMatch <- colourMatch
