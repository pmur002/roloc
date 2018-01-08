
## Given an R colour specification, return a colour name

## There are three possible specs:
## - colour name (from set known by R; see colours()) [character]
## - "#RRGGBB" (or "#RRGGBBAA")
## - numeric (which is index into palette())

col2char <- function(numspec) {
        if (any(numspec < 1))
            stop("Invalid colour specification")
        pal <- palette()
        ## Palette values are recycled
        npal <- length(pal)
        if (any(numspec > npal)) {
            numspec <- (numspec - 1) %% length(pal) + 1
        }
        pal[numspec]
}

## Assume char, but may be colour name not hex colour
col2hex <- function(charspec) {
    hex <- grepl("^#", charspec)
    if (any(!hex)) {
        charspec[!hex] <- rgb(t(col2rgb(charspec[!hex])), maxColorValue=255)
    }
    charspec
}

colourNames <- function(x, ...) {
    UseMethod("colourNames")
}

colNames <- function(distances, colourList) {
    if (all(is.na(distances))) {
        NA
    } else {
        finiteDist <- is.finite(distances)
        if (any(finiteDist)) {
            colourList$names[finiteDist][order(distances[finiteDist])]
        } else {
            "unknown"
        }
    }
}

colourNames.colourMatch <- function(x, ...) {
    distList <- lapply(seq_len(nrow(x$colourDist)),
                       function(i) x$colourDist[i,])
    lapply(distList, colNames, x$colourList)
}

colourNames.default <- function(x,
                                colourList=getOption("roloc.colourList"),
                                colourMetric=getOption("roloc.colourMetric"),
                                ...) {
    colourNames(colourMatch(x, colourList, colourMetric, ...))
}


colorNames <- colourNames

colourName <- function(...) {
    names <- colourNames(...)
    sapply(names, "[[", 1)
}

colorName <- colourName
