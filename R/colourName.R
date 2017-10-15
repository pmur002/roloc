
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
        charspec[!hex] <- rgb(t(col2rgb(charspec[!hex])), max=255)
    }
    charspec
}

## 'colourspace' must be one that the 'colorspace' package knows about
colourName <- function(colour,
                       colourList=Rcolours,
                       colourMetric=euclideanLUV,
                       ...) {
    if (!inherits(colourList, "colourList"))
        stop("Invalid colourList")
    ## Convert to character
    if (is.numeric(colour)) {
        colour <- col2char(colour)
    }
    ## If the colour specification is already a name in the 'colourList',
    ## just return that
    alreadyName <- colour %in% colourList$names
    if (any(!alreadyName)) {
        colourSpec <- colour[!alreadyName]
        ## Otherwise, convert "#RRGGBB[AA]" specification to name
        colourRGB <- hex2RGB(col2hex(colourSpec))
        colourIndex <- colourMetric(colourRGB, colourList$colours, ...)
        missing <- is.na(colourIndex)
        unknown <- !missing & colourIndex < 1
        colour[!alreadyName & missing] <- NA
        colour[!alreadyName & unknown] <- "unknown"
        colour[!alreadyName & !(missing | unknown)] <-
            colourList$names[colourIndex[!(missing | unknown)]]
    }
    colour
}

colorName <- colourName
