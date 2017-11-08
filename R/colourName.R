
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

colourNames <- function(colour,
                        colourList=getOption("roloc.colourList"),
                        colourMetric=getOption("roloc.colourMetric"),
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
        missing <- sapply(colourIndex, is.na)
        unknown <- !missing & sapply(colourIndex,
                                     function(x) { length(x) == 1 && x < 1 })
        colour[!alreadyName & missing] <- NA
        colour[!alreadyName & unknown] <- "unknown"
        colours <- as.list(colour)
        colours[!alreadyName & !(missing | unknown)] <-
            lapply(colourIndex[!(missing | unknown)],
                   function(i) { colourList$names[i] })
    } else {
        colours <- as.list(colour)
    }
    colours
}

colorNames <- colourNames

colourName <- function(...) {
    nameList <- colourNames(...)
    sapply(nameList, "[[", 1)
}

colorName <- colourName
