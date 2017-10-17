
## Attempt to generate sensible set of *unique* colour names
## for a colour palette

## Differences less than "deltaTol" are treated as zero
deltaTol <- 5

runs <- function(dx) {
    delta <- ifelse(dx < -deltaTol, -1, ifelse(dx > deltaTol, 1, 0))
    ## Treat NA values as no change
    delta[is.na(delta)] <- 0
    rle(delta)
}

## Assumes both a and b within [0, 360]
angleDiff <- function(x) {
    ((x[-1] - x[-length(x)]) + 180) %% 360 -180
}

## Automatically drops NAs (because of sort())
angleRange <- function(x) {
    ## Sort first
    sx <- sort(x)
    if (length(sx)) {
        ## Find differences
        dx <- c(diff(sx), sx[1] + 360 - sx[length(sx)])
        ## 360 - largest difference
        360 - max(dx)
    } else {
        ## All angles missing
        0
    }
}

monoH <- function(colours) {
    colourName(colours, basicColours)
}

## Component i provides i adjectives
lnames <- list("",
               c("darker", "lighter"),
               c("darker", "medium", "lighter"),
               c("darkest", "darker", "lighter", "lightest"),
               c("darkest", "darker", "medium", "lighter", "lightest"))
cnames <- list("",
               c("brighter", "duller"),
               c("bright", "plain", "dull"),
               c("brightest", "brighter", "duller", "dullest"),
               c("brightest", "brighter", "plain", "duller", "dullest"))

monoL <- function(colours, hcl) {
    hue <- colourName(colours, basicHues)
    luminance <- lnames[[length(colours)]]
    if (hcl[1, "L"] > hcl[nrow(hcl), "L"]) luminance <- rev(luminance)
    paste(luminance, hue)    
}

monoC <- function(colours, hcl) {
    hue <- colourName(colours, basicHues)
    chroma <- cnames[[length(colours)]]
    if (hcl[1, "C"] < hcl[nrow(hcl), "C"]) chroma <- rev(chroma)
    paste(chroma, hue)
}

monoLC <- function(colours, hcl) {
    hue <- colourName(colours, basicHues)
    luminance <- lnames[[length(colours)]]
    if (hcl[1, "L"] > hcl[nrow(hcl), "L"]) luminance <- rev(luminance)
    chroma <- cnames[[length(colours)]]
    if (hcl[1, "C"] < hcl[nrow(hcl), "C"]) chroma <- rev(chroma)
    paste(chroma, luminance, hue)
}

## Break scale into sub-scales
subRuns <- function(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL) {
    ## Try the longest monotonic run
    f <- list(inverse.rle(hruns), inverse.rle(cruns), inverse.rle(lruns))
    indices <- split(2:length(colours), f, drop=TRUE)
    runs <- lapply(indices,
                   function(i) { if (2 %in% i) i <- c(1, i); colours[i] })
    hcls <- lapply(indices,
                   function(i) { if (2 %in% i) i <- c(1, i); hcl[i,] })
    colourRuns <-
        mapply(
            function(c, hcl) {
                colourRun(c, 
                          colourList, colourMetric,
                          hcl,
                          if (is.null(fullHrange)) hrange,
                          if (is.null(fullCrange)) crange,
                          if (is.null(fullLrange)) lrange)
            },
            runs, hcls)
    firstName <-
        unlist(mapply(
            function(i, cruns)
                if (2 %in% i) cruns[1] else NULL,
            indices, colourRuns))
    otherNames <-
        mapply(
            function(i, cruns)
                if (2 %in% i) cruns[-1] else cruns,
            indices, colourRuns, SIMPLIFY=FALSE)
    unname(c(firstName, unsplit(otherNames, f, drop=TRUE)))
}

colourRun <- function(colours, colourList, colourMetric,
                      hcl,
                      fullHrange=NULL, fullCrange=NULL, fullLrange=NULL) {

    ## Handle (recursive) empty request
    if (length(colours) == 0) {
        return(NULL)
    }
    
    ## If only one colour, just use colourName()
    if (length(colours) == 1) {
        return(colourName(colours, colourList, colourMetric))
    }
    
    L <- hcl[,"L"]
    C <- hcl[,"C"]
    H <- hcl[,"H"]
    ## Low chroma and/or low/high luminance produces unreliable hue
    ## (setting those hues to NA will mean they are treated as no change)
    H[C < deltaTol | L < deltaTol | L > (100 - deltaTol)] <- NA
    dl <- diff(L)
    dc <- diff(C)
    dh <- angleDiff(H)
    lruns <- runs(dl)
    cruns <- runs(dc)
    hruns <- runs(dh)
    lrange <- diff(range(L))/100
    crange <- diff(range(C))/100
    hrange <- angleRange(H)/360

    hscale <- hrange/lrange > 5 && hrange/crange > 5
    lscale <- lrange/hrange > 5 && lrange/crange > 5
    cscale <- crange/hrange > 5 && crange/lrange > 5

    hlscale <- hrange/crange > 5 && lrange/crange > 5
    hcscale <- hrange/lrange > 5 && crange/lrange > 5
    lcscale <- lrange/hrange > 5 && crange/hrange > 5
    
    if (hscale) {
        if (length(hruns$lengths) == 1) {
            monoH(colours)
        } else {
            ## Handle individual runs
            hcl[,1] <- mean(hcl[,1])
            hcl[,2] <- mean(hcl[,2])
            subRuns(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL)
        }
    } else if (lscale) {
        if (length(lruns$lengths) == 1) {
            monoL(colours, hcl)
        } else {
            ## Handle individual runs            
            hcl[,3] <- mean(hcl[,3])
            hcl[,2] <- mean(hcl[,2])
            subRuns(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL)
        }
    } else if (cscale) {
        if (length(cruns$lengths) == 1) {
            monoC(colours, hcl)
        } else {
            ## Handle individual runs            
            hcl[,1] <- mean(hcl[,1])
            hcl[,3] <- mean(hcl[,3])
            subRuns(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL)
        }
    } else if (hlscale) {
        if (length(hruns$lengths) == 1 &&
            length(lruns$lengths) == 1) {
            monoL(colours, hcl)
        } else {
            ## Handle individual runs            
            hcl[,2] <- mean(hcl[,2])
            subRuns(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL)
        }
    } else if (hcscale) {
        if (length(hruns$lengths) == 1 &&
            length(cruns$lengths) == 1) {
            monoC(colours, hcl)
        } else {
            ## Handle individual runs            
            hcl[,1] <- mean(hcl[,1])
            subRuns(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL)
        }
    } else if (lcscale) {
        if (length(lruns$lengths) == 1 &&
            length(cruns$lengths) == 1) {
            hcl[,3] <- mean(hcl[,3])
            monoLC(colours, hcl)
        } else {
            ## Handle individual runs
            subRuns(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL)
        }
    } else {
        ## All three dimensions varying (try smaller runs)
        if (length(colours) == 2) {
            ## If only two colours, we would loop forever trying smaller runs
            colourName(colours, colourList, colourMetric)
        } else {
            subRuns(colours, colourList, colourMetric,
                    hcl,
                    hrange, crange, lrange,
                    hruns, cruns, lruns,
                    fullHrange=NULL, fullCrange=NULL, fullLrange=NULL)
        }
    }
}

colourScale <- function(colours,
                        colourList=Rcolours,
                        colourMetric=euclideanLUV) {
    if (is.numeric(colours)) {
        colours <- col2char(colours)
    }
    colHCL <- as(hex2RGB(col2hex(colours)), "polarLUV")
    hcl <- coords(colHCL)

    scale <- colourRun(colours, colourList, euclideanLUV, hcl)

    notrun <- function() {
        luv <- coords(as(colHCL, "LUV"))
        library(scatterplot3d)
        scatterplot3d(luv[,2], luv[,3], luv[,1], 
                      xlim=c(-100, 100), ylim=c(-100, 100), zlim=c(0, 100),
                      pch=16, color=colours, type="h")
    }

    scale
}

testing <- function() {

    ## NOTE:
    ## Have to watch out for hcl() producing colours not representable in RGB
    ## (hence fixup=FALSE, which produces NAs)
    ## Have to watch out for chroma == 0 or luminance == 0/100
    ## (which generates random hue)
    
    library(roloc)
    ## Monotonic hue
    colourScale(hcl(seq(0, 300, 60), 40, 80, fixup=FALSE))
    ## Monotonic luminance
    colourScale(hcl(240, 30, seq(50, 80, 10), fixup=FALSE))
    ## Monotonic chroma 
    colourScale(hcl(240, seq(10, 40, 10), 80, fixup=FALSE))

    library(colorspace)
    ## Monotonic hue
    colourScale(rainbow_hcl(5))
    ## Monotonic chroma and luminance
    colourScale(sequential_hcl(5))
    ## Diverging chroma and luminance
    colourScale(diverge_hcl(5))

    library(RColorBrewer)
    colourScale(brewer.pal(7, "Greens"))

    library(pals)
    colourScale(stepped())    
}
