
euclideanDistance <- function(spec, list, tolerance) {
    if (any(!is.finite(spec))) {
        NA
    } else {
        distances <- sqrt((spec[1] - list[,1])^2 +
                          (spec[2] - list[,2])^2 +
                          (spec[3] - list[,3])^2)
        minIndex <- which.min(distances)
        if (distances[minIndex] > tolerance) {
            0
        } else {
            minIndex
        }
    }
}

LUVcoords <- function(sRGB) {
    luv <- as(sRGB, "LUV")
    coords <- coords(luv)
    ## Special case "black" to avoid NAs (set U and V to 0)
    coords[coords[,"L"] == 0, ] <- 0
    coords
}

euclideanLUV <- function(colour, colourList, tolerance=Inf) {
    ## Both 'colour' and 'colourList' are already 'sRGB'
    specCoords <- LUVcoords(colour)
    listCoords <- LUVcoords(colourList)
    apply(specCoords, 1, euclideanDistance, listCoords, tolerance)
}

euclideanRGB <- function(colour, colourList, tolerance=Inf) {
    ## Both 'colour' and 'colourList' are already 'sRGB'
    ## Calculate distances
    specCoords <- coords(colour)
    listCoords <- coords(colourList)
    ## This needs speeding up when number of colours is large
    numCores <- detectCores()
    numSpecs <- nrow(specCoords)
    apply(specCoords, 1, euclideanDistance, listCoords, tolerance)
}
                         
