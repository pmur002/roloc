
euclideanDistance <- function(spec, list, tol) {
    if (any(!is.finite(spec))) {
        NA
    } else {
        distances <- sqrt((spec[1] - list[,1])^2 +
                          (spec[2] - list[,2])^2 +
                          (spec[3] - list[,3])^2)
        minIndex <- which.min(distances)
        if (distances[minIndex] > tol) {
            0
        } else {
            minIndex
        }
    }
}
                    
euclideanLUV <- function(colour, colourList, tolerance=Inf) {
    ## Both 'colour' and 'colourList' are already 'sRGB'
    ## Convert 'spec' to 'Luv'
    specColour <- as(colour, "LUV")
    ## Convert 'colourList' to 'Luv'
    listColours <- as(colourList, "LUV")
    ## Calculate distances
    specCoords <- coords(specColour)
    listCoords <- coords(listColours)
    apply(specCoords, 1, euclideanDistance, listCoords, tolerance)
}

euclideanRGB <- function(colour, colourList, tolerance=Inf) {
    ## Both 'colour' and 'colourList' are already 'sRGB'
    ## Calculate distances
    specCoords <- coords(colour)
    listCoords <- coords(colourList)
    apply(specCoords, 1, euclideanDistance, listCoords, tolerance)
}
                         
