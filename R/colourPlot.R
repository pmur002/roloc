
## Functions to visualise colour names

colourRegion <- function(names, colourName) {
    indices <- arrayInd(which(names == colourName), c(256, 256))
    corners <- rbind(cbind((indices[,1] - 1.5)/255, (indices[,2] - 1.5)/255),
                     cbind((indices[,1] - 1.5)/255, (indices[,2])/255),
                     cbind((indices[,1])/255, (indices[,2] - 1.5)/255),
                     cbind((indices[,1])/255, (indices[,2])/255))
    boundary <- chull(corners)
    as.data.frame(corners[boundary, ])
}

colourPlot <- function(colour,
                       colourList=Rcolours,
                       colourMetric=euclideanLUV,
                       plane="RG") {
    if (length(colour) > 1) 
        stop("Only a single colour specification is supported (for now)")
    require(ggplot2)
    if (is.numeric(colour)) {
        colour <- col2char(colour)
    }
    colName <- colourName(colour, colourList, colourMetric)
    colRGB <- coords(hex2RGB(col2hex(colour)))
    x <- 0:255/255
    y <- 0:255/255
    title <- paste0('Colour = "', colour, '"\nName = "', colName, '"')
    if (plane == "RG") {
        colX <- colRGB[,1]
        colY <- colRGB[,2]
        xlab <- "red"
        ylab <- "green"
        z <- unique(colRGB[,3])
        subtitle <- paste0("blue = ", round(z, 2))
    } else if (plane == "GB") {
        colX <- colRGB[,2]
        colY <- colRGB[,3]
        xlab <- "green"
        ylab <- "blue"
        z <- unique(colRGB[,1])
        subtitle <- paste0("red = ", round(z, 2))
    } else if (plane == "RB") {
        colX <- colRGB[,1]
        colY <- colRGB[,3]
        xlab <- "red"
        ylab <- "blue"
        z <- unique(colRGB[,2])
        subtitle <- paste0("green = ", round(z, 2))
    } else {
        stop("Invalid 'plane' argument")
    }
    RGB <- expand.grid(x, y, z)
    cols <- rgb(RGB)
    names <- colourName(cols, colourList, colourMetric)
    region <- colourRegion(colName, names)
    ggplot(as.data.frame(RGB)) +
        geom_tile(aes(Var1, Var2), fill=cols) +
        scale_x_continuous(expand=c(0, 0)) +
        scale_y_continuous(expand=c(0, 0)) +
        ggtitle(title) + xlab(xlab) + ylab(ylab) +
        theme(plot.title=element_text(face="bold")) +
        labs(caption=subtitle) +
        geom_point(x=colX, y=colY) +
        geom_polygon(aes(x=V1, y=V2), data=region, col="black", fill=NA) 
}
