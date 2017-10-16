
## Functions to visualise colour names

colourRegion <- function(colourName, names) {
    indices <- arrayInd(which(names == colourName), c(256, 256))
    corners <- rbind(cbind((indices[,1] - 1.5)/255, (indices[,2] - 1.5)/255),
                     cbind((indices[,1] - 1.5)/255, (indices[,2])/255),
                     cbind((indices[,1])/255, (indices[,2] - 1.5)/255),
                     cbind((indices[,1])/255, (indices[,2])/255))
    boundary <- chull(corners)
    as.data.frame(corners[boundary, ])
}

colPlot <- function(colour, colourList, colourMetric,
                    x, y, z, colX, colY, xlab, ylab, subtitle) {
    require(ggplot2)
    colName <- colourName(colour, colourList, colourMetric)
    title <- paste0('Colour = "', paste(colour, collapse='", "'),
                    '"\nName = "', paste(colName, collapse='", "'), '"')
    RGB <- expand.grid(x, y, z)
    cols <- rgb(RGB)
    names <- colourName(cols, colourList, colourMetric)
    regions <- lapply(colName, colourRegion, names)
    regionDF <- cbind(do.call(rbind, regions),
                      group=rep(1:length(regions), sapply(regions, nrow)))
    ggplot(as.data.frame(RGB)) +
        geom_tile(aes(Var1, Var2), fill=cols) +
        scale_x_continuous(expand=c(0, 0)) +
        scale_y_continuous(expand=c(0, 0)) +
        ggtitle(title) + xlab(xlab) + ylab(ylab) +
        theme(plot.title=element_text(face="bold")) +
        labs(caption=subtitle) +
        geom_point(aes(x=x, y=y), data=data.frame(x=colX, y=colY), pch=1) +
        geom_polygon(aes(x=V1, y=V2, group=group), data=regionDF,
                     col="black", fill=NA) 
}

colourPlot <- function(colour,
                       colourList=Rcolours,
                       colourMetric=euclideanLUV,
                       plane="RG",
                       newpage=TRUE) {
    if (is.numeric(colour)) {
        colour <- col2char(colour)
    }
    colRGB <- coords(hex2RGB(col2hex(colour)))
    x <- 0:255/255
    y <- 0:255/255
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
    nz <- length(z)
    if (nz > 1) {
        require(grid)
        dim <- n2mfrow(nz)
        ## Swap dimensions so have preference for fewer rows 
        layout <- grid.layout(nrow=dim[2], ncol=dim[1])
        if (newpage) {
            grid.newpage()
        }
        pushViewport(viewport(layout=layout, name="roloc-layout"))
        for (i in 1:dim[2]) {
            for (j in 1:dim[1]) {
                index <- (i - 1)*dim[1] + j
                colIndex <- switch(plane,
                                   RG=which(colRGB[,3] == z[index]),
                                   GB=which(colRGB[,1] == z[index]),
                                   RB=which(colRGB[,2] == z[index]))
                if (index <= nz) {
                    pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
                    print(colPlot(colour[colIndex], colourList, colourMetric,
                                  x, y, z[index],
                                  colX[colIndex], colY[colIndex],
                                  xlab, ylab, subtitle[index]),
                          newpage=FALSE)
                    upViewport()
                }
            }
            upViewport()
        }
    } else {
        print(colPlot(colour, colourList, colourMetric, x, y, z,
                      colX, colY, xlab, ylab, subtitle),
              newpage=newpage)
    }            
}

colorPlot <- colourPlot
