
## A colourList has to be a list with components 'names' and 'colours'
## The latter should be a "color" object (from the 'colorspace' package)
## with the same number of colours as length(names)

colourList <- function(names, colours) {
    if (!is(colours, "color"))
        stop("'colours' must be a color object")
    if (nrow(coords(colours)) != length(names))
        stop("'colours' and 'names' not the same length")
    cl <- list(names=as.character(names), colours=colours)
    class(cl) <- "colourList"
    cl
}

colorList <- colourList

## Predefined colourLists

## The list of colours that R knows about
Rcolours <- colourList(names=colours(),
                       colours=sRGB(t(col2rgb(colours())/255)))
## Aliases
X11colours <- Rcolours
SVGcolours <- Rcolours
CSScolours <- Rcolours
Rcolors <- Rcolours
X11colors <- Rcolours
SVGcolors <- Rcolours
CSScolors <- Rcolours

## A small standard set for the Web ("HTML colours")
HTMLcolours <- colourList(names=c("black",
                                  "silver",
                                  "gray",
                                  "white",
                                  "maroon",
                                  "red",
                                  "purple",
                                  "fuschia",
                                  "green",
                                  "lime",
                                  "olive",
                                  "yellow",
                                  "navy",
                                  "blue",
                                  "teal",
                                  "aqua"),
                          colours=sRGB(rbind(c(0,0,0),
                                             c(192, 192, 192),
                                             c(128, 128, 128),
                                             c(255, 255, 255),
                                             c(128, 0, 0),
                                             c(255, 0, 0),
                                             c(128, 0, 128),
                                             c(255, 0, 255),
                                             c(0, 128, 0),
                                             c(0, 255, 0),
                                             c(128, 128, 0),
                                             c(255, 255, 0),
                                             c(0, 0, 128),
                                             c(0, 0, 255),
                                             c(0, 128, 128),
                                             c(0, 255, 255))/255))

HTMLcolors <- HTMLcolours

## NBS-ISCC Colour Dictionary
NBS <- read.table(system.file("Dictionaries", "NBS-ISCC-rgb.txt",
                              package="roloc"),
                  comment.char="!")
NBScolours <- colourList(NBS$V4, sRGB(as.matrix(NBS[1:3])/255))
NBScolors <- NBScolours

## Resene Colour Dictionary
Resene <- read.table(system.file("Dictionaries", "Resene-2010-rgb.txt",
                                 package="roloc"),
                     comment.char="!")
ReseneColours <- colourList(Resene$V4, sRGB(as.matrix(Resene[1:3])/255))
ReseneColors <- ReseneColours
    
## Hues based on 1908 colour wheel
## https://en.wikipedia.org/wiki/Color_wheel#/media/File:RGV_color_wheel_1908.png
## https://en.wikipedia.org/wiki/Color_wheel
luminance <- c("light", "", "dark")
chroma <- c("bright", "", "dull")
hue <- c("red", "yellow", "green", "cyan", "blue", "magenta")
luminanceNum <- c(80, 50, 20)
chromaNum <- c(80, 50, 20)
hueNum <- seq(0, 300, 60)
colourNames <- gsub(" +", " ", 
                    gsub("^ +| +$", "",
                         apply(expand.grid(chroma, luminance, hue), 1, 
                               paste, collapse=" ")))
colours <- apply(expand.grid(chromaNum, luminanceNum, hueNum), 1, 
                             function(x) hcl(x[3], x[1], x[2]))
basicColours <- colourList(colourNames, hex2RGB(colours))

basicHues <- colourList(hue, hex2RGB(hcl(hueNum, 40, 50)))
