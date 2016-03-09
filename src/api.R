library(RCurl)
library(jsonlite)
library(rgdal)
library(ggplot2)
library(ggmap)

crimes <- fromJSON("https://hoyodecrimen.com/api/v1/crimes")$rows

#Get all crimes in all sectors for the last 12 months
crime.sectors <- fromJSON("https://hoyodecrimen.com/api/v1/sectores/all/crimes/all/period")$rows

sectores <- readOGR(file.path("cuadrante-shps", "sectores.shp"), layer = "sectores")
fsectors <- fortify(sectores, region = "sector")
names(crime.sectors)[5] <- "id"
sector.map <- plyr::join(fsectors, crime.sectors)
sector.map$rate <- sector.map$count / sector.map$population * 10^5

draw_map <- function(sector.map, crimeName = "ROBO A TRANSEUNTE C.V.") {
  ggplot(subset(sector.map, crime == crimeName), 
         aes(long, lat, group = group, fill = rate), color = "black") +
    geom_polygon(color = "#666666", size = .1) +
    coord_map() +
    ggtitle(crimeName) +
    scale_fill_continuous(low = "#fee6ce",
                          high = "#e6550d", space = "Lab", na.value = "grey50",
                          guide = "colourbar") +
    theme_bw()
}

draw_gmap <- function(sector.map, crimeName, bb, pal, alpha=.7) {
  ggmap(get_map(location = bb)) + 
    geom_polygon(data= subset(sector.map, crime == crimeName), 
                 aes(long, lat, group = group, fill = rate),
                 color = "#666666", size = .1,
                 alpha = alpha) +
    coord_map() +
    ggtitle(crimeName) +
    scale_fill_continuous(low = brewer.pal(9, pal)[1],
                          high = brewer.pal(9, pal)[9], 
                          space = "Lab", na.value = "grey50",
                          guide = "colourbar") +
    #scale_fill_brewer(palette = "Reds") +
    theme ( 
      
      legend.justification = c(0, 0),
      legend.background = element_rect(colour = F, fill = "white"),
      legend.key = element_rect (fill = F, colour = F),
      panel.grid.major = element_blank (), # remove major grid
      panel.grid.minor = element_blank (),  # remove minor grid
      axis.text = element_blank (), 
      axis.title = element_blank (),
      axis.ticks = element_blank ()
    ) 
  
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# draw_map(sector.map, "ROBO A TRANSEUNTE C.V.")
# draw_map(sector.map, "ROBO A TRANSEUNTE S.V.")
# draw_map(sector.map, "ROBO A CASA HABITACION C.V.")

bb <- bbox(sectores)
rtcv <- draw_gmap(sector.map, "ROBO A TRANSEUNTE C.V.", bb, "Oranges", 1)
rtsv <- draw_gmap(sector.map, "ROBO A TRANSEUNTE S.V.", bb, "PuRd", 1)
rccv <- draw_gmap(sector.map, "ROBO A CASA HABITACION C.V.", bb, "PuBu", 1)
hom <- draw_gmap(sector.map, crimes[1,], bb, "Reds", 1)
rncv <- draw_gmap(sector.map, crimes[3,], bb, "Blues", 1)
rvcv <- draw_gmap(sector.map, crimes[6,], bb, "Purples", 1)
rvsv <- draw_gmap(sector.map, crimes[7,], bb, "Greens", 1)
viol <- draw_gmap(sector.map, crimes[8,], bb, "Greys", 1)

multiplot(hom, viol,  rvsv, 
          rtcv,  rccv, rvcv,
          rtsv, rncv, 
          cols = 3)
