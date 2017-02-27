library(shiny)
library(datasets)
library(rgdal)
library(leaflet)
library(png)
library(jpeg)
library(ncdf)   
library(rasterVis)
library(maptools)


library(RColorBrewer)
library(ggplot2)
library(data.table)
library(rgdal)
library(maptools)
library(maptools)
library(SDMTools)
library(fields)

# Define server logic required to summarize and view the selected
# dataset

shinyServer(function(input, output) {

  output$plot <- renderPlot({
  i = paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")
  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state)
  #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  u <- data.frame(trimws(x$county))

    colnames(u) <- c("NAME")
    colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")
    z <- cbind(x,u)
    m <- merge(counties, z, by='NAME')
    m$LOSS[is.na(m$LOSS)] <- 0
    m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
    m$ACRES[is.na(m$ACRES)] <- 0

    #shapefile(m)
    #--begin polygon work
    #length(na.omit(m$LOSS))
    tt <- colorRampPalette(brewer.pal(8, "Spectral"))
    mz <- subset(m, LOSS > 0)
    leng <- length(m$LOSS)
    len2 <- tt(len <- length(mz$LOSS))
    len2a <- length(mz$LOSS)
    len3 <- tt(len <- length(m$LOSS))
  
    orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]

    newframe <- data.frame(m$LOSS)
  
    xx <- 1
    newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
  
  for (jj in 1:leng){
    
   if (m$LOSS[jj] == 0) {
     #print("yes this worked, added 0")
     newmatrix[jj,] <- 0
   } else {
     #print("yes, this worked, added color")
     #newmatrix[jj,] <- len3[jj] 
     newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
   }
    
  }
  
  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  

  newmatrix <- c(newmatrix)
  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)

  png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/", "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  layout(matrix(c(1,2),1, 2, byrow=TRUE))
  #--turn image horizontal
  
  plotmonth <- month.abb[x$MONTHCODE[1]]
  plotyear <- x$YEAR[1]
  
  plot(m, col = newmatrix, main = paste(input$state, " crop loss \n", plotmonth, " ", plotyear, sep=""))
  barplot(mz$LOSS, names.arg = mz$NAME, las=2, col = newmatrix2)

})


  output$plot2 <- renderPlot({
    setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/commodity_csv/", sep=""))
    commodity <- read.csv(paste(input$commodity, ".csv", sep=""))
    commodity_result <- subset(commodity, year == input$year & monthcode == input$month) 
    library(lattice)
    bwplot(damagecause ~ log(loss), data=commodity_result, scales=list(x=list(rot=90)))   
  })

  output$plot3 <- renderPlot({
    setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/commodity_csv/", sep=""))
    commodity <- read.csv(paste(input$commodity, ".csv", sep=""))
    commodity_year <- subset(commodity, year == input$year)
    library(lattice)
    bwplot(damagecause ~ log(loss), data=commodity_year, groups=year, scales=list(x=list(rot=90)))  
  })

  output$plot4 <- renderPlot({
    #setwd("/dmine/data/USDA/agmesh-scenarios/scenario_52177/raster_commodity/")
    #r <- raster(paste(input$year, ".", input$month, ".", input$commodity, "_raster.grd", sep="")) 


    #setwd("/dmine/data/counties/") 
    setwd("/srv/shiny-server/commodityEDA/data/counties/")
    counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    #projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    WA <- counties[grep(input$county, counties@data$NAME),]
    #rcounty <- crop(r, WA_county) 
    #WA <- counties[grep(input$county, WA@data$NAME),]
    # WA <- counties[grep(input$state, counties@data$STATE_NAME), (input$county, counties@data$COUNTY),]
    rgb.palette <- colorRampPalette(c("blue", "green"))
    #levelplot(rcounty, att = 'LOSS', col.regions=rgb.palette(120)) + layer(sp.polygons(WA_county, alpha=0.2), data=list(WA_county=WA_county))
    

 })
  






})
