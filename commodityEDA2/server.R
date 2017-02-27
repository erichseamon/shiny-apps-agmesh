library(shiny)
library(datasets)
library(rgdal)
library(leaflet)
library(png)
library(jpeg)
library(ncdf)   
library(rasterVis)
library(maptools)

# Define server logic required to summarize and view the selected
# dataset

shinyServer(function(input, output) {
  output$plot <- renderPlot({ 
   setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", sep="")) 
   r <- readPNG(paste(input$year, ".", input$month, ".", input$commodity, "_plot", sep=""))
   plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "", axes = FALSE)
   rasterImage(r, 100,300,250,450) 
})
  output$plot2 <- renderPlot({
    setwd("/dmine/data/USDA/agmesh-scenarios/scenario_52177/commodity_csv/")
    commodity <- read.csv(paste(input$commodity, ".csv", sep=""))
    commodity_result <- subset(commodity, year == input$year & monthcode == input$month) 
    library(lattice)
    bwplot(damagecause ~ log(loss), data=commodity_result, scales=list(x=list(rot=90)))   
  })

  output$plot3 <- renderPlot({
    setwd("/dmine/data/USDA/agmesh-scenarios/scenario_52177/commodity_csv/")
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
    
    setwd("/dmine/data/USDA/agmesh-scenarios/scenario_52177/raster_commodity/")
    r1 <-crop(raster(paste(input$year, ".", "1", ".", input$commodity, "_raster.grd", sep="")), WA)
    r2 <- crop(raster(paste(input$year, ".", "2", ".", input$commodity, "_raster.grd", sep="")), WA)
    r3 <- crop(raster(paste(input$year, ".", "3", ".", input$commodity, "_raster.grd", sep="")), WA)
    r4 <- crop(raster(paste(input$year, ".", "4", ".", input$commodity, "_raster.grd", sep="")), WA)
    r5 <- crop(raster(paste(input$year, ".", "5", ".", input$commodity, "_raster.grd", sep="")), WA)
    r6 <- crop(raster(paste(input$year, ".", "6", ".", input$commodity, "_raster.grd", sep="")), WA)
    r7 <- crop(raster(paste(input$year, ".", "7", ".", input$commodity, "_raster.grd", sep="")), WA)
    r8 <- crop(raster(paste(input$year, ".", "8", ".", input$commodity, "_raster.grd", sep="")), WA)
    r9 <- crop(raster(paste(input$year, ".", "9", ".", input$commodity, "_raster.grd", sep="")), WA)
    r10 <- crop(raster(paste(input$year, ".", "10", ".", input$commodity, "_raster.grd", sep="")), WA)
    r11 <- crop(raster(paste(input$year, ".", "11", ".", input$commodity, "_raster.grd", sep="")), WA)
    r12 <- crop(raster(paste(input$year, ".", "12", ".", input$commodity, "_raster.grd", sep="")), WA)
    
    p1 <- levelplot(r1, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("January ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p2 <- levelplot(r2, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("February ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p3 <- levelplot(r3, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("March ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p4 <- levelplot(r4, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("April ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p5 <- levelplot(r5, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("May ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p6 <- levelplot(r6, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("June ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p7 <- levelplot(r7, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("July ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p8 <- levelplot(r8, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("August ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p9 <- levelplot(r9, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("September ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p10 <- levelplot(r10, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("October ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p11 <- levelplot(r11, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("November ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))
    p12 <- levelplot(r12, att = 'LOSS', col.regions=rgb.palette(120), scales = list(draw = FALSE), main = paste("December ", input$year, sep=""), xlab="", ylab="") + layer(sp.polygons(WA, alpha=0.2), data=list(WA=WA))

    print(p1, split = c(1,1,2,6), more = TRUE)
    print(p2, split = c(2,1,2,6), more = TRUE)
    print(p3, split = c(1,2,2,6), more = TRUE)
    print(p4, split = c(2,2,2,6), more = TRUE)
    print(p5, split = c(1,3,2,6), more = TRUE)
    print(p6, split = c(2,3,2,6), more = TRUE)
    print(p7, split = c(1,4,2,6), more = TRUE)
    print(p8, split = c(2,4,2,6), more = TRUE)
    print(p9, split = c(1,5,2,6), more = TRUE)
    print(p10, split = c(2,5,2,6), more = TRUE)
    print(p11, split = c(1,6,2,6), more = TRUE)
    print(p12, split = c(2,6,2,6), more = FALSE)

















 })
  






})
