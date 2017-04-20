
library(rgdal)
library(leaflet)

source("/srv/shiny-server/commodityEDA-dashboard2/utils.R")

shinyServer(function(input, output) {
  
  output$kpi_summary_box_1 <- renderValueBox({
    valueBox(
      value = sprintf("%s", 245923),
      subtitle = sprintf("WHEAT LOSS (%.1f%%)", 8.9202),
      icon = icon("pagelines"),
      color = "yellow"
    )
  })
  
  output$kpi_summary_box_2 <- renderValueBox({
    valueBox(
      value = sprintf("%s", 190),
      subtitle = sprintf("PREDICTED WHEAT LOSS (%.1f%%)", -0.23),
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$kpi_summary_box_3 <- renderValueBox({
    valueBox(
      value = sprintf("%s", 104924422),
      subtitle = sprintf("KPI 3 (%.1f%%)", -5.422),
      icon = icon("arrow-down"),
      color = "green"
    )
  })

output$plot7y <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {





#--
library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state, counties@data$STATE_NAME),]
#counties <- counties[grep(input$county, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

#newj <- 1
#for (j in climz) {
##  newx <- 1
# for (i in monthz) {
# setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/netcdf/")
# ncfile = paste(input$climate, "_", i, "_", input$year, ".nc", sep="")
# rasterout <- brick(ncfile)
# rasterout <- crop(rasterout, counties)
# vect <- cellStats(rasterout, mean)
# vect2 <- mean(vect)
# newmat[newx] <- vect2
## newx <- newx + 1
# }
 #newj <- newj + 1
#}


##colnames(newmat) <- "pr"
##rownames(newmat) <- monthz
##newmat <- data.frame(newmat)

#---

i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

#DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT, commodity == input$commodity)
DT2 <- subset(DT2, damagecause == input$damage)

newmat2 <- matrix(, ncol = 1, nrow = 12)
#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1

#--loss
 newii <- 1
 for (ii in monthzz) {
   nez <- subset(DT2, monthcode == ii)
   newmat2[ii, newjj] <- nrow(nez)
   newii <- newii + 1
 }

newmat3 <- matrix(, ncol = 1, nrow = 12)

#--acres

  newii <- 1
  for (ii in monthzz) {
    nez <- subset(DT2, monthcode == ii)
    newmat3[ii, newjj] <- sum(nez[,31])
    newii <- newii + 1
  }

newmat4 <- cbind(newmat2, newmat3)
colnames(newmat4) <- c("claims", "acres")
newmat <- as.data.frame(c(1:12))
#newmat5 <- as.matrix(newmat4)

newmat5 <- cbind(newmat, newmat4)
rownames(newmat5) <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


newmat5[is.na(newmat5)] <- 0

#library(dygraphs)
#library(xts)
#test <- cbind(newmat5$loss)
#dygraph(data.frame(test))

#newmat6 <- newmat5
#rownames(newmat6) -> data.frame(newmat7)
#test <- cbind(newmat7, newmat5$pr)
#as.xts(newmat5$pr, newmat7)

#xxx <- min(newmat$pr)
#xxxx <- max(newmat$pr)
#interval <- (xxxx-xxx)/5



#setwd("/dmine/data/counties/")

#    counties <- readShapePoly('UScounties.shp',
#                              proj4string=CRS
#                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#    counties <- subset(counties, STATE_NAME %in% input$state)
#    counties_one <- subset(counties, NAME %in% input$county)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 1, byrow=TRUE))

barplot(newmat5$claims, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), cex.names=1, las=3, col="blue", main = paste(input$state, " crop claim counts ", input$month, " ", input$year, " ", input$commodity, "\n", "Annual breakdown of insurance claim counts per month, due to ", input$damage, sep=""), horiz=FALSE)

#par(mar=c(0,0,3,1)+1)

#plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county, " County map", sep=""))

#plot(counties_one, col="blue", add=T)

#plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
#axis(side=1, at=c(1:12))
#axis(side=2, at=seq(xxx, xxxx, by = interval))
#lines(newmat5$pr, las=2, col="blue")

  })
})





output$countycontrols <- renderUI({

   i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")

   setwd(yeardir)
   x <- as.data.frame(read.csv(i, strip.white = TRUE))
   DT <- data.table(x)

   DT2 <- unique(DT$county)
   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( DT2 )

  selectizeInput("county", "Choose a county", uf2[,1], choices = as.vector(uf2[,1]))

})


output$commoditycontrols <- renderUI({
   uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
   elems <- unlist( strsplit( uniquez, "\\." ) )
   uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( uf2 )

   uf3 <- subset(uf2, V1 == input$year & V2 == input$month)
  selectizeInput("commodity", "Choose a commodity", uf3[,3], choices = as.vector(uf3[,3]))

})


output$plot4 <- renderPlot({

  withProgress(message = 'Working', value = 0, {

 setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state)
  #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
  setwd(yeardir)
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  DT <- data.table(x)

#--change to lowercase DT2!!
  DT2 <- subset(DT, commodity == input$commodity)
  DT2loss <- DT2[,list(loss=sum(loss)), by = county]
  DT2$acres <- as.numeric(DT2$acres)
  DT2acres <- DT2[,list(acres=sum(acres)), by = county]
  DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
  DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
  DT6 <- cbind(DT2loss, DT2acres$acres)
  DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres)
  setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
  names(counties)[1] <- "county"

  m <- merge(counties, DT6, by='county')
  names(m)[7] <- "acres"


  m$loss[is.na(m$loss)] <- 0
  m$acres[is.na(m$acres)] <- 0

  tt <- colorRampPalette(c("light blue", "dark blue"), space = "Lab")
  lengacres <- length(m$acres)
  leng <- length(m$loss)
  len3 <- tt(len <- length(m$loss))
  len4 <- tt(len <- length(m$acres))


  orderedcolors2 <- tt(length(m$loss))[order(m$loss)]
  orderedcolors3 <- tt(length(m$acres))[order(m$acres)]
  m[["loss"]][is.na(m[["loss"]])] <- 0
  m[["acres"]][is.na(m[["acres"]])] <- 0

  xx <- 1
  newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
  for (jj in 1:leng){
    if (m$loss[jj] == 0) {
      #print("yes this worked, added 0")

      newmatrix[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj]
      newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }}

  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

  for (jj in 1:leng){

    if (m$acres[jj] == 0) {
      newmatrix_acres[jj,] <- 0
    } else {
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }}

  par(mar=c(6,6,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,1,2),1, 3, byrow=TRUE))
  #--turn image horizontal
  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]

  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)
  par(mar=c(6,6,8,2))
  #b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix, cex.names=1, horiz=TRUE, main = paste(input$state, " crop loss bar chart ($) \n", " ", plotyear, "\n", input$commodity, sep=""), cex.axis=1.3, cex.main=1.5, width=4)

map <-  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -80.8858673, lat = 41.1450276, zoom = 5)
 

  #plot(m, col = newmatrix, main = paste(input$state, " crop loss map ($) \n", " ", plotyear, "\n", input$commodity, sep=""), cex.main=1.5)


})

})
 

output$plot5 = renderPlot({
library(leaflet)

setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state)
  #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
  setwd(yeardir)
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  DT <- data.table(x)

#--change to lowercase DT2!!
  DT2 <- subset(DT, commodity == input$commodity)
  DT2loss <- DT2[,list(loss=sum(loss)), by = county]
  DT2$acres <- as.numeric(DT2$acres)
  DT2acres <- DT2[,list(acres=sum(acres)), by = county]
  DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
  DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
  DT6 <- cbind(DT2loss, DT2acres$acres)
  DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres)
  setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
  names(counties)[1] <- "county"

  m <- merge(counties, DT6, by='county')
  names(m)[7] <- "acres"


  m$loss[is.na(m$loss)] <- 0
  m$acres[is.na(m$acres)] <- 0

  tt <- colorRampPalette(c("light blue", "dark blue"), space = "Lab")
  lengacres <- length(m$acres)
  leng <- length(m$loss)
  len3 <- tt(len <- length(m$loss))
  len4 <- tt(len <- length(m$acres))


  orderedcolors2 <- tt(length(m$loss))[order(m$loss)]
  orderedcolors3 <- tt(length(m$acres))[order(m$acres)]
  m[["loss"]][is.na(m[["loss"]])] <- 0
  m[["acres"]][is.na(m[["acres"]])] <- 0

  xx <- 1
  newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
  for (jj in 1:leng){
    if (m$loss[jj] == 0) {
      #print("yes this worked, added 0")


newmatrix[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj]
      newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }}

  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

  for (jj in 1:leng){

    if (m$acres[jj] == 0) {
      newmatrix_acres[jj,] <- 0
    } else {
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }}

 par(mar=c(6,6,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,1,2),1, 3, byrow=TRUE))
  #--turn image horizontal
  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]

  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)
  par(mar=c(6,6,8,2))
  #b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix, cex.names=1, horiz=TRUE, main = paste(input$state, " crop loss bar chart ($) \n", " ", plotyear, "\n", input$commodity, sep=""), cex.axis=1.3, cex.main=1.5, width=4)


# plot(m, col = newmatrix, main = paste(input$state, " crop loss map ($) \n", " ", plotyear, "\n", input$commodity, sep=""), cex.main=1.5)
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% setView(-1.5, 53.4, 9)


}) 

output$myMap = renderLeaflet({

setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state)


setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state)
  #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
  setwd(yeardir)
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  DT <- data.table(x)
  # DT2 <- DT
  DT2 <- subset(DT, commodity == input$commodity)
  DT2loss <- DT2[,list(loss=sum(loss)), by = county]
  DT2$acres <- as.numeric(DT2$acres)
  DT2acres <- DT2[,list(acres=sum(acres)), by = county]
  DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
  DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
  DT6 <- cbind(DT2loss, DT2acres$acres)
  DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres)
  setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
  names(counties)[1] <- "county"

  m <- merge(counties, DT6, by='county')

 names(m)[7] <- "acres"


  m$loss[is.na(m$loss)] <- 0
  m$acres[is.na(m$acres)] <- 0

  tt <- colorRampPalette(c("light blue", "dark blue"), space = "Lab")
  lengacres <- length(m$acres)
  leng <- length(m$loss)
  len3 <- tt(len <- length(m$loss))
  len4 <- tt(len <- length(m$acres))


  orderedcolors2 <- tt(length(m$loss))[order(m$loss)]
  orderedcolors3 <- tt(length(m$acres))[order(m$acres)]
  m[["loss"]][is.na(m[["loss"]])] <- 0
  m[["acres"]][is.na(m[["acres"]])] <- 0

  xx <- 1
  newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
  for (jj in 1:leng){
    if (m$loss[jj] == 0) {
      #print("yes this worked, added 0")


newmatrix[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj]
      newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }}

  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

  for (jj in 1:leng){

    if (m$acres[jj] == 0) {
      newmatrix_acres[jj,] <- 0
    } else {
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }}


























pal <- colorNumeric(palette = c("blue", "red"),
                               domain = m$loss)
exte <- as.vector(extent(counties))

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(loss), weight = 1)
})
})
