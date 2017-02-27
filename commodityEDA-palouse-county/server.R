library(datasets)
library(rgdal)
library(leaflet)
library(png)
library(jpeg)
library(ncdf)   
library(data.table)
library(raster)
library(maptools)

# Define server logic required to summarize and view the selected
# dataset

shinyServer(function(input, output) {

#loadHandler <- reactive({
#  input$myLoader #create a dependency on the button, per Shiny examples.

  output$plot <- renderPlot({
#req(input$commodity)
#withProgress(message = 'Working', value = 0, {
withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
#--bringing in county shapefile



N1 <- "2001"
N2 <- "2015"
state1 <- "Washington"
state2 <- "Oregon"
state3 <- "Idaho"

idlist1 <- c("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai")
walist1 <- c("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin")
orlist1 <- c("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa")
statelist <- c("Washington", "Oregon", "Idaho")


iddlist <- c("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", "Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa")
#colnames(iddlist) <- "countyname"



idlist <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
walist <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
orlist <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")
#fulllist <- list(c(idlist, walist, orlist))

onedata <- paste("/dmine/data/USDA/agmesh-scenarios/", state1, "/summaries/", N1, "_", N2, "_usda_gridmet_", state1, sep="")
twodata <- paste("/dmine/data/USDA/agmesh-scenarios/", state2, "/summaries/", N1, "_", N2, "_usda_gridmet_", state2, sep="")
threedata <- paste("/dmine/data/USDA/agmesh-scenarios/", state3, "/summaries/", N1, "_", N2, "_usda_gridmet_", state3, sep="")

stater1 <- as.data.frame(read.csv(onedata, strip.white = TRUE))
stater2 <- as.data.frame(read.csv(twodata, strip.white = TRUE))
stater3 <- as.data.frame(read.csv(threedata, strip.white = TRUE))

threestate_summary <- rbind(stater1, stater2, stater3)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- counties[grep(paste(statelist[1], sep=""), counties@data$STATE_NAME),]
wacounties <- counties[grep(walist, counties@data$NAME),]

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


counties <- counties[grep(paste(statelist[3], sep=""), counties@data$STATE_NAME),]
idcounties <- counties[grep(idlist, counties@data$NAME),]

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


counties <- counties[grep(paste(statelist[2], sep=""), counties@data$STATE_NAME),]
orcounties <- counties[grep(orlist, counties@data$NAME),]



bb <- data.frame(wacounties$FIPS)
names(bb) <- "FIPS"
cc <- data.frame(idcounties$FIPS)
names(cc) <- "FIPS"
dd <- data.frame(orcounties$FIPS)
names(dd) <- "FIPS"

alll <- rbind(bb, cc, dd)
all2 <- t(alll)
all3 <- paste(all2, sep="|", collapse="|")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


allcounties <- counties[grep(all3, counties@data$FIPS),]








#--read 2001-2015 data summary table for the palouse
setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/summary"))
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/palouse/summaries")

palouse3 <- read.csv("2001_2015_palouse_summary")
palouse3 <- data.table(palouse3)
palouse3$date <-as.numeric(paste(palouse3$year, ".", palouse3$monthcode, sep=""))

palouse4 <- subset(palouse3, year >= input$firstyear & year <= input$lastyear)

palouse4 <- subset(palouse4, commodity == "WHEAT")

palouse4 <- subset(palouse4, county == input$county)

#palouse3$date <- as.numeric(palouse3$date)
#mydates$Months <- factor(mydates$Months, levels = paste(month.abb, c(rep(12, 12), 13)))




setwd(dirname2)

files  <- list.files(pattern = '\\_summary')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)

combined2.df <- subset(combined.df, year >= input$firstyear & year <= input$lastyear)
combined2.df <- data.table(combined2.df)

combined2.df$month <- trimws(combined2.df$month)
combined2.df$numeric_month <- match(combined2.df$month, tolower(month.abb))



combined2.df$monthyear <- NA
combined2.df <- transform(combined2.df, monthyear=paste(year, numeric_month, sep="."))

combined2.df <- data.table(combined2.df)

#combined2.df$monthyear <- cbind(combined2.df$year, numeric_month)

#combined4cov.df <- combined2.df[,list(eval(parse(text=input$climate))=sd(eval(parse(text=input$climate)))/mean(eval(parse(text=input$climate)))*100), by monthyear]
#combined4cov.df <- combined2.df[,list([[input$climate]]=sd[[input$climate]]/mean[[input$climate]]*100), by = monthyear]
#combined4mean.df <- combined2.df[,list([[input$climate]]=mean[[input$climate]]), by = monthyear]

#combined4mean.df <- combined2.df[,list(eval(parse(text=input$climate))=mean(eval(parse(text=input$climate))), by = monthyear]


combined3cov.df  <- combined2.df[,list(pr=sd(pr)/mean(pr)*100, tmmx=sd(tmmx)/mean(tmmx)*100), by = monthyear]
combined3mean.df  <- combined2.df[,list(pr=mean(pr), tmmx=mean(tmmx)), by = monthyear]

#variable <- paste("combined3", input$var, ".df$", input$climate,  sep=""), 
#newone <- get(variable)

layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))

#-by year
palouse_loss_year1 <- palouse4[,list(loss=sum(loss)), by = date]
barplot(palouse_loss_year1$loss)

#-by county
palouse_loss_county <- palouse4[,list(loss=sum(loss)), by = county]

tt <- colorRampPalette(c("light blue", "red"))

orderedcolors2 <- tt(length(palouse_loss_county$loss))[order(palouse_loss_county$loss)]
palouse_loss_county <- cbind(palouse_loss_county, orderedcolors2)
names(allcounties)[1] <- "county"
palouse_counties <- merge(allcounties, palouse_loss_county, by = 'county')
plot(palouse_counties, col = palouse_loss_county$orderedcolors2)

plot(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))), ylab=input$climate, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""))
#points(paste("combined3", input$var,  ".df", sep=""))
#lines(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))))
lines(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))), ylab=input$climate, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""))
 
#barplot(palouse2$loss)




#---------end
#setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries2/")
#climate_summary <- read.csv(paste("/dmine/data/USDA/agmesh-scenarios/palouse/summaries2/Oregon_2008_palouse_summary"))


 
})
})



output$plot2aa <- renderDataTable({
req(input$commdity)
  withProgress(message = 'Working', value = 0, {
    i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state)
  #counties <- counties[grep(input$state, counties@data$STATE_NAME),]


    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")

    setwd(yeardir)
    x <- as.data.frame(read.csv(i, strip.white = TRUE))
    DT <- data.table(x)

    #DTnew <- tolower(DT$commodity)

    #simpleCap <- function(x) {
    #  s <- strsplit(x, " ")[[1]]
    #  paste(toupper(substring(s, 1,1)), substring(s, 2),
    #        sep="", collapse=" ")
    #}

    #DTnew1a <- data.frame(sapply(DTnew,simpleCap))
    #colnames(DTnew1a) <- c("commodity_new")
    #DTnew3 <- cbind(DT, DTnew1a)

    # DTnew3 <- DT
    #  DTnew3$commodity <- DTnew3$commodity_new

    #--change to lowercase DT2!!
    #DT2 <- DT
    DT2 <- subset(DT, damagecause == input$damage)
    #DT2 <- subset(DTnew3, commodity == input$commodity)
    #DT3 <- data.frame(DT2$acres, DT2$loss)
    #DT4 <- cbind(x, DT3)
    DT2loss <- DT2[,list(loss=sum(loss)), by = county]
    #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
    DT2acres <- DT2[,list(acres=sum(acres)), by = county]
    #DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
    #DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
    #lengthDT2 <- length(DT2)
    #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1)
    DT6 <- cbind(DT2loss, DT2acres$acres)
    #--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
    #DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres)
    setnames(DT6, c("COUNTY", "LOSS", "ACRES"))

    #v <- data.frame(input$state, input$county, input$year)
    #colnames(v) <- c("State", "County", "Year")
    DT6
  })
})


output$plot2 <- renderPlot({
req(input$commodity)
    #withProgress(message = 'Working', value = 0, {

    setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep=""))

    DT <- read.csv(paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep=""), strip.white = TRUE)

   # DT <- data.table(DT)

    #DTnew <- data.frame(tolower(DT$commodity))

    #capFirst <- function(s) {
    
    #  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
    #}
    
 
    #DTnew2 <- data.frame(capFirst(DTnew$tolower.DT.commodity.))
    #colnames(DTnew2) <- c("commodity")
    #DTnew2$commodity <- as.character(DTnew2$commodity)

    #DTnew2$commodity[DTnew2$commodity == "All other crops"] <- "All Other Crops"

    #DTnew3 <- cbind(DT, DTnew2)
    #DTnew3$commodity <- DTnew3$commodity2

    commodity_result <- subset(DT, commodity == input$commodity) 
    library(lattice)
    bwplot(damagecause ~ log(loss), data=commodity_result, scales=list(x=list(rot=90)), main = paste(input$state, " damage cause for ", input$month, ", ",  input$year, "\n", "Commodity: ",  input$commodity, sep="") )   
  })





  output$plot2_backup <- renderPlot({
req(input$commodity)
    #withProgress(message = 'Working', value = 0, {

    setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep=""))

    DT <- read.csv(paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep=""), strip.white = TRUE)

   # DT <- data.table(DT)

    DTnew <- data.frame(tolower(DT$commodity))

    capFirst <- function(s) {
      paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
    }

    DTnew2 <- data.frame(capFirst(DTnew$tolower.DT.commodity.))
    colnames(DTnew2) <- c("commodity")
    DTnew2$commodity <- as.character(DTnew2$commodity)

    DTnew2$commodity[DTnew2$commodity == "All other crops"] <- "All Other Crops"

    DTnew3 <- cbind(DT, DTnew2)
    DTnew3$commodity <- DTnew3$commodity2

    commodity_result <- subset(DTnew3, commodity == input$commodity) 
    library(lattice)
    bwplot(damagecause ~ log(loss), data=commodity_result, scales=list(x=list(rot=90)), main = paste(input$state, " damage cause for ", input$month, ", ",  input$year, "\n", "Commodity: ",  input$commodity, sep="") )   
  })

  output$plot3 <- renderPlot({
    req(input$commodity)
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
    setwd(yeardir)
    
    plotmonth <- month.abb[input$month]

    i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
    commodity <- read.csv(paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep=""), strip.white = TRUE)
    #commodity <- as.data.frame(read.csv(i, strip.white = TRUE))
    commodity_year <- subset(commodity, year == input$year & monthcode == input$month)
    library(lattice)
    bwplot(damagecause ~ log(loss), data=commodity_year, scales=list(x=list(rot=90)), main = paste(input$state, " damage cause for ", plotmonth, ", ",  input$year, "\n", "Commodity: ",  input$commodity, sep="") )
  })





output$plot4 <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {

 setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
#i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")

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

  #DTnew <- tolower(DT$commodity)

  #simpleCap <- function(x) {
  #  s <- strsplit(x, " ")[[1]]
  #  paste(toupper(substring(s, 1,1)), substring(s, 2),
  #        sep="", collapse=" ")
  #}

  #DTnew1a <- data.frame(sapply(DTnew,simpleCap))
  #colnames(DTnew1a) <- c("commodity_new")
  #DTnew3 <- cbind(DT, DTnew1a)

  # DTnew3 <- DT
#  DTnew3$commodity <- DTnew3$commodity_new

#--change to lowercase DT2!!
  #DT2 <- DT
  DT2 <- subset(DT, commodity == input$commodity)
  DT2 <- subset(DT2, damagecause == input$damage)
  #DT2 <- subset(DTnew3, commodity == input$commodity)
  #DT3 <- data.frame(DT2$acres, DT2$loss)
  #DT4 <- cbind(x, DT3)
  DT2loss <- DT2[,list(loss=sum(loss)), by = county]
  #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
  DT2acres <- DT2[,list(acres=sum(acres)), by = county]
  DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
  DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
  #lengthDT2 <- length(DT2)
  #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1)
  DT6 <- cbind(DT2loss, DT2acres$acres)
  #--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
  DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres)
  setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
  #m <- subset(x, county = "ID")
  names(counties)[1] <- "county"
  #colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")

  #colnames(u) <- c("NAME")
  #z <- cbind(u,DT)
  m <- merge(counties, DT6, by='county')
  names(m)[7] <- "acres"


  m$loss[is.na(m$loss)] <- 0
  #m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  m$acres[is.na(m$acres)] <- 0

  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("blue", "orange", "red"))
  #mz <- subset(m, LOSS != 0)
  #mzacres <- subset(m, acres > 0)
  lengacres <- length(m$acres)
  leng <- length(m$loss)
  #len2 <- tt(len <- length(mz$loss))
  #len2acres <- tt(len <- length(mzacres$acres))
  #len2a <- length(mz$loss)
  #len2a <- length(mzacres$acres)
  len3 <- tt(len <- length(m$loss))
  len4 <- tt(len <- length(m$acres))

  orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
  orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
  #newframe <- data.frame(m$LOSS)
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
      #print("yes this worked, added 0")
      newmatrix_acres[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
     # newmatrix[jj,] <- len4[jj]
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }}


  #newmatrix[newmatrix==0] <- NA
  #newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  #newmatrix[newmatrix == NA] <- 0
  #newmatrix <- c(newmatrix)

  #newmatrix_acres[newmatrix_acres==0] <- NA
  #newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]

  #newmatrix2acres <- subset(newmatrix = TRUE)
  #newmatrix_acres[newmatrix_acres == NA] <- 0
  #newmatrix_acres <- c(newmatrix_acres)


  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,3,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
  #--turn image horizontal

  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]

  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)

  b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix, horiz=TRUE)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))

  bb <- barplot(m$acres, names.arg = m$county, las=2, col = newmatrix_acres, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
  plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})

})









output$plot4_backup <- renderPlot({
req(input$commodity)
withProgress(message = 'Working', value = 0, {


 setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))








setwd(yeardir)
i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
#i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")

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

  DTnew <- data.frame(tolower(DT$commodity))

  capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
  }

  DTnew2 <- data.frame(capFirst(DTnew$tolower.DT.commodity.))
  colnames(DTnew2) <- c("commodity")
  DTnew2$commodity <- as.character(DTnew2$commodity)

  DTnew2$commodity[DTnew2$commodity == "All other crops"] <- "All Other Crops"

  DTnew3 <- cbind(DT, DTnew2)
  DTnew3$commodity <- DTnew3$commodity2

#--change to lowercase DT2!!
  DT2 <- subset(DTnew3, commodity == input$commodity)
  #DT3 <- data.frame(DT2$acres, DT2$loss)
  #DT4 <- cbind(x, DT3)
  DT2loss <- DT2[,list(loss=sum(loss)), by = county]
  #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
  DT2acres <- DT2[,list(acres=sum(acres)), by = county]
  #lengthDT2 <- length(DT2)
  #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1) 
  DT6 <- cbind(DT2loss, DT2acres$acres) 
  #m <- subset(x, county = "ID")
  names(counties)[1] <- "county"
  #colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")
  #u <- data.frame(trimws(counties$county))
  #colnames(u) <- c("NAME")
  #z <- cbind(u,DT)
  m <- merge(counties, DT6, by='county')
  names(m)[7] <- "acres" 
  

  m$loss[is.na(m$loss)] <- 0
  #m$commoditycode[is.na(m$commoditycode)] <- 0
  m$acres[is.na(m$acres)] <- 0
  
  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$loss))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("blue", "orange", "red"))
  #mz <- subset(m, loss != 0)
  #mzacres <- subset(m, acres > 0)
  lengacres <- length(m$acres)
  leng <- length(m$loss)
  #len2 <- tt(len <- length(mz$loss))
  #len2acres <- tt(len <- length(mzacres$acres))
  #len2a <- length(mz$loss)
  #len2a <- length(mzacres$acres)
  len3 <- tt(len <- length(m$loss))
  len4 <- tt(len <- length(m$acres))
 
  orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
  orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
  #newframe <- data.frame(m$loss)
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
    }
    
  }
  
  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
  
  for (jj in 1:leng){
    
    if (m$acres[jj] == 0) {
      #print("yes this worked, added 0")
      newmatrix_acres[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len4[jj] 
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }
    
  }
 
 
  #newmatrix[newmatrix==0] <- NA
  #newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  #newmatrix[newmatrix == NA] <- 0
  #newmatrix <- c(newmatrix)
  
  #newmatrix_acres[newmatrix_acres==0] <- NA
  #newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2acres <- subset(newmatrix = TRUE)
  #newmatrix_acres[newmatrix_acres == NA] <- 0
  #newmatrix_acres <- c(newmatrix_acres)
  
  
  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,3,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
  #--turn image horizontal
  
  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]
  
  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)
  
  b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix, horiz=TRUE)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
  
  bb <- barplot(m$acres, names.arg = m$county, las=2, col = newmatrix_acres, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})  
})


output$plot5aa <- renderPlot({
#req(input$commodity)
  withProgress(message = 'Working', value = 0, {

library(DAAG)


setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
files <- list.files(pattern = "\\_WHEAT_drought$")
myfiles = do.call(rbind, lapply(files, function(x) 
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year") 
myfiles$prpet <- (myfiles$pr - myfiles$pet)
#write.csv(myfiles, file = "WHEAT_drought_summary")
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss_unscaled <- myfiles_allyears$loss
myfiles_allyears$countratio <- scale(myfiles_allyears$countratio, center = TRUE, scale = FALSE)
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)

#--allyears pairwise plot

#--countratio

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
lmcount <- lm(as.formula(paste(input$predictor," ~ ",paste(input$climate,collapse="+"))), data=myfiles_allyears)
cv.lm(data=myfiles_allyears, lmcount, m=3, main = "Wheat loss regression 2007-2015")

summary(lmcount) 
})
})

output$plot5ab <- renderPrint({
#req(input$commodity)
  withProgress(message = 'Working', value = 0, {

library(DAAG)
library(stats)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
files <- list.files(pattern = "\\_WHEAT_drought$")
myfiles = do.call(rbind, lapply(files, function(x)
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year")
myfiles$prpet <- (myfiles$pr - myfiles$pet)
#write.csv(myfiles, file = "WHEAT_drought_summary")
#myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss_unscaled <- myfiles_allyears$loss
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)

#--allyears pairwise plot

#--countratio

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex=cex, col=2)
}
lmcount <- lm(as.formula(paste(input$predictor," ~ ",paste(input$climate,collapse="+"))), data=myfiles_allyears)
#cv.lm(data=myfiles_allyears, lmcount, m=3, main = "Wheat loss regression 2007-2015") 
summary(lmcount)
})
})





















output$plot5 <- renderPlot({
req(input$commodity)
  withProgress(message = 'Working', value = 0, {

 setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))








setwd(yeardir)
i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
#i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")

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

  #DTnew <- tolower(DT$commodity)
  
  #simpleCap <- function(x) {
  #  s <- strsplit(x, " ")[[1]]
  #  paste(toupper(substring(s, 1,1)), substring(s, 2),
  #        sep="", collapse=" ")
  #}
  
  #DTnew1a <- data.frame(sapply(DTnew,simpleCap))
  #colnames(DTnew1a) <- c("commodity_new")
  #DTnew3 <- cbind(DT, DTnew1a)

  # DTnew3 <- DT
#  DTnew3$commodity <- DTnew3$commodity_new
  
#--change to lowercase DT2!!
  #DT2 <- DT
  DT2 <- subset(DT, county == input$county)
  DT2 <- subset(DT2, commodity == input$commodity)
  #DT2 <- subset(DTnew3, commodity == input$commodity)
  #DT3 <- data.frame(DT2$acres, DT2$loss)
  #DT4 <- cbind(x, DT3)
  DT2loss <- DT2[,list(loss=sum(loss)), by = county]
  #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
  DT2acres <- DT2[,list(acres=sum(acres)), by = county]
  DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
  DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
  #lengthDT2 <- length(DT2)
  #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1) 
  DT6 <- cbind(DT2loss, DT2acres$acres)
  #--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
  DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres) 
  setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
  #m <- subset(x, county = "ID")
  names(counties)[1] <- "county"
  #colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")

  #colnames(u) <- c("NAME")
  #z <- cbind(u,DT)
  m <- merge(counties, DT6, by='county')
  names(m)[7] <- "acres" 
  

  m$loss[is.na(m$loss)] <- 0
  #m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  m$acres[is.na(m$acres)] <- 0
  
  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("blue", "orange", "red"))
  #mz <- subset(m, LOSS != 0)
  #mzacres <- subset(m, acres > 0)
  lengacres <- length(m$acres)
  leng <- length(m$loss)
  #len2 <- tt(len <- length(mz$loss))
  #len2acres <- tt(len <- length(mzacres$acres))
  #len2a <- length(mz$loss)
  #len2a <- length(mzacres$acres)
  len3 <- tt(len <- length(m$loss))
  len4 <- tt(len <- length(m$acres))
 
  orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
  orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
  #newframe <- data.frame(m$LOSS)
  m[["loss"]][is.na(m[["loss"]])] <- 0
  m[["acres"]][is.na(m[["acres"]])] <- 0 
  xx <- 1
  newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
 
  for (jj in 1:leng){
    #if (DT7$LOSS[jj] == 0) {
      #print("yes this worked, added 0")
     # newmatrix[jj,] <- 0
    #} else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj] 
      newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }
  
  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
  
  for (jj in 1:leng){
    
    #if (DT7$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
     # newmatrix_acres[jj,] <- 0
    #} else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len4[jj] 
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }
 
 
  #newmatrix[newmatrix==0] <- NA
  #newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  #newmatrix[newmatrix == NA] <- 0
  #newmatrix <- c(newmatrix)
  
  #newmatrix_acres[newmatrix_acres==0] <- NA
  #newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2acres <- subset(newmatrix = TRUE)
  #newmatrix_acres[newmatrix_acres == NA] <- 0
  #newmatrix_acres <- c(newmatrix_acres)
  
  
  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(5,9,3,1)+1)
  par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 2, byrow=TRUE))
  #--turn image horizontal
  
  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]
  
  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)

  options(scipen=5)
  b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix, horiz=TRUE, cex.names = 0.81, cex.axis = 0.8, main = paste(input$commodity, " - Damage Report", sep=""))
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  #plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
  par(mar=c(0,0,3,1)+1)

   setwd("/dmine/data/counties/")

    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)
    plot(counties, main = paste("State: ", input$state, "\n   County: ", input$county, "   Year: ", input$year, sep=""))
    plot(counties_one, col="blue", add=T)


  
  #bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})  

})

output$plot5y <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {
    
    setwd("/dmine/data/counties/")
    
    counties <- readShapePoly('UScounties.shp', 
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
    #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
    counties <- subset(counties, STATE_NAME %in% input$state)
    monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
    #uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
    maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
    #setwd(monthdir)
    #system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")
    
    setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
    system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
    uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
    
    
    
    
    
    
    
    
    setwd(yeardir)
    i <- paste("2001_2015_usda_gridmet_", input$state, sep="")
    #i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")
    
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
    
    #DTnew <- tolower(DT$commodity)
    
    #simpleCap <- function(x) {
    #  s <- strsplit(x, " ")[[1]]
    #  paste(toupper(substring(s, 1,1)), substring(s, 2),
    #        sep="", collapse=" ")
    #}
    
    #DTnew1a <- data.frame(sapply(DTnew,simpleCap))
    #colnames(DTnew1a) <- c("commodity_new")
    #DTnew3 <- cbind(DT, DTnew1a)
    
    # DTnew3 <- DT
    #  DTnew3$commodity <- DTnew3$commodity_new
    
    #--change to lowercase DT2!!
    #DT2 <- DT
    #DT2 <- subset(DT, county == input$county)
    DT2 <- subset(DT, commodity == input$commodity)
    #DT2 <- subset(DTnew3, commodity == input$commodity)
    #DT3 <- data.frame(DT2$acres, DT2$loss)
    #DT4 <- cbind(x, DT3)
    #DT2count <- DT2[,list(loss=count(loss)), by = county]
    DT2loss <- DT2[,list(loss=sum(loss)), by = county]
    #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
    DT2acres <- DT2[,list(acres=sum(acres)), by = county]
    DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
    DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
    #lengthDT2 <- length(DT2)
    #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1) 
    DT6 <- cbind(DT2loss, DT2acres$acres)
    #--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
    DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres) 
    setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
    #m <- subset(x, county = "ID")
    names(counties)[1] <- "county"
    #colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")
    
    #colnames(u) <- c("NAME")
    #z <- cbind(u,DT)
    m <- merge(counties, DT6, by='county')
    names(m)[7] <- "acres" 
    
    
    m$loss[is.na(m$loss)] <- 0
    #m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
    m$acres[is.na(m$acres)] <- 0
    
    #shapefile(m)
    #--begin polygon work
    #length(na.omit(m$LOSS))
    #tt <- colorRampPalette(brewer.pal(11, "Spectral")
    tt <- colorRampPalette(c("blue", "orange", "red"))
    #mz <- subset(m, LOSS != 0)
    #mzacres <- subset(m, acres > 0)
    lengacres <- length(m$acres)
    leng <- length(m$loss)
    #len2 <- tt(len <- length(mz$loss))
    #len2acres <- tt(len <- length(mzacres$acres))
    #len2a <- length(mz$loss)
    #len2a <- length(mzacres$acres)
    len3 <- tt(len <- length(m$loss))
    len4 <- tt(len <- length(m$acres))
    
    orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
    orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
    #newframe <- data.frame(m$LOSS)
    m[["loss"]][is.na(m[["loss"]])] <- 0
    m[["acres"]][is.na(m[["acres"]])] <- 0 
    xx <- 1
    newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
    
    for (jj in 1:leng){
      #if (DT7$LOSS[jj] == 0) {
      #print("yes this worked, added 0")
      # newmatrix[jj,] <- 0
      #} else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj] 
      newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }
    
    xx <- 1
    newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
    
    for (jj in 1:leng){
      
      #if (DT7$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
      # newmatrix_acres[jj,] <- 0
      #} else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len4[jj] 
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }
    
    
    #newmatrix[newmatrix==0] <- NA
    #newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
    #newmatrix2 <- subset(newmatrix = TRUE)
    #newmatrix[newmatrix == NA] <- 0
    #newmatrix <- c(newmatrix)
    
    #newmatrix_acres[newmatrix_acres==0] <- NA
    #newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
    #newmatrix2acres <- subset(newmatrix = TRUE)
    #newmatrix_acres[newmatrix_acres == NA] <- 0
    #newmatrix_acres <- c(newmatrix_acres)
    
    
    #orderedcolors2 <- colorRampPalette(c(44))
    #m <- cbind(m$LOSS, newmatrix)
    #midpoints <- barplot(mz$LOSS)
    #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
    par(mar=c(5,11,3,1)+1)
    par(mfrow=c(1,2))
    layout(matrix(c(1,2,3,4),1, 1, byrow=TRUE))
    #--turn image horizontal
    
    plotmonth <- month.abb[x$monthcode[1]]
    plotyear <- x$year[1]
    plotcommodity <- x$commodity[1]
    
    midpoint_loss <- (max(m$loss) + min(m$loss)/2)
    midpoint_acres <- (max(m$acres) + min(m$acres)/2)
    
    options(scipen=5)
    b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix, horiz=TRUE, cex.names = 0.9, cex.axis = 0.8, main = paste(input$commodity, " - Damage Report", sep=""))
    #text(bb, midpoint_loss, labels=mz$loss, srt=90)
    #plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
    par(mar=c(0,0,3,1)+1)
    
    setwd("/dmine/data/counties/")
    
    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)
    #plot(counties, main = paste("State: ", input$state, "\n   County: ", input$county, "   Year: ", input$year, sep=""))
    #plot(counties_one, col="blue", add=T)
    
    
    
    #bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres, horiz=TRUE)
    #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
    #plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
  })  
  
})




output$plot5xx <- renderPlot({
  #req(input$commodity)
  withProgress(message = 'Working', value = 0, {

library(DAAG)
library(stats)
library(treemap)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
files <- list.files(pattern = "\\_WHEAT_drought$")
myfiles = do.call(rbind, lapply(files, function(x)
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year")
myfiles$prpet <- (myfiles$pr - myfiles$pet)
#write.csv(myfiles, file = "WHEAT_drought_summary")
#myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss_unscaled <- myfiles_allyears$loss
#myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)

#--allyears pairwise plot

#--countratio

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex=cex, col=2)
}



treemap(myfiles_allyears, #Your data frame object
        index=c("county", "year"),  #A list of your categorical variables
        vColor= "year",
        vSize = input$predictor,  #This is your quantitative variable
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = c("Blues"),  #Select your color palette from the RColorBrewer presets or make your own.
        title="Crop Loss by county and year", #Customize your title
        fontsize.title = 14 #Change the font size of the title
        )






})
})

output$plot5nn <- renderPlot({
#  req(input$commodity)
  withProgress(message = 'Working', value = 0, {


library(DAAG)
library(stats)
library(neuralnet)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
files <- list.files(pattern = "\\_WHEAT_drought$")
myfiles = do.call(rbind, lapply(files, function(x)
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year")
myfiles$prpet <- (myfiles$pr - myfiles$pet)
#write.csv(myfiles, file = "WHEAT_drought_summary")
#myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss_unscaled <- myfiles_allyears$loss
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)

#--allyears pairwise plot

#--countratio

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex=cex, col=2)
}

set.seed(500)
#data <- myfiles_allyears[1:7]
#data_orig <- myfiles_allyears[1:6]
yearpred <- myfiles_allyears[input$predictor] 
data <- cbind(myfiles_allyears[input$climate], yearpred) 
#colnames(data)[12] <- c(input$predictor)
colnames(data[ncol(data)]) <- c(input$predictor)

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
#newone <- colnames(data[7])
lm.fit <- glm(as.formula(paste(input$predictor, " ~.", sep="")), data=train)
#summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test[input$predictor])^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
setwd("/tmp")
train_ <- scaled[index,]
test_ <- scaled[-index,]
library(neuralnet)
n <- names(train_)
f <- as.formula(paste(input$predictor, " ~", paste(n[!n %in% input$predictor], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
#par(mfrow = c(2, 1))
plot(nn, rep="best")

pr.nn <- compute(nn,test_[input$climate])

pr.nn_ <- pr.nn$net.result*(max(data[input$predictor])-min(data[input$predictor]))+min(data[input$predictor])
test.r <- (test_[input$predictor])*(max(data[input$predictor])-min(data[input$predictor]))+min(data[input$predictor])

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)


#plot(test(paste("test$", input$predictor, sep=""), pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='NN',pch=18,col='red', bty='n')

#plot(test[input$predictor],pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)




})
})


output$plot5nn2 <- renderPlot({
#  req(input$commodity)
  withProgress(message = 'Working', value = 0, {


library(DAAG)
library(stats)
library(neuralnet)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
files <- list.files(pattern = "\\_WHEAT_drought$")
myfiles = do.call(rbind, lapply(files, function(x)
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year")
myfiles$prpet <- (myfiles$pr - myfiles$pet)
#write.csv(myfiles, file = "WHEAT_drought_summary")
#myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss_unscaled <- myfiles_allyears$loss
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)
myfiles_allyears$countratio <- scale(myfiles_allyears$countratio, center = TRUE, scale = FALSE)

#--allyears pairwise plot

#--countratio

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex=cex, col=2)
}

set.seed(500)
#data <- myfiles_allyears[1:7]
#data_orig <- myfiles_allyears[1:6]
yearpred <- myfiles_allyears[input$predictor]
data <- cbind(myfiles_allyears[input$climate], yearpred)
#colnames(data)[12] <- c(input$predictor)
colnames(data[ncol(data)]) <- c(input$predictor)

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
#newone <- colnames(data[7])
lm.fit <- glm(as.formula(paste(input$predictor, " ~.", sep="")), data=train)
#summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test[input$predictor])^2)/nrow(test)

maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
setwd("/tmp")
train_ <- scaled[index,]
test_ <- scaled[-index,]
library(neuralnet)
n <- names(train_)
f <- as.formula(paste(input$predictor, " ~", paste(n[!n %in% input$predictor], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
par(mfrow = c(1,2))
#plot(nn, rep="best")

pr.nn <- compute(nn,test_[input$climate])

pr.nn_ <- pr.nn$net.result*(max(data[input$predictor])-min(data[input$predictor]))+min(data[input$predictor])
test.r <- (test_[input$predictor])*(max(data[input$predictor])-min(data[input$predictor]))+min(data[input$predictor])

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

plot(test[ncol(test)],pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test[ncol(test)],pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)




})
})













output$plot5x <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {
    
    setwd("/dmine/data/counties/")
    
    counties <- readShapePoly('UScounties.shp', 
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
    #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
    counties <- subset(counties, STATE_NAME %in% input$state)
    monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
    #uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
    maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
    #setwd(monthdir)
    #system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")
    
    setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
    system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
    uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
    
    
    
    
    
    
    
    
    setwd(yeardir)
    i <- paste("2001_2015_usda_gridmet_", input$state, sep="")
    #i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")
    
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
    
    #DTnew <- tolower(DT$commodity)
    
    #simpleCap <- function(x) {
    #  s <- strsplit(x, " ")[[1]]
    #  paste(toupper(substring(s, 1,1)), substring(s, 2),
    #        sep="", collapse=" ")
    #}
    
    #DTnew1a <- data.frame(sapply(DTnew,simpleCap))
    #colnames(DTnew1a) <- c("commodity_new")
    #DTnew3 <- cbind(DT, DTnew1a)
    
    # DTnew3 <- DT
    #  DTnew3$commodity <- DTnew3$commodity_new
    
    #--change to lowercase DT2!!
    #DT2 <- DT
    #DT2 <- subset(DT, county == input$county)
    DT2 <- subset(DT, commodity == input$commodity)
    #DT2 <- subset(DTnew3, commodity == input$commodity)
    #DT3 <- data.frame(DT2$acres, DT2$loss)
    #DT4 <- cbind(x, DT3)
    DT2loss <- DT2[,list(loss=sum(loss)), by = county]
    #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
    DT2acres <- DT2[,list(acres=sum(acres)), by = county]
    DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
    DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
    #lengthDT2 <- length(DT2)
    #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1) 
    DT6 <- cbind(DT2loss, DT2acres$acres)
    #--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
    DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres) 
    setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
    #m <- subset(x, county = "ID")
    names(counties)[1] <- "county"
    #colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")
    
    #colnames(u) <- c("NAME")
    #z <- cbind(u,DT)
    m <- merge(counties, DT6, by='county')
    names(m)[7] <- "acres" 
    
    
    m$loss[is.na(m$loss)] <- 0
    #m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
    m$acres[is.na(m$acres)] <- 0
    
    #shapefile(m)
    #--begin polygon work
    #length(na.omit(m$LOSS))
    #tt <- colorRampPalette(brewer.pal(11, "Spectral")
    tt <- colorRampPalette(c("blue", "orange", "red"))
    #mz <- subset(m, LOSS != 0)
    #mzacres <- subset(m, acres > 0)
    lengacres <- length(m$acres)
    leng <- length(m$loss)
    #len2 <- tt(len <- length(mz$loss))
    #len2acres <- tt(len <- length(mzacres$acres))
    #len2a <- length(mz$loss)
    #len2a <- length(mzacres$acres)
    len3 <- tt(len <- length(m$loss))
    len4 <- tt(len <- length(m$acres))
    
    orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
    orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
    #newframe <- data.frame(m$LOSS)
    m[["loss"]][is.na(m[["loss"]])] <- 0
    m[["acres"]][is.na(m[["acres"]])] <- 0 
    xx <- 1
    newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
    
    for (jj in 1:leng){
      #if (DT7$LOSS[jj] == 0) {
      #print("yes this worked, added 0")
      # newmatrix[jj,] <- 0
      #} else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj] 
      newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }
    
    xx <- 1
    newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
    
    for (jj in 1:leng){
      
      #if (DT7$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
      # newmatrix_acres[jj,] <- 0
      #} else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len4[jj] 
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }
    
    
    #newmatrix[newmatrix==0] <- NA
    #newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
    #newmatrix2 <- subset(newmatrix = TRUE)
    #newmatrix[newmatrix == NA] <- 0
    #newmatrix <- c(newmatrix)
    
    #newmatrix_acres[newmatrix_acres==0] <- NA
    #newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
    #newmatrix2acres <- subset(newmatrix = TRUE)
    #newmatrix_acres[newmatrix_acres == NA] <- 0
    #newmatrix_acres <- c(newmatrix_acres)
    
    
    #orderedcolors2 <- colorRampPalette(c(44))
    #m <- cbind(m$LOSS, newmatrix)
    #midpoints <- barplot(mz$LOSS)
    #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
    par(mar=c(5,11,3,1)+1)
    par(mfrow=c(1,2))
    layout(matrix(c(1,2,3,4),1, 1, byrow=TRUE))
    #--turn image horizontal
    
    plotmonth <- month.abb[x$monthcode[1]]
    plotyear <- x$year[1]
    plotcommodity <- x$commodity[1]
    
    midpoint_loss <- (max(m$loss) + min(m$loss)/2)
    midpoint_acres <- (max(m$acres) + min(m$acres)/2)
    
    options(scipen=5)
    b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix, horiz=TRUE, cex.names = 0.9, cex.axis = 0.8, main = paste(input$commodity, " - Damage Report", sep=""))
    #text(bb, midpoint_loss, labels=mz$loss, srt=90)
    #plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
    par(mar=c(0,0,3,1)+1)
    
    setwd("/dmine/data/counties/")
    
    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)
    #plot(counties, main = paste("State: ", input$state, "\n   County: ", input$county, "   Year: ", input$year, sep=""))
    #plot(counties_one, col="blue", add=T)
    
    
    
    #bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres, horiz=TRUE)
    #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
    #plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
  })  
  
})







output$plot6 <- renderPlot({
req(input$commodity)

  library(rnoaa)

  withProgress(message = 'Working', value = 0, {

  setwd("/dmine/data/ghcnd/ghcnd_admin/")
  ghcnd_stations <- read.csv("ghcnd-stations_revised.csv")

  #coordinates(ghcnd_stations) <- ~long + lat

  xy <- ghcnd_stations[,c(3,2)]

  spdf <- SpatialPointsDataFrame(coords = xy, data = ghcnd_stations,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  columns <- c("ID", "YEAR", "MONTH", "ELEMENT", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN", "EVAP", "MNPN", "MXPN", "SN*#", "SX*#", "TOBS", "WDMV", "WESF", "WT**", "WTEQ", "VALUE1", "MFLAG1", "QFLAG1", "SFLAG1", "VALUE2", "MFLAG2", "QFLAG2", "SFLAG2")


  setwd("/dmine/data/counties/")

  counties <- readShapePoly('UScounties.shp', 
                         proj4string=CRS
                         ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  counties <- counties[grep(input$state, counties@data$STATE_NAME),]
  counties <- counties[grep(input$county, counties@data$NAME),]

  subset_stations <- spdf[counties, ]


  subset_stations_data <- meteo_pull_monitors(subset_stations$station)

  datetxt <- subset_stations_data$date
  datetxt <- as.Date(datetxt)
  df <- data.frame(year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

  subset_stations_data <- cbind(subset_stations_data, df)

  subsetstations_month <- subset(subset_stations_data, month == 1)

  library(doBy)
  tmax <- summaryBy(tmax ~ year, data = subsetstations_month, 
                 FUN = list(mean, max, min, median, sd))

  library(doBy)
  prcp <- summaryBy(prcp ~ year, data = subsetstations_month, 
          FUN = list(mean, max, min, median, sd))

  plot(prcp$year, prcp$prcp.mean)

  lines(stats::lowess(prcp), col = "red")

  plot(tmax$year, tmax$tmax.mean)

  lines(stats::lowess(tmax), col = "red")
  })

})

output$plot6b <- renderPlot({
req(input$commodity)

  library(rnoaa)

  withProgress(message = 'Working', value = 0, {

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- counties[grep(input$county, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

newmat <- matrix(,ncol = 15, nrow = 12 )

newj <- 1
for (j in climz) {
  newx <- 1
 for (i in monthz) {
 setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/", sep=""))
 ncfile = paste(j, "_", i, "_", input$year, ".nc", sep="")
 rasterout <- brick(ncfile)
 rasterout <- crop(rasterout, counties)
 vect <- cellStats(rasterout, mean)
 vect2 <- mean(vect)
 newmat[newx,newj] <- vect2
 newx <- newx + 1
 }
 newj <- newj + 1
}
colnames(newmat) <- climz
rownames(newmat) <- monthz
newmat <- data.frame(newmat)

#---

i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT2, commodity == input$commodity)

newmat2 <- matrix(, ncol = 1, nrow = 12)
#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1

#--loss
 newii <- 1
 for (ii in monthzz) {
   nez <- subset(DT2, monthcode == ii)
   newmat2[ii, newjj] <- sum(nez[,32])
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
colnames(newmat4) <- c("loss", "acres")

newmat5 <- cbind(newmat, newmat4)

newmat5[is.na(newmat5)] <- 0

#library(dygraphs)
#library(xts)
#test <- cbind(newmat5$loss)
#dygraph(data.frame(test))

#newmat6 <- newmat5
#rownames(newmat6) -> data.frame(newmat7)
#test <- cbind(newmat7, newmat5$pr)
#as.xts(newmat5$pr, newmat7)


par(mar=c(1,1,1,1))
par(mfrow = c(15, 1))                 
barplot(newmat5$loss, las=2, col="blue", horiz=TRUE)
barplot(newmat5$acres, names.arg = rownames(newmat5), las=2, col="blue", horiz=TRUE)
#legend = c("Low", "Mid", "High")
#text(0,bp,round(femaleses, 1),cex=1,pos=4) # label on the bars themselvesbarplot(newmat5$pdsi, las=2, col="green")
barplot(newmat5$bi, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$th, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$tmmx, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$tmmn, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$fm100, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$fm1000, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$erc, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$pet, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$vs, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$rmin, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$rmax, names.arg = rownames(newmat5), las=2, col="green")
barplot(newmat5$sph, names.arg = rownames(newmat5), las=2, col="green")

  





})

})



output$plot7 <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {
    
    setwd("/dmine/data/counties/")
    
    counties <- readShapePoly('UScounties.shp', 
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)
    plot(counties, main = paste("Crop Commodity Loss - Damage Report\n", "State: ", input$state, "   County: ", input$county, "   Year: ", input$year, sep=""))
    plot(counties_one, col="blue", add=T)
  })
})

 output$plot7a <- renderPlot({
       req(input$commodity)
       withProgress(message = 'Working', value = 0, {
    
         setwd("/dmine/data/counties/")
    
         counties <- readShapePoly('UScounties.shp',
                                   proj4string=CRS
                                   ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
         projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
         counties <- subset(counties, STATE_NAME %in% input$state)
         #counties_one <- subset(counties, NAME %in% input$county)
         yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
         setwd(yeardir)
         zz <- as.numeric(input$month) 
         plotmonth <- month.abb[zz]

         commodity <- read.csv(paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep=""), strip.white = TRUE)

         commodity_year <- subset(commodity, year == input$year & monthcode == input$month)
         par(mfrow = c(1, 2))
         #plot(counties)
         tr <- plot(counties, main = paste("Crop Commodity Statewide Monthly Loss Report", "State: ", input$state, "   Month ", input$month, "  Year: ", input$year, "   Commodity: ", input$commodity, sep=""))
         library(lattice) 
         bwplot(damagecause ~ log(loss), data=commodity_year, scales=list(x=list(rot=90)), main = paste(input$state, " damage cause for ", plotmonth, ", ",  input$year, "\n", "Commodity: ",  input$commodity, sep="") )

         #print(tr2, position = c(0, 0, 0.5, 1), more = TRUE)
	 #print(tra, position = c(0.5, 0, 1, 1))
         #plot(counties_one, col="blue", add=T)
       })
     })


output$plot7b <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {

    setwd("/dmine/data/counties/")

    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)
    plot(counties, main = paste("Crop Commodity Loss - Annual Loss and Acreage Report\n", "State: ", input$state, "   Year: ", input$year, "   Commodity: ", input$commodity, sep=""))
   # plot(counties_one, col="blue", add=T)
  })
})

output$plotregression <- renderPlot({
 # req(input$commodity)
  withProgress(message = 'Working', value = 0, {

library(DAAG)


setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
#files <- list.files(pattern = "\\_WHEAT_drought$")
#myfiles = do.call(rbind, lapply(files, function(x) 
#  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year") 
#myfiles$prpet <- (myfiles$pr - myfiles$pet)
myfiles <- read.csv("WHEAT_drought_summary")

#myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss_unscaled <- myfiles_allyears$loss
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)

#--allyears pairwise plot

#--countratio

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
} 

yearpred <- myfiles_allyears[input$predictor] 
data <- cbind(myfiles_allyears[input$climate], yearpred)
#colnames(data)[12] <- c(input$predictor)

colnames(data[ncol(data)]) <- c(input$predictor)


pairs(data, lower.panel=panel.smooth, upper.panel=panel.cor) 
#pairs(myfiles_allyears[c(1,2,3,4,5,6,8)], lower.panel=panel.smooth, upper.panel=panel.cor)


#pairs(myfiles_allyears[c(1,2,3,4,5,6,8)], lower.panel=panel.smooth, upper.panel=panel.cor)
  })
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
counties <- counties[grep(input$county, counties@data$NAME),]

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



setwd("/dmine/data/counties/")

    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))                 

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 1, byrow=TRUE))

 

barplot(newmat5$claims, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), cex.names=1, las=3, col="blue", main = paste(input$state, " crop claim counts  \n", input$month, " ", input$year, "\n", input$commodity, sep=""), horiz=FALSE)

#par(mar=c(0,0,3,1)+1)

#plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county, " County map", sep=""))

#plot(counties_one, col="blue", add=T)

#plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
#axis(side=1, at=c(1:12))
#axis(side=2, at=seq(xxx, xxxx, by = interval))
#lines(newmat5$pr, las=2, col="blue")

  })
})


output$plot7dd <- renderPlot({
 # req(input$commodity)
  withProgress(message = 'Working', value = 0, {


library(DAAG)
library(rpart.plot)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
files <- list.files(pattern = "\\_WHEAT_drought$")
myfiles = do.call(rbind, lapply(files, function(x) 
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year") 
myfiles$prpet <- (myfiles$pr - myfiles$pet)
#write.csv(myfiles, file = "WHEAT_drought_summary")
#myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss_unscaled <- myfiles_allyears$loss
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)

#--allyears pairwise plot

#--countratio

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

#attach(myfiles_allyears)

# Make big tree

form <- as.formula(paste(input$predictor, " ~ ", paste(input$climate, collapse="+")))

#form <- as.formula(loss ~ tmmx + pr + pdsi + prpet + erc, data=myfiles_allyears)
tree.1 <- rpart(form,data=myfiles_allyears,control=rpart.control(minsplit=20,cp=0))


rpart.plot(tree.1, # middle graph
           extra="auto", box.palette="GnBu",
           branch.lty=5, shadow.col="gray", nn=TRUE)

})
})


output$plot7d <- renderPlot({
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
    counties <- counties[grep(input$county, counties@data$NAME),]
    
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
    
    DT2 <- subset(DT, county == input$county)
    DT2 <- subset(DT2, commodity == input$commodity)
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
    
    
    
    setwd("/dmine/data/counties/")
    
    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)
    
    
    
    
    
    #layout(matrix(c(1,2),1, 2, byrow=TRUE))
    
    #par(mar=c(4,4,4,5))
    #par(mfrow = c(1, 2))                 
    
    #par(mar=c(0,3,3,2)+1)
    #par(mfrow=c(1,2))
    layout(matrix(c(1,2,3,4),1, 2, byrow=TRUE))
    
    
    
    barplot(newmat5$claims, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), cex.names=1, las=3, col="blue", main = paste(input$state, " crop claim counts  \n", input$month, " ", input$year, "\n", input$commodity, sep=""), horiz=FALSE)
    
    par(mar=c(0,0,3,1)+1)
    
    plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county, " County map", sep=""))
    
    plot(counties_one, col="blue", add=T)
    
    #plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
    #axis(side=1, at=c(1:12))
    #axis(side=2, at=seq(xxx, xxxx, by = interval))
    #lines(newmat5$pr, las=2, col="blue")
    
  })
})










#------

output$plot7c <- renderPlot({
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
counties <- counties[grep(input$county, counties@data$NAME),]

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

DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT2, commodity == input$commodity)
DT2 <- subset(DT2, damagecause == input$damage)

newmat2 <- matrix(, ncol = 1, nrow = 12)
#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1

#--loss
 newii <- 1
 for (ii in monthzz) {
   nez <- subset(DT2, monthcode == ii)
   newmat2[ii, newjj] <- sum(nez$loss)
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
colnames(newmat4) <- c("loss", "acres")
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



setwd("/dmine/data/counties/")

    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state)
    counties_one <- subset(counties, NAME %in% input$county)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))                 

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 2, byrow=TRUE))




barplot(newmat5$loss, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), las=2, col="blue", main = paste(input$state, " crop loss $ \n", input$month, " ", input$year, "\n", input$commodity, sep=""), horiz=FALSE)

par(mar=c(0,0,3,1)+1)

plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county, " County map", sep=""))

plot(counties_one, col="blue", add=T)

#plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
#axis(side=1, at=c(1:12))
#axis(side=2, at=seq(xxx, xxxx, by = interval))
#lines(newmat5$pr, las=2, col="blue")

  })
})




output$plot8 <- renderDataTable({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {
    i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
    
    setwd(yeardir)
    x <- as.data.frame(read.csv(i, strip.white = TRUE))
    DT <- data.table(x)
    
    #DTnew <- tolower(DT$commodity)
    
    #simpleCap <- function(x) {
    #  s <- strsplit(x, " ")[[1]]
    #  paste(toupper(substring(s, 1,1)), substring(s, 2),
    #        sep="", collapse=" ")
    #}
    
    #DTnew1a <- data.frame(sapply(DTnew,simpleCap))
    #colnames(DTnew1a) <- c("commodity_new")
    #DTnew3 <- cbind(DT, DTnew1a)
    
    # DTnew3 <- DT
    #  DTnew3$commodity <- DTnew3$commodity_new
    
    #--change to lowercase DT2!!
    #DT2 <- DT
    DT2 <- subset(DT, county == input$county)
    #DT2 <- subset(DTnew3, commodity == input$commodity)
    #DT3 <- data.frame(DT2$acres, DT2$loss)
    #DT4 <- cbind(x, DT3)
    DT2loss <- DT2[,list(loss=sum(loss)), by = county]
    #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
    DT2acres <- DT2[,list(acres=sum(acres)), by = county]
    DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
    DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
    #lengthDT2 <- length(DT2)
    #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1) 
    DT6 <- cbind(DT2loss, DT2acres$acres)
    #--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
    DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres) 
    setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
    
    #v <- data.frame(input$state, input$county, input$year)
    #colnames(v) <- c("State", "County", "Year")
    DT7 
  })
})

output$plot8x <- renderDataTable({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {
    i <- paste("2001_2015_usda_gridmet_", input$state, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
    
    setwd(yeardir)
    x <- as.data.frame(read.csv(i, strip.white = TRUE))
    DT <- data.table(x)
    
    #DTnew <- tolower(DT$commodity)
    
    #simpleCap <- function(x) {
    #  s <- strsplit(x, " ")[[1]]
    #  paste(toupper(substring(s, 1,1)), substring(s, 2),
    #        sep="", collapse=" ")
    #}
    
    #DTnew1a <- data.frame(sapply(DTnew,simpleCap))
    #colnames(DTnew1a) <- c("commodity_new")
    #DTnew3 <- cbind(DT, DTnew1a)
    
    # DTnew3 <- DT
    #  DTnew3$commodity <- DTnew3$commodity_new
    
    #--change to lowercase DT2!!
    #DT2 <- DT
    #DT2 <- subset(DT, county == input$county)
    #DT2 <- subset(DTnew3, commodity == input$commodity)
    #DT3 <- data.frame(DT2$acres, DT2$loss)
    #DT4 <- cbind(x, DT3)
    DT2loss <- DT[,list(loss=sum(loss)), by = county]
    #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
    DT2acres <- DT[,list(acres=sum(acres)), by = county]
    DTdamage_loss <- DT[,list(loss=sum(loss)), by = damagecause]
    DTdamage_acres <- DT[,list(acres=sum(acres)), by = damagecause]
    #lengthDT2 <- length(DT2)
    #DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1) 
    DT6 <- cbind(DT2loss, DT2acres$acres)
    #--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
    DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres) 
    setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
    
    #v <- data.frame(input$state, input$county, input$year)
    #colnames(v) <- c("State", "County", "Year")
    DT7 
  })
})



output$plot8f <- renderDataTable({
  #req(input$commodity)
 # withProgress(message = 'Working', value = 0, {
withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {

i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")



monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
monthz <- data.frame(monthz)

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT2, commodity == input$commodity)
DT2 <- subset(DT2, damagecause == input$damage)

newmat2 <- matrix(, ncol = 1, nrow = 12)
#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1

#--loss
 newii <- 1
 for (ii in monthzz) {
   nez <- subset(DT2, monthcode == ii)
   newmat2[ii, newjj] <- sum(nez$loss)
   newii <- newii + 1
 }

newmat3 <- matrix(, ncol = 1, nrow = 12)

#--acres

  newii <- 1
  for (ii in monthzz) {
    nez <- subset(DT2, monthcode == ii)
    newmat3[ii, newjj] <- sum(nez$acres)
    newii <- newii + 1
  }

newmat4 <- cbind(monthz, newmat2, newmat3)
colnames(newmat4) <- c("MONTH", "LOSS", "ACRES")
newmat4 <- data.frame(newmat4)
newmat5 <- newmat4$loss
#newmat5 <- cbind(newmat, newmat4)

#newmat4[is.na(newmat4)] <- 0

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


newmat4

  
  })
})

output$plot8y <- renderDataTable({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {
    
    i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
    
    
    
    monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    monthz <- data.frame(monthz)
    
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
      newmat3[ii, newjj] <- sum(nez$acres)
      newii <- newii + 1
    }
    
    newmat4 <- cbind(monthz, newmat2, newmat3)
    colnames(newmat4) <- c("MONTH", "CLAIMS", "ACRES")
    newmat4 <- data.frame(newmat4)
    #newmat5 <- newmat4$CLAIMS
    #newmat5 <- cbind(newmat, newmat4)
    
    #newmat4[is.na(newmat4)] <- 0
    
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
    
    
    newmat4
    
    
  })
})

output$plot8g <- renderDataTable({
  req(input$commodity)
  withProgress(message = 'Working', value = 0, {
    
    i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
    
    
    
    monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    monthz <- data.frame(monthz)
    
    setwd(yeardir)
    x <- as.data.frame(read.csv(i, strip.white = TRUE))
    DT <- data.table(x)
    
    DT2 <- subset(DT, county == input$county)
    DT2 <- subset(DT2, commodity == input$commodity)
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
      newmat3[ii, newjj] <- sum(nez$acres)
      newii <- newii + 1
    }
    
    newmat4 <- cbind(monthz, newmat2, newmat3)
    colnames(newmat4) <- c("MONTH", "CLAIMS", "ACRES")
    newmat4 <- data.frame(newmat4)
    #newmat5 <- newmat4$CLAIMS
    #newmat5 <- cbind(newmat, newmat4)
    
    #newmat4[is.na(newmat4)] <- 0
    
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
    
    
    newmat4
    
    
  })
})


output$countycontrols <- renderUI({

   i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")
   
   setwd(yeardir)
   x <- as.data.frame(read.csv(i, strip.white = TRUE))
   DT <- data.table(x)
   DTa <- subset(DT, commodity == input$commodity) 
   DT2 <- unique(DTa$county)
   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( DT2 )

  selectizeInput("county", "Choose a county", uf2[,1], choices = as.vector(uf2[,1]), selected = "Latah", multiple = FALSE)

})


#output$video <- renderUI({
#inputcom <- input$commodity
#inputcom <- gsub(" ", "", inputcom, fixed = TRUE)
#  dir <- paste("http://dmine.io/waf/predictor_USDA/agmesh-scenarios/", input$state, "/month_png/timelapse/", inputcom, "_timelapse.mp4", sep="")
#  tags$video(src = dir, type = "video/mp4", autoplay = NA, controls = NA, width = "700")
#})




output$commoditycontrols <- renderUI({
   uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
   elems <- unlist( strsplit( uniquez, "\\." ) )
   uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( uf2 )

   uf3 <- subset(uf2, V1 == input$year)
  selectizeInput("commodity", "STEP 1: Choose a commodity", uf3[,3], choices = as.vector(uf3[,3]), selected = "WHEAT", multiple = FALSE)

})


output$damagecontrols <- renderUI({

   i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")

   setwd(yeardir)
   x <- as.data.frame(read.csv(i, strip.white = TRUE))
   DT <- data.table(x)
   DTa <- subset(DT, county == input$county)
   #DTa <- subset(DT, county == input$county & commodity == input$commodity)
   DTa <- subset(DTa, commodity == input$commodity)
   DT2 <- unique(DTa$damagecause)
   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- matrix( DT2 )
   uf2 <- as.data.frame(DT2)
   #uf3 <- subset(uf2, commodity  == input$commodity)
  selectizeInput("damage", "Choose a cause of loss", uf2[,1], choices = as.vector(uf2[,1]), selected = "Drought", multiple = FALSE)

})

output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(n = input$county)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
)




})

