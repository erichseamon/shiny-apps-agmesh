library(shiny)
library(doBy)
library(datasets)
library(rgdal)
library(leaflet)
library(png)
library(jpeg)
library(ncdf)   
library(rasterVis)
library(maptools)
library(data.table)


# Define server logic required to summarize and view the selected
# dataset

shinyServer(function(input, output) {
  output$plot1 <- renderPlot({
withProgress(message = 'Working', value = 0, {



#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#
cat("\14")

#----supress warnings

options(warn=0)

#----set packages--------------------------------------------------------#

library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
library("ncdf4")
library("RColorBrewer")
library("raster")
library("rasterVis")
library("latticeExtra")
library("maptools")
library("parallel")
library("Evapotranspiration")
library("plyr")
library("data.table")
library("sirad")
library("rgeos")
library("MASS")
library("stringr")
library("car")
library("sp")

#memory.size(10000)
#----Setting vectors for loops for years, variables, day, and rasters---#

#----input ranges of years to examine----------#

rm(list = ls()) #--clears all lists------#


N1 <- input$firstyear
N2 <- input$lastyear
STATE <- input$state
assign("N1", N1, envir = .GlobalEnv)
assign("N2", N2, envir = .GlobalEnv)

#----writes subset variables to file for shell script operations--#

RSCENARIO <- sample(50000:100000, 1, replace=F)
#scen <- paste("scenario_", RSCENARIO, sep="")

print(paste("Creating R Scenario ", RSCENARIO, sep=""))

#--writes data to a temp file for use later

fileConn<-file("/tmp/agmesh-subset-R-scenario.txt", "w")
writeLines(c(paste('yearstart=', N1, sep='')), fileConn)
writeLines(c(paste('yearend=', N2, sep='')), fileConn)
writeLines(c(paste('state=', STATE, sep='')), fileConn)
writeLines(c(paste('scenario=', "scenario_", RSCENARIO, sep='')), fileConn)
close(fileConn)

#--call bash script to run nc operator functions to extract
#--nc data for each file - using the input of file saved above

#write.table(data.frame(ID,N1,N2,LAT1,LAT2,LON1,LON2), "/tmp/agmesh-subset-vartmp.txt", sep="\t")
system("/agmesh-code/agmesh-subset.sh")
#system("rm /tmp/agmesh-subset-vartmp.txt")




#---Second portion of script that extracts data by county and
#---generates a matrix

scen <- read.table("/tmp/agmesh-subset-R-scenario.txt")
scen <- t(scen)

scen7 = unlist(strsplit(scen[4], split='=', fixed=TRUE))[2]
scen1 = unlist(strsplit(scen[1], split='=', fixed=TRUE))[2]
scen2 = unlist(strsplit(scen[2], split='=', fixed=TRUE))[2]
scen_state = unlist(strsplit(scen[7], split='=', fixed=TRUE))[2]

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen7, sep="")) 
yearspan <- c(scen1:scen2)

scen <- scen7
dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep = "")

print("Generating raster arrays for analysis...")

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep("Idaho", counties@data$STATE_NAME),]

#--loop list for county by fip
countyfiploop <- counties@data$FIPS

#--data frame of county fip list
countyfiplist <- data.frame(counties@data$FIPS)

#--data frame of county names
countynames <- data.frame(counties@data$NAME)

#combo of county names and fip for this list
countylist <- cbind(countynames, countyfiplist)

#--number of rows in county list
countylistrows <- 12 * nrow(countylist)

list <- list.files(path = dirname)
list2 = list
list2 = substr(list2,1,nchar(list2)-3)
list3 <- data.frame(list2)

listcols <- nrow(list)


#--creates an empty matrix that is the number of counties mulitiplied by the number of months (one year standards for the runs.  
#This will be the length of the full final matrix, which will be 14 variables/columns wide
#longlist <- matrix(NA, nrow=countylistrows * 12)

#--loop to generate raster brick from each nc file, subset by the county, extact the values 
#--for each variable, for each month and year combo.

setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(N1:N2)

for (i in yearspan) { 
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=18)
  colnames(newmatrix) <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100", "countyfips", "month", "year")
  varspannumber = 0
  for (j in varspan) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/", j, "_", k, "_", i, ".nc", sep="")
      rasterout <- brick(ncfile)
      rasterout <- mean(rasterout)
      rasterout <- crop(rasterout, counties)
      png(paste(dirname, "/", j, "_", k, "_", i, ".png", sep=""))
      plot(rasterout, main = paste0("Monthly Plot for: ", j, ", ", k, ", ", i, sep=""))
      plot(counties)
      dev.off() 
      #rasterout <- t(rasterout)
      proj4string(rasterout) <- projection 
      for (l in countyfiploop) {
        jj = jj + 1
        subset_county <- counties[counties@data$FIPS == l,]
        e <- extract(rasterout, subset_county, fun=mean) 
        newmatrix[jj,varspannumber] <- mean(e)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--year
      }  
    } 
  }
  setwd(dirname)
  name <- paste(input$state, "_", i, "_summary", sep="")
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}
 
#--test from here on

#--alter yearspan below to minimize the number of years processed for merging.
#--done because years before 2001 have a different structure for USDA data,
#--and thus require an additional loop or alteration to the following loop
#--to factor in that alternative structure (so gridmet can merge correctly)

if (N1 > '2000') {
  
  yearspan <- c(N1:N2)
  #--merge usda data with gridmet data
  
  for (i in yearspan) {
    gridmetmonthly <- paste(dirname, "/", i, "_summary", sep="")
    usda <- paste("/dmine/data/USDA/crop_indemnity_txt/", i, ".txt", sep="")
    usda <- read.csv(usda, sep="|")
    usda <- data.frame(usda)
    gridmetmonthly <- read.csv(gridmetmonthly, strip.white=TRUE)
    gridmetmonthly <- data.frame(gridmetmonthly)
    #usda <- as.matrix(usda)
    #gridmetmonthly <- as.matrix(gridmetmonthly)
    colnames(usda) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
    usda$statecode <- str_pad(usda$statecode, 2, pad = "0") #--pad state with zeros in front so we can combine into one nationwide fips number
    usda$countycode <- str_pad(usda$countycode, 3, pad = "0") #--pad county with zeros in front so we can combine into one nationwide fips number
    usda["countyfips"] <- NA  #--creates a new countyfips column to hold the merged columns
    usda$countyfips <- paste(usda$statecode, usda$countycode, sep="") #--merges the two columns in to one
    gridmetmonthly$month <- sapply(gridmetmonthly$month, toupper)
    
    df3 = merge(gridmetmonthly, usda, by.x=c("year", "month", "countyfips"), by.y=c("year", "month", "countyfips"))
    name = paste(i, "_monthly_usda_gridmet_post2001_", input$state, sep="")
    write.matrix(df3, file=name, sep=",")
    
    #--merge county shapefile with USDA data for mapping purposes
    #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
    #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
    #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
    #write.matrix(m, file=mergename, sep=",")
  }
  
  setwd(dirname)
  files <- list.files(dirname, pattern = 'monthly_usda_gridmet_post2001')
  tables <- lapply(files, read.csv, header=TRUE)
  combined.df <- do.call(rbind, tables)
  name2 = paste(input$firstyear, "_", input$lastyear, "_", "usda_gridmet_", input$state, sep="")
  write.matrix(combined.df, file=name2, sep=",")
  
} else {
  
  yearspan <- c(N1:N2)
  #--merge usda data with gridmet data
  
  for (i in yearspan) {
    gridmetmonthly <- paste(dirname, "/", i, "_summary", sep="")
    usda <- paste("/dmine/data/USDA/crop_indemnity_txt/", i, ".txt", sep="")
    usda <- read.csv(usda, sep="|")
    usda <- data.frame(usda)
    gridmetmonthly <- read.csv(gridmetmonthly, strip.white=TRUE)
    gridmetmonthly <- data.frame(gridmetmonthly)
    #usda <- as.matrix(usda)
    #gridmetmonthly <- as.matrix(gridmetmonthly)
    colnames(usda) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss")
    usda$statecode <- str_pad(usda$statecode, 2, pad = "0") #--pad state with zeros in front so we can combine into one nationwide fips number
    usda$countycode <- str_pad(usda$countycode, 3, pad = "0") #--pad county with zeros in front so we can combine into one nationwide fips number
    usda["countyfips"] <- NA  #--creates a new countyfips column to hold the merged columns
    usda$countyfips <- paste(usda$statecode, usda$countycode, sep="") #--merges the two columns in to one
    gridmetmonthly$month <- sapply(gridmetmonthly$month, toupper)
    
    df3 = merge(gridmetmonthly, usda, by.x=c("year", "month", "countyfips"), by.y=c("year", "month", "countyfips"))
    name = paste(i, "_monthly_usda_gridmet_pre2001_", input$state, sep="")
    write.matrix(df3, file=name, sep=",")
    
    #--merge county shapefile with USDA data for mapping purposes
    #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
    #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
    #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
    #write.matrix(m, file=mergename, sep=",")
  }
  
  setwd(dirname)
  files <- list.files(dirname, pattern = 'monthly_usda_gridmet_pre2001')
  tables <- lapply(files, read.csv, header=TRUE)
  combined.df <- do.call(rbind, tables)
  name2 = paste(input$firstyear, "_", input$lastyear, "_", "usda_gridmet_", input$state, sep="")
  write.matrix(combined.df, file=name2, sep=",")
  
}

##-move files to appropriate locations

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep=""))
system("mkdir netcdf")
system("mv *.nc ./netcdf")

system(paste("mkdir", input$state, " summaries"))
system(paste("mkdir", input$state, " month"))
system(paste("mkdir", input$state, " raster_commodity"))
system(paste("mkdir", input$state, " raster_commodity_plots"))
system(paste("mkdir", input$state, " gridmet_monthly_plots"))
systen(paste("mkdir", input$state, " commodity_csv"))
system(paste("mkdir", input$state, " month_positive"))
system(paste("mkdir", input$state, " commodity_csv_agr"))
system(paste("mkdir", input$state, " commodity_csv_agr_month"))
system(paste("mkdir", input$state, " month_png"))
system(paste("mv *summary", " ",  "./", input_state, " summaries", sep=""))
system(paste("mv *_monthly*", " ", "./", input$state, " summaries", sep=""))
system(paste("mv *_gridmet*", " ", "./", input$state, " summaries", sep=""))
system(paste("mv *.png", " ", "./", input$state, " gridmet_monthly_plots", sep=""))
 
 
})
})


output$plot2 <- renderPlot({

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

  b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))

  bb <- barplot(m$acres, names.arg = m$county, las=2, col = newmatrix_acres)
  #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
  plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})

})









output$plot4_backup <- renderPlot({

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
  
  b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
  
  bb <- barplot(m$acres, names.arg = m$county, las=2, col = newmatrix_acres)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})  
})

output$plot5 <- renderPlot({

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
  par(mar=c(6,3,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
  #--turn image horizontal
  
  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]
  
  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)
 
  b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  #plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
  
  bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres)
  #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})  

})

output$plot6 <- renderPlot({


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
barplot(newmat5$loss, las=2, col="blue")
barplot(newmat5$acres, names.arg = rownames(newmat5), las=2, col="blue")
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











output$plot8 <- renderDataTable({
  
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

})
