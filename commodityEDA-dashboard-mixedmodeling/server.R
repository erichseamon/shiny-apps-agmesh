library(rgdal)
library(leaflet)

if (!interactive()) sink(stderr(), type = "output")

#source("/srv/shiny-server/commodityEDA-dashboard2/utils.R")


xx_eqip <- readRDS(file = "/dmine/data/soilses/data/Eqip.rds")
#xx_eqip <- read.csv("/dmine/data/soilses/data/Eqip.csv", header = TRUE)

#xx_commodity <- read.csv("/dmine/data/USDA/crop_indemnity_originals_aggregated/commodities.csv", header = TRUE)
xx_commodity <- readRDS(file = "/dmine/data/USDA/crop_indemnity_originals_aggregated/commodities.rds")

#xx_damage <- read.csv("/dmine/data/USDA/crop_indemnity_originals_aggregated/commodities_damagecause.csv", header = TRUE)
xx_damage <- readRDS(file = "/dmine/data/USDA/crop_indemnity_originals_aggregated/commodities_damagecause.rds")


setwd("/dmine/data/counties/")

counties_conus <- readShapePoly('UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")



xx_damage$loss[is.na(xx_damage$loss)] <- 0
xx_damage$log10loss <- log10(xx_damage$loss)
xx_damage$log10loss <- ifelse(xx_damage$log10loss < 0, 0, xx_damage$log10loss)

xx_commodity$loss[is.na(xx_commodity$loss)] <- 0
xx_commodity$log10loss <- log10(xx_commodity$loss)
xx_commodity$log10loss <- ifelse(xx_commodity$log10loss < 0, 0, xx_commodity$log10loss)

xx_damage$count[is.na(xx_damage$count)] <- 0



shinyServer(function(input, output) {
  
  output$kpi_summary_box_1 <- renderValueBox({

library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep("Washington", counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = input$state7)
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_1989, xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)

#par(mar=c(5,5,3,3), mgp = c(4, 1, 0))
#layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow=TRUE))

#barplot of one year, average by month

xrangeyear <- subset(xrange, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7 & year == input$year7)
xrangeyeara <- aggregate(xrangeyear$loss, list(xrangeyear$monthcode), FUN = "sum")
colnames(xrangeyeara) <- c("monthcode", "loss")

monthlist <- data.frame(c(1:12))
colnames(monthlist) <- c("monthcode")
xrangeyeara <- data.frame(xrangeyeara)
xrangeyear2 <- join(monthlist, xrangeyeara, by = "monthcode")
xrangeyear2 <- data.frame(xrangeyear2)
#colnames(xrangeyear2)[4] <- "NAME"
xrangeyear3 <- t(cbind(xrangeyear2$monthcode, xrangeyear2$loss))

#barplot(xrangeyear2$loss)

#barplot of normals for 1989-2015 for county/commodity/damage cause 


xrange2 <- aggregate(xrange$loss, list(xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "mean")
colnames(xrange2) <- c("county", "commodity", "damagecause", "monthcode", "loss")

xrange3 <- subset(xrange2, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7)
xrange3 <- subset(xrange3, monthcode != 0)
xrange3_month <- c(1:12)
xrange3_month <- data.frame(xrange3_month)
colnames(xrange3_month) <- c("monthcode")
xrange3 <- join(xrange3_month, xrange3, by = "monthcode" )

#lines(xrange3$loss)

xrange4 <- cbind(c(1:12), xrangeyear2$loss, xrange3$loss)
colnames(xrange4) <- c("monthcode", "single_year", "loss")
xrange4 <- data.frame(xrange4)

xrange5 <- cbind(xrange4$monthcode, xrange4$loss)
xrange6 <- t(xrange5)


#--full range for all years, sum of each month

xrange2full <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "sum")
colnames(xrange2full) <- c("year", "county", "commodity", "damagecause", "monthcode", "loss")

xrange3full <- subset(xrange2full, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7)
xrange3full[order(xrange3full$year, xrange3full$monthcode),]
xrange_yearlist <- as.data.frame(rep(1989:2015, each = 12))
xrange_monthlist <- as.data.frame(rep(1:12, 27))
xrange_yearmonthlist <- cbind(xrange_yearlist, xrange_monthlist)
colnames(xrange_yearmonthlist) <- c("year", "monthcode")

xrange3fullfinal <- join(xrange_yearmonthlist, xrange3full, by=c("year","monthcode"))

xrange3fullfinal2 <- cbind(xrange3fullfinal, as.data.frame(rep(xrange6[2,], 27)))
#xrange3fullfinal2 <- subset(xrange3fullfinal2, county != "<NA>")
xrange3fullfinal2[is.na(xrange3fullfinal2)] <- 0

colnames(xrange3fullfinal2) <- c("year", "monthcode", "county", "commodity", "damagecause", "loss", "normal")
xrange3fullfinal2$anomaly <- xrange3fullfinal2$loss - xrange3fullfinal2$normal







xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
#barplot(xrange7[2:3,], cex.main = 1.2, cex.axis = 1, cex.lab = 1, col=c("red", "darkblue"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = paste("Crop Loss Normals (1989-2015) vs. ", input$year7, "\n", input$state7, " ", input$commodity7, " ", input$damage7, " Claims", sep=""), legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", las = 2, font.lab=2, cex.names = 1)

#legend("topright", 
#       legend = c(paste(input$year7, sep=""), paste("1989-2015", sep="")), 
#       fill = c("red", "darkblue"), cex = 1)


xrange7[is.na(xrange7)] <- 0
anomaly <- xrange7[2,] - xrange7[3,]

anonsum <- round(sum(anomaly), 0)

library(scales); anonsum2 <- dollar(anonsum)

    valueBox(


      value = paste(anonsum2, sep=""),
      subtitle = paste(input$county7, " county, ", input$state7, " ", input$damage7, " based ", input$commodity7, " Claims ", sep=""),
      icon = if (anonsum >= 0) icon("info-circle") else icon("arrow-down"), 
      color = if (anonsum >= 0) "light-blue" else "red"
    )
  })
  
  output$kpi_summary_box_2 <- renderValueBox({

library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep("Washington", counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7 , "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = input$state7)
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_1989, xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)

#par(mar=c(8,9,7,5), mgp = c(7, 1, 0))
#layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow=TRUE))

#barplot of one year, average by month

countieslist <- as.data.frame(counties$NAME)
countieslist$anomaly <- NA

clist <- matrix(NA, nrow = nrow(countieslist), ncol = 2)

jj=1
xrangeyearallcounties <- subset(xrange, commodity == input$commodity7  & damagecause == input$damage7 & year == input$year7)

for (ii in unique(xrangeyearallcounties$county)) {
  


xrangeyear <- subset(xrange, county == ii & commodity == input$commodity7 & damagecause == input$damage7 & year == input$year7)
xrangeyeara <- aggregate(xrangeyear$loss, list(xrangeyear$monthcode), FUN = "sum")
colnames(xrangeyeara) <- c("monthcode", "loss")

monthlist <- data.frame(c(1:12))
colnames(monthlist) <- c("monthcode")
xrangeyeara <- data.frame(xrangeyeara)
xrangeyear2 <- join(monthlist, xrangeyeara, by = "monthcode")
xrangeyear2 <- data.frame(xrangeyear2)
#colnames(xrangeyear2)[4] <- "NAME"
xrangeyear3 <- t(cbind(xrangeyear2$monthcode, xrangeyear2$loss))

#barplot(xrangeyear2$loss)

#barplot of normals for 1989-2015 for county/commodity/damage cause 


xrange2 <- aggregate(xrange$loss, list(xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "mean")
colnames(xrange2) <- c("county", "commodity", "damagecause", "monthcode", "loss")

xrange3 <- subset(xrange2, county == ii & commodity == input$commodity7 & damagecause == input$damage7)
xrange3 <- subset(xrange3, monthcode != 0)
xrange3_month <- c(1:12)
xrange3_month <- data.frame(xrange3_month)
colnames(xrange3_month) <- c("monthcode")
xrange3 <- join(xrange3_month, xrange3, by = "monthcode" )

#lines(xrange3$loss)

xrange4 <- cbind(c(1:12), xrangeyear2$loss, xrange3$loss)
colnames(xrange4) <- c("monthcode", "single_year", "loss")
xrange4 <- data.frame(xrange4)

xrange5 <- cbind(xrange4$monthcode, xrange4$loss)
xrange6 <- t(xrange5)


#--full range for all years, sum of each month

xrange2full <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "sum")
colnames(xrange2full) <- c("year", "county", "commodity", "damagecause", "monthcode", "loss")

xrange3full <- subset(xrange2full, county == ii & commodity == input$commdity7 & damagecause == input$damage7)
xrange3full[order(xrange3full$year, xrange3full$monthcode),]
xrange_yearlist <- as.data.frame(rep(1989:2015, each = 12))
xrange_monthlist <- as.data.frame(rep(1:12, 27))
xrange_yearmonthlist <- cbind(xrange_yearlist, xrange_monthlist)
colnames(xrange_yearmonthlist) <- c("year", "monthcode")

xrange3fullfinal <- join(xrange_yearmonthlist, xrange3full, by=c("year","monthcode"))

xrange3fullfinal2 <- cbind(xrange3fullfinal, as.data.frame(rep(xrange6[2,], 27)))
#xrange3fullfinal2 <- subset(xrange3fullfinal2, county != "<NA>")
xrange3fullfinal2[is.na(xrange3fullfinal2)] <- 0

colnames(xrange3fullfinal2) <- c("year", "monthcode", "county", "commodity", "damagecause", "loss", "normal")
xrange3fullfinal2$anomaly <- xrange3fullfinal2$loss - xrange3fullfinal2$normal






xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
#barplot(xrange7[2:3,], cex.main = 2, cex.axis = 2, cex.lab = 2, col=c("red", "darkblue"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = paste("Monthly Crop Loss Comparison: Crop Loss Normals (1989-2015) vs.", input$year7, "\n State of ", input$state7, " ", input$commodity7, " ", input$damage7, sep=""),  legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", las = 2, font.lab=2, cex.names = 2)

#legend("topright", 
#       legend = c(paste("Crop Loss for ", input$year7, sep=""), " Crop Loss Monthly Average 1989-2015"), 
#       fill = c("red", "darkblue"), cex = 2)


xrange7[is.na(xrange7)] <- 0
anomaly <- xrange7[2,] - xrange7[3,]






an1 <- anomaly[c(1:12)]>=0
an2 <- anomaly[c(1:12)]<=0

if ( all(an1, na.rm=TRUE) == TRUE ) {
  
  ylimz <- max(anomaly, na.rm=TRUE)
  ylimzz <- c(-ylimz, ylimz) 
} else {
  
  ylimzz <- c(0, max(anomaly, na.rm=TRUE))      
  
  
}

if ( all(an2, na.rm=TRUE) == TRUE ) {
  
  ylimz <- min(anomaly, na.rm=TRUE)
  ylimzz <- c(ylimz, abs(ylimz)) 
} else {
  
  ylimzz <- c(-max(anomaly, na.rm=TRUE), max(anomaly, na.rm=TRUE))
}


#anomaly <- xrange7[2,] - xrange7[3,]
anomaly2 <- t(xrange3fullfinal2$anomaly)
an3 <- anomaly2[c(1:324)]>=0
an4 <- anomaly2[c(1:324)]<=0

if ( all(an3, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- max(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(-ylimz1, ylimz1)
} else {
  
  ylimzz1 <- c(0, max(anomaly2, na.rm=TRUE))
  
  
}

if ( all(an4, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- min(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(ylimz1, abs(ylimz1))
} else {
  
  ylimzz1 <- c(-max(anomaly2, na.rm=TRUE), max(anomaly2, na.rm=TRUE))
}




#barplot(anomaly, col=ifelse(anomaly>0,"red","darkblue"), cex.names = 2, cex.axis = 2, cex.main = 2, ylim = ylimzz, cex.lab = 2, names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=3, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=3)

#legend("topright", 
#       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), cex = 2, 
#       fill = c("darkblue", "red"))

clist[jj,] <- c(ii, sum(anomaly))
jj = jj + 1


}

ccanon <- round(sum(as.numeric(na.omit((clist[,2])))), 0)






library(scales); ccanon2 <- dollar(ccanon)
    valueBox(
      value = paste(ccanon2, sep=""),
      subtitle = paste(input$state7, " Anomalous ", input$commodity7, " ", input$damage7, " claims, ", "1989-2015 vs ", " ", input$year7, sep=""), 
      icon = if (ccanon >= 0) icon("arrow-up") else icon("arrow-down"),
      color = if (ccanon >= 0) "green" else "red")
  })


  output$kpi_summary_box_1a <- renderValueBox({
withProgress(message = 'Please Wait', value = 0,  {


input$submit4

isolate({

library(plyr)

xx_damage2 <- subset(xx_damage, commodity == input$commodityUS_damage & year == input$year7 & damagecause == input$damageUS_damage)
ccanon <- round(sum(as.numeric(na.omit((xx_damage2$loss)))), 0)


ccanon_count <- round(sum(as.numeric(na.omit((xx_damage2$count)))), 0)



library(scales); ccanon2 <- dollar(ccanon)
    valueBox(
      value = paste(ccanon2, " revenue loss for ", ccanon_count, " claims",  sep=""),
      subtitle = paste("US total ", input$commodityUS_damage, " ", input$damageUS_damage, " insurance loss, ", input$year7, sep=""),
      icon = if (ccanon >= 0) icon("info-circle") else icon("arrow-down"),
      color = if (ccanon >= 0) "light-blue" else "red")
  })
})
})

output$kpi_summary_box_1b <- renderValueBox({
withProgress(message = 'Please Wait', value = 0,  {


input$submit5

isolate({

library(plyr)

#xx_damage2 <- subset(xx_damage, commodity == input$commodityUS_damage & year == input$year7 & damagecause == input$damageUS_damage)
#ccanon <- round(sum(as.numeric(na.omit((xx_damage2$count)))), 0)
setwd("/dmine/data/soilses/data")
n1 <- read.csv("NRI_combined.csv", header=TRUE)


xxx <- subset(n1, Year == input$NRI_year)

ccanon <- sum(eval(parse(text=paste("xxx$", input$NRI_fields, sep="")))) 


#library(scales); ccanon2 <- dollar(ccanon)
    valueBox(
      value = paste(ccanon, sep=""),
      subtitle = paste("US total ", input$NRI_fields, " ",  "(thousands of acres) ", input$NRI_year, sep=""),
      icon = if (ccanon >= 0) icon("info-circle") else icon("arrow-down"),
      color = if (ccanon >= 0) "light-blue" else "red")
  })
})
})

output$kpi_summary_box_1b_agcensus <- renderValueBox({
withProgress(message = 'Please Wait', value = 0,  {


input$submit6

isolate({

library(plyr)

#xx_damage2 <- subset(xx_damage, commodity == input$commodityUS_damage & year == input$year7 & damagecause == input$damageUS_damage)
#ccanon <- round(sum(as.numeric(na.omit((xx_damage2$count)))), 0)
setwd("/dmine/data/soilses/data")
n1 <- read.csv("AgCensus2012.csv", header=TRUE)


xxx <- subset(n1, year == input$ag_year)


ccanon <- sum(eval(parse(text=paste("xxx$", input$agcensuscontrols, sep=""))))


#library(scales); ccanon2 <- dollar(ccanon)
    valueBox(
      value = paste(ccanon, sep=""),
      subtitle = paste("US total ", input$agcensuscontrols, " ",  "(thousands of acres) ", input$ag_year, sep=""),
      icon = if (ccanon >= 0) icon("info-circle") else icon("arrow-down"),
      color = if (ccanon >= 0) "green" else "red")
  })
})
})



output$kpi_summary_box_NRI_ <- renderValueBox({
withProgress(message = 'Please Wait', value = 0,  {


input$submit5

isolate({

library(plyr)

xx_damage2 <- subset(xx_damage, commodity == input$commodityUS_damage & year == input$year7 & damagecause == input$damageUS_damage)
ccanon <- round(sum(as.numeric(na.omit((xx_damage2$count)))), 0)






#library(scales); ccanon2 <- dollar(ccanon)
    valueBox(
      value = paste(ccanon, sep=""),
      subtitle = paste("US total ", input$commodityUS_damage, " ", input$damageUS_damage, " insurance loss, ", input$year7, sep=""),
      icon = if (ccanon >= 0) icon("info-circle") else icon("arrow-down"),
      color = if (ccanon >= 0) "light-blue" else "red")
  })
})
})



  
  output$kpi_summary_box_3 <- renderValueBox({
    valueBox(
      value = sprintf("%s", 104924422),
      subtitle = sprintf("KPI 3 (%.1f%%)", -5.422),
      icon = icon("arrow-down"),
      color = "green"
    )
  })





if(FALSE) {

output$crop_interaction1 <- renderPlotly({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({


#library(rpart)				        # Popular decision tree algorithm
#library(rattle)					# Fancy tree plot
#library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
#library(party)					# Alternative decision tree algorithm
#library(partykit)				# Convert rpart object to BinaryTree
#library(caret)
#library(plotly)					# Just a data source for this script
#library(mvnormtest)
library(htmlwidgets)
# but probably one of the best R packages ever. 
#data(segmentationData)				# Get some data
#data <- segmentationData[,-c(1,2)]
library(maptools)
library(MASS)
#------

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) 
{ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
  text(0.5, 0.5, txt, cex = cex.cor * r) 
} 



#-Loading all commodities for the palouse 1989 - 2015

pnw_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/PNW_summary_all.csv")
pnw_counts <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/PNW_summary_counts.csv")
pnw_sumloss <- aggregate(loss ~ year + damagecause + state + county + commodity,  pnw_sumloss, sum)
pnw_counts <- aggregate(count ~ year + damagecause + state + county + commodity,  pnw_counts, sum)

palouse_sumloss_allcomm <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/palouse_summary_all.csv")
palouse_sumloss_allcomm2  <- aggregate(loss ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)
palouse_count_allcomm2  <- aggregate(count ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)

palouse_sumloss_allcomm2 <- palouse_sumloss_allcomm2[palouse_sumloss_allcomm2$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015

palouse_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")
palouse_sumloss <- aggregate(loss ~ year + damagecause + state + county,  palouse_sumloss, sum)
palouse_counts <- aggregate(count ~ year + damagecause + state + county,  palouse_counts, sum)


#-is there a normal signal using just wheat, drought claims across all of the pacific northwest

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought")
#qqnorm(palouse_sumloss_drought$loss)


#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#palouse_sumloss2 <- subset(palouse_sumloss, loss > 0)

palouse_sumloss$cube_loss <- Math.cbrt(palouse_sumloss$loss)
palouse_counts$cube_counts <- Math.cbrt(palouse_counts$count)

#use a cube transformation on loss for all commodity claims

palouse_sumloss_allcomm2$cube_loss <- Math.cbrt(palouse_sumloss_allcomm2$loss)

#-use a log transform on the same WHEAT claims data

palouse_sumloss$log_loss <- log(which(!is.na(palouse_sumloss$loss)))

# - plot some qqplots to see how normal the data is

#qqnorm(palouse_sumloss$loss)
#qqnorm(palouse_sumloss$cube_loss)
#qqnorm(palouse_sumloss$log_loss)
#qqnorm(palouse_sumloss_allcomm2$cube_loss)
#qqnorm(palouse_counts$count)

#box cox transformation


#-factor counties
palouse_sumloss$county = factor(palouse_sumloss$county,
                                levels=unique(palouse_sumloss$county))

#-factor years
#palouse_sumloss$year = factor(palouse_sumloss$year,
#                                levels=unique(palouse_sumloss$year))


#-plot basic interaction plots for WHEAT cube root loss using year as x and damagecause as the line

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Decline in Price" | damagecause == "Cold Wet Weather")

#interaction.plot(x.factor     = palouse_sumloss_drought$year,
#                 trace.factor = palouse_sumloss_drought$damagecause, 
#                 response     = palouse_sumloss_drought$cube_loss, 
#                 fun = mean,
#                 las = 2,
#                 type="b",
#                 col=c("black","red","green"),  ### Colors for levels of trace var.
#                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
#                 fixed=TRUE,                    ### Order by factor order in data
#                 leg.bty = "o")

addNoAnswer <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "No Answer")))
  return(x)
}

stateabbrev <- state.abb[grep(input$state7, state.name)]



pnw_sumloss_2007_2015_aggregate <- subset(pnw_sumloss, year >= input$startyear7 & year <= input$endyear7 )
pnw_sumloss_2007_2015_aggregate <- subset(pnw_sumloss_2007_2015_aggregate, state == stateabbrev)
pnw_sumloss_2007_2015_aggregate <- subset(pnw_sumloss_2007_2015_aggregate, county == input$county7)
pnw_sumloss_2007_2015_aggregate <- subset(pnw_sumloss_2007_2015_aggregate, commodity == input$commodity7)



pnw_sumloss_2007_2015_aggregate <- aggregate(pnw_sumloss_2007_2015_aggregate$loss, list(pnw_sumloss_2007_2015_aggregate$damagecause), FUN = "sum")

palouse_sumloss_2007_2015_aggregate <- pnw_sumloss_2007_2015_aggregate[order(pnw_sumloss_2007_2015_aggregate$x),] 
X <- palouse_sumloss_2007_2015_aggregate[order(palouse_sumloss_2007_2015_aggregate$x),] 
colnames(X) <- c("damagecause", "loss")



summary_dataset <- X %>% filter(loss < max(loss)/5) 
summary_dataset2 <- X %>% filter(loss > max(loss)/5) 
colnames(summary_dataset) <- c("damagecause", "loss")
Y <- sum(summary_dataset$loss)
YY <- c("Other", Y)
YY <- data.frame(t(YY))
colnames(YY) <- c("damagecause", "loss")
YYY <- rbind(summary_dataset2, YY)
#factor(YYY)

YYY <- as.data.frame(lapply(YYY, addNoAnswer))
YYY$loss <- as.numeric(as.character(YYY$loss))
YYY1<- YYY[order(YYY$loss),] 
YYY1 <- YYY1[ nrow(YYY1):1, ]

plot_ly(YYY, labels = ~damagecause, values = ~loss, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', loss),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%

config(displayModeBar = F) %>%
  layout(title = paste("Top Damage Causes for ", input$commodity7, ", ", input$startyear7, "-", input$endyear7, "\n", input$county7, ", ", stateabbrev,  sep=""),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#grid.table(YYY1)






})
})
})

}

output$crop_interaction_USbarplot2 <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({

library(classInt)
library(leaflet)
library(dplyr)


par(mar=c(7.1,2.1,4.1,2.1))
xx_prestate <- subset(xx_damage, year == input$year7 & commodity == input$commodityUS_damage & damagecause == input$damageUS_damage)
xx_state <- aggregate(xx_prestate$loss, list(xx_prestate$STATE_NAME), FUN = "sum")
xx_state2 <- aggregate(xx_prestate$count, list(xx_prestate$STATE_NAME), FUN = "sum")
xx_state3 <- data.frame(xx_state[,2]/xx_state2[,2])
xx_state4 <- cbind(xx_state, xx_state3)
colnames(xx_state4) <- c("state", "total", "average")
xx_state[,1] <- state.name[match(xx_state[,1], state.abb)]
barplot(xx_state4$average, main = paste(input$commodityUS_damage, " ", input$damageUS_damage,  " Mean Totals, ", input$year7, sep=""), names.arg=xx_state[,1], horiz = FALSE, las = 3, col = "lightblue")

#par(mar=c(5.1,8.1,4.1,2.1))
#xx_prestate <- subset(xx_damage, year == input$year7 & commodity == input$commodityUS_damage & damagecause == input$damageUS_damage)
#xx_state <- aggregate(xx_prestate$loss, list(xx_prestate$STATE_NAME), FUN = "mean")
#xx_state[,1] <- state.name[match(xx_state[,1], state.abb)]
#barplot(xx_state[,2], main = paste(input$commodityUS_damage, " ", input$damageUS_damage,  " Mean Totals, ", input$year7, sep=""), names.arg=xx_state[,1], horiz = TRUE, las = 1, col = "lightblue")

})
})
})

output$crop_interaction_USbarplot <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({

library(classInt)
library(leaflet)
library(dplyr)

par(mar=c(7.1,2.1,4.1,2.1))
xx_prestate <- subset(xx_damage, year == input$year7 & commodity == input$commodityUS_damage & damagecause == input$damageUS_damage)
xx_state <- aggregate(xx_prestate$loss, list(xx_prestate$STATE_NAME), FUN = "sum")
xx_state[,1] <- state.name[match(xx_state[,1], state.abb)]
colnames(xx_state) <- c("state", "total")
barplot(xx_state[,2], main = paste(input$commodityUS_damage, " ", input$damageUS_damage,  " Sum Totals, ", input$year7, sep=""), names.arg=xx_state[,1], horiz = FALSE, las = 3, col = "lightblue")

})
})
})

output$crop_interaction_USbarplot_NRIall <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit5

isolate({

library(classInt)
library(leaflet)
library(dplyr)

par(mar=c(7.1,2.1,3.1,2.1))
setwd("/dmine/data/soilses/data")
n1 <- read.csv("NRI_combined.csv", header = TRUE)
n1_State <- subset(n1, Year == input$NRI_year)

barplot(eval(parse(text=paste("n1_State$", input$NRI_fields, sep=""))), names.arg=n1_State$State, las=3, col = "lightblue", main=paste("All States ", input$NRI_fields, " ", input$NRI_year, sep=""))


})
})
})



output$crop_interaction_USbarplot_agcensus1 <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({

library(classInt)
library(leaflet)
library(dplyr)

par(mar=c(7.1,2.1,3.1,2.1))
setwd("/dmine/data/soilses/data")
n1 <- read.csv("NRI_combined.csv", header = TRUE)
n1_State <- subset(n1, State == input$NRI_state)

barplot(eval(parse(text=paste("n1_State$", input$NRI_fields, sep=""))), names.arg=n1_State$Year, las=3, col="lightblue", main = paste(input$NRI_state, " ", input$NRI_fields, sep=""))


})
})
})


output$crop_interaction_USbarplot_NRI <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit5

isolate({

library(classInt)
library(leaflet)
library(dplyr)

par(mar=c(7.1,2.1,3.1,2.1))
setwd("/dmine/data/soilses/data")
n1 <- read.csv("NRI_combined.csv", header = TRUE)
n1_State <- subset(n1, State == input$NRI_state)

barplot(eval(parse(text=paste("n1_State$", input$NRI_fields, sep=""))), names.arg=n1_State$Year, las=3, col="lightblue", main = paste(input$NRI_state, " ", input$NRI_fields, sep=""))


})
})
})


output$crop_interaction_USbarplot_NRI2 <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit5

isolate({

library(classInt)
library(leaflet)
library(dplyr)

par(mar=c(5.1,8.1,4.1,2.1))
setwd("/dmine/data/soilses/data")
n1 <- read.csv("NRI_combined.csv", header = TRUE)
n1_State <- subset(n1, State == "Washington")

barplot(n1_State$CRP_land, names.arg=n1_State$Year, las=3)


})
})
})




output$crop_interaction_USdamage_count <- renderLeaflet({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({

library(classInt)
library(leaflet)
library(dplyr)

xx_damage$STATE_NAME <- state.name[match(xx_damage[,3], state.abb)]


xxx <- subset(xx_damage, year == input$year7 & commodity == input$commodityUS_damage & damagecause == input$damageUS_damage )

m <- merge(counties_conus, xxx, by=c("STATE_NAME", "NAME"))
#m$count[is.na(m$count)] <- 0
#m$log10loss <- log10(m$loss)
#m$log10loss <- ifelse(m$log10loss < 0, 0, m$log10loss)



pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
                    domain = m$count)
exte <- as.vector(extent(counties_conus))

label <- paste(sep = "<br/>", m$NAME, round(m$count, 0))
markers <- data.frame(label)
labs <- as.list(m$count)

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(count), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal, values = ~count, opacity = 0.5, title = NULL,
            position = "bottomright")


})
})
})


output$crop_interaction_USmap_NRI <- renderLeaflet({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0, style="old",  {

input$submit5

isolate({

library(classInt)
library(leaflet)
library(dplyr)
library(RColorBrewer)

setwd("/dmine/data/states/")

Sys.sleep(1)
incProgress(0.2, detail = "Load map data")

states_conus <- readShapePoly('states_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
setwd("/dmine/data/soilses/data")

n1 <- read.csv("NRI_combined.csv", header=TRUE)


xxx <- subset(n1, Year == input$NRI_year)

m <- merge(states_conus, xxx, by=c("State", "State"))
#m$count[is.na(m$count)] <- 0
#m$log10loss <- log10(m$loss)
#m$log10loss <- ifelse(m$log10loss < 0, 0, m$log10loss)

Sys.sleep(2)
incProgress(0.7, detail = "Plot map")

#pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
#                    domain = eval(parse(text=paste("m$", input$NRI_fields, sep="")))) 


pal <- colorNumeric(brewer.pal(11, "GnBu"), na.color = "#ffffff",
                    domain = eval(parse(text=paste("m$", input$NRI_fields, sep=""))))


exte <- as.vector(extent(states_conus))

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", input$NRI_fields, sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", input$NRI_fields, sep=""))))

incProgress(3, detail = "Done")

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", input$NRI_fields, sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal, values = ~eval(parse(text=paste("m$", input$NRI_fields, sep=""))), opacity = 0.5, title = NULL,
            position = "bottomright")



})
})
})


output$crop_interaction_USmap_eqip <- renderLeaflet({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0, style="old",  {

input$submit7

isolate({
library(rgdal)
library(leaflet)
library(maptools)
library(classInt)
library(leaflet)
library(dplyr)
library(Hmisc)
library(RColorBrewer)
library(raster)

Sys.sleep(1)
incProgress(0.2, detail = "Load & aggregate")

#xx_eqip <- read.csv("/dmine/data/soilses/data/Eqip.csv", header = TRUE)

xx_eqip2 <- aggregate(xx_eqip$Dollars.Paid, by=list(xx_eqip$State, xx_eqip$County, eval(parse(text=paste("xx_eqip$", input$eqipyearcontrols, sep=""))), xx_eqip$practice_name), FUN = "sum")
colnames(xx_eqip2) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")

setwd("/dmine/data/counties/")

Sys.sleep(2)
incProgress(0.5, detail = "Load and Merge Counties")

counties_conus <- readShapePoly('UScounties_conus.shp',
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
xx_eqip3a <- subset(xx_eqip2, Practice_Name == input$eqipcontrols & Year == input$eqipyear)
#xx_eqip3a <- subset(xx_eqip2, Practice_Name %in% c(value) & Year == input$eqipyear)
xx_eqip3 <- aggregate(xx_eqip3a$Dollars_Paid, by = list(xx_eqip3a$State, xx_eqip3a$County), FUN = "sum")
colnames(xx_eqip3) <- c("State", "County", "Dollars_Paid")

#--need to deal with units ft vs acres
#eqip_ft <- subset(xx_eqip, units == "ft")
#eqip_ft$units



xx_eqip3$County <- tolower(xx_eqip3$County)
xx_eqip3$County <- capitalize(xx_eqip3$County)

colnames(xx_eqip3)[2] <- "NAME"
colnames(xx_eqip3)[1] <- "STATE_NAME"

m <- merge(counties_conus, xx_eqip3, by=c("STATE_NAME", "NAME"))


Sys.sleep(3)
incProgress(0.7, detail = "Loading Map data")
palz1 <- brewer.pal(9, "GnBu")

palz <- colorRampPalette(palz1)

m$Dollars_Paid[is.na(m$Dollars_Paid)] <- 0 
m$Dollars_Paid <- as.numeric(m$Dollars_Paid)


palData <- classIntervals(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), style="hclust")
colors <- findColours(palData, palz(100))


#pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
#                    domain = eval(parse(text=paste("m$",input$agcensuscontrols , sep=""))))

pal2 <- colorNumeric(brewer.pal(9, "GnBu"), na.color = "#ffffff",
                     domain = eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))



exte <- as.vector(extent(counties_conus))

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))

incProgress(1, detail = "Done")

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", "Dollars_Paid", sep="")))),opacity = .5,  popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), opacity = 1, title = NULL,
            position = "bottomright")


})
})
})








output$crop_interaction_USmap_total <- renderLeaflet({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0, style="old",  {

input$submit8

isolate({


library(classInt)
library(leaflet)
library(dplyr)
library(Hmisc)
library(RColorBrewer)
library(maptools)
library(plyr)
library(raster)

Sys.sleep(1)
incProgress(0.2, detail = "Load map data")

setwd("/dmine/data/states/")

states <- readShapePoly('states_conus.shp',
                        proj4string=CRS
                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")




setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
setwd("/dmine/data/soilses/data")

n1 <- read.csv("TOTALData_Assets_census_bycounty.csv", header=TRUE)
colnames(n1) <- c("program",          "year",             "geo.level",        "state",            "state.ansi",       "ag.district",      "ag.district.code", "county",   
                  "county.ansi",      "commodity",        "data.item",        "domain",           "domain.category",  "value",            "cv"     )


n2 <- read.csv("TOTALData_rental_income_census_by_county.csv", header=TRUE)
colnames(n2) <- c("program",          "year",             "geo.level",        "state",            "state.ansi",       "ag.district",      "ag.district.code", "county",   
                  "county.ansi",      "commodity",        "data.item",        "domain",           "domain.category",  "value",            "cv"     )

n1 <- rbind(n1, n2)




Sys.sleep(2)
incProgress(0.5, detail = "Subset data")


xxx <- subset(n1, year == input$totalyear)


counties$NAME <- tolower(counties$NAME)
counties$STATE_NAME<- tolower(counties$STATE_NAME)

xxx$county <- tolower(xxx$county)
#xxx$county <- capitalize(xxx$county)

xxx$state <- tolower(xxx$state)
#xxx$state <- capitalize(xxx$state)

colnames(xxx)[8] <- "NAME"
colnames(xxx)[4] <- "STATE_NAME"

xxx$value <- as.numeric(xxx$value)
#xxx$data.item <- as.numeric(xxx$data.item)

levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $" ] <- "dollars"
levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE" ] <- "dollars_per_acre"
levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / OPERATION" ] <- "dollars_per_operation"
levels(xxx$data.item)[levels(xxx$data.item)=="INCOME, FARM-RELATED, RENT, LAND & BUILDINGS - RECEIPTS, MEASURED IN $" ] <- "ag_income"


xxx <- subset(xxx, data.item == input$totalcontrols)
xxx <- data.frame(xxx)
#id = 1:15
#xxx[id] = as.numeric(unlist(xxx[id]))

Sys.sleep(3)
incProgress(0.7, detail = "Merge and plot data")


m <- merge(counties, xxx, by=c("STATE_NAME", "NAME"))

palz1 <- brewer.pal(9, "GnBu")

palz <- colorRampPalette(palz1)

m$value[is.na(m$value)] <- 0

palData <- classIntervals(eval(parse(text=paste("m$", "value", sep=""))), style="hclust")
colors <- findColours(palData, palz(100))


#pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
#                    domain = eval(parse(text=paste("m$",input$agcensuscontrols , sep=""))))

pal2 <- colorNumeric(brewer.pal(9, "GnBu"), na.color = "#ffffff",
                     domain = eval(parse(text=paste("m$", "value", sep=""))))



exte <- as.vector(extent(states))

label <- paste(sep = "<br/>", m$STATE_NAME, m$NAME, round(eval(parse(text=paste("m$", "value", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "value", sep=""))))

incProgress(4, detail = "Done")
leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", "value", sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "value", sep=""))), opacity = 1, title = NULL,
            position = "bottomright")

})
})
})


output$crop_interaction_USbarplot_total1 <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0, style="old",  {

input$submit8

isolate({

library(classInt)
library(leaflet)
library(dplyr)
library(Hmisc)
library(RColorBrewer)
library(maptools)
library(plyr)
library(raster)



setwd("/dmine/data/states/")

states <- readShapePoly('states_conus.shp',
                        proj4string=CRS
                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")




setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
setwd("/dmine/data/soilses/data")

n1 <- read.csv("TOTALData_Assets_census_bycounty.csv", header=TRUE)
colnames(n1) <- c("program",          "year",             "geo.level",        "state",            "state.ansi",       "ag.district",      "ag.district.code", "county",   
                  "county.ansi",      "commodity",        "data.item",        "domain",           "domain.category",  "value",            "cv"     )

n2 <- read.csv("TOTALData_rental_income_census_by_county.csv", header=TRUE)
colnames(n2) <- c("program",          "year",             "geo.level",        "state",            "state.ansi",       "ag.district",      "ag.district.code", "county",   
                  "county.ansi",      "commodity",        "data.item",        "domain",           "domain.category",  "value",            "cv"     )

n1 <- rbind(n1, n2)



xxx <- subset(n1, year == input$totalyear)


counties$NAME <- tolower(counties$NAME)
counties$STATE_NAME<- tolower(counties$STATE_NAME)

xxx$county <- tolower(xxx$county)
#xxx$county <- capitalize(xxx$county)

xxx$state <- tolower(xxx$state)
#xxx$state <- capitalize(xxx$state)

colnames(xxx)[8] <- "NAME"
colnames(xxx)[4] <- "STATE_NAME"

xxx$value <- as.numeric(xxx$value)
#xxx$data.item <- as.numeric(xxx$data.item)

levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $" ] <- "dollars"
levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE" ] <- "dollars_per_acre"
levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / OPERATION" ] <- "dollars_per_operation"
levels(xxx$data.item)[levels(xxx$data.item)=="INCOME, FARM-RELATED, RENT, LAND & BUILDINGS - RECEIPTS, MEASURED IN $" ] <- "ag_income"



xxx <- subset(xxx, data.item == input$totalcontrols)
xxx <- data.frame(xxx)


xxx_nonzero <- subset(xxx, value > 8000)
boxplot(value ~ STATE_NAME, data = xxx_nonzero, las = 3)




})
})
})


output$crop_interaction_USmap_agcensus1 <- renderLeaflet({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0, style="old",  {

input$submit6

isolate({

library(classInt)
library(leaflet)
library(dplyr)
library(Hmisc)
library(RColorBrewer)

Sys.sleep(1)
incProgress(0.2, detail = "Load map data")

setwd("/dmine/data/states/")

states <- readShapePoly('states_conus.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")




setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
setwd("/dmine/data/soilses/data")

n1 <- read.csv("AgCensus2012.csv", header=TRUE)

n1 <- n1[,1:25]


Sys.sleep(2)
incProgress(0.5, detail = "Subset data")


xxx <- subset(n1, year == input$ag_year)

xxx$county <- tolower(xxx$county)
xxx$county <- capitalize(xxx$county)

colnames(xxx)[4] <- "NAME"
colnames(xxx)[3] <- "STATE_NAME"

id = 5:25
xxx[id] = as.numeric(unlist(xxx[id]))


Sys.sleep(3)
incProgress(0.7, detail = "Merge and plot data")

m <- merge(counties, xxx, by=c("STATE_NAME", "NAME"))


palz1 <- brewer.pal(9, "GnBu")

palz <- colorRampPalette(palz1)


palData <- classIntervals(eval(parse(text=paste("m$", input$agcensuscontrols, sep=""))), style="hclust")
colors <- findColours(palData, palz(100))


#pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
#                    domain = eval(parse(text=paste("m$",input$agcensuscontrols , sep=""))))

pal2 <- colorNumeric(brewer.pal(11, "GnBu"), na.color = "#ffffff",
                    domain = eval(parse(text=paste("m$", input$agcensuscontrols, sep=""))))



exte <- as.vector(extent(states))

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", input$agcensuscontrols, sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", input$agcensuscontrols, sep=""))))

incProgress(4, detail = "Done")
leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", input$agcensuscontrols, sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", input$agcensuscontrols, sep=""))), opacity = 1, title = NULL,
            position = "bottomright")

})
})
})





output$crop_interaction_USdamage <- renderLeaflet({ 
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0, style="old",  {

input$submit4

isolate({

library(classInt)
library(leaflet)
library(dplyr)
library(RColorBrewer)

xx_damage$STATE_NAME <- state.name[match(xx_damage[,3], state.abb)]

Sys.sleep(1)
incProgress(0.2, detail = "Subset data")

xxx <- subset(xx_damage, year == input$year7 & commodity == input$commodityUS_damage & damagecause == input$damageUS_damage )

m <- merge(counties_conus, xxx, by=c("STATE_NAME", "NAME"))
m$loss[is.na(m$loss)] <- 0

Sys.sleep(2)
incProgress(0.4, detail = "Create logarithmic loss")

m$log10loss <- log10(m$loss)
m$log10loss <- ifelse(m$log10loss < 0, 0, m$log10loss)

m$count[is.na(m$count)] <- 0


inputy <- input$loss7

#pal <- colorNumeric(palette = c("#ffffff00", "#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#08589E"),
#                    domain = eval(parse(text=paste("m$", input$loss7, sep="")))) 

#pal2 <- colorNumeric(palette = c("#ffffff00", "#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#08589E"),
#                    domain = eval(parse(text=paste("m$", "loss", sep="")))) 




#GnBu_transparent <- c("#ffff00", "#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#08589E")


pal <- colorNumeric(brewer.pal(11, "GnBu"), na.color = "#ffff00",
                    domain = eval(parse(text=paste("m$", input$loss7, sep=""))))



#pal <- colorNumeric(brewer.pal(11, "GnBu"), na.color = "#ffff00",
#                    domain = eval(parse(text=paste("m$", input$loss7, sep=""))))

pal2 <- colorNumeric(brewer.pal(11, "GnBu"), na.color = "#ffff00",
                    domain = eval(parse(text=paste("m$", "loss", sep=""))))


#pal2 <- colorNumeric(brewer.pal(11, "GnBu"), na.color = "#ffff00",
#                    domain = eval(parse(text=paste("m$", "loss", sep=""))))

exte <- as.vector(extent(counties_conus))
m2a <- subset(m, loss == 0)


labela <- paste(sep = "</br>", m2a$NAME, paste(round(eval(parse(text=paste("m2a$", "count", sep=""))), 0), " claims", sep=""), paste("$", round(eval(parse(text=paste("m2a$", "loss", sep=""))), 0), sep="") )
markersa <- data.frame(label)
labsa <- as.list(eval(parse(text=paste("m2a$", input$loss7, sep=""))))



m2 <- subset(m, loss != 0)


label <- paste(sep = "</br>", m2$NAME, paste(round(eval(parse(text=paste("m2$", "count", sep=""))), 0), " claims", sep=""), paste("$", round(eval(parse(text=paste("m2$", "loss", sep=""))), 0), sep="") )
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m2$", input$loss7, sep=""))))

Sys.sleep(3)
incProgress(0.7, detail = "Plotting map data")


if(input$loss7 == 'loss') {




leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", input$loss7, sep="")))), popup = markersa$labela, weight = 0) %>% addPolygons(data = m2, color = ~pal(eval(parse(text=paste("m2$", input$loss7, sep="")))), fillOpacity = 0, weight = 1, popup = markers$label) %>% addLegend(pal = pal, values = ~eval(parse(text=paste("m$", "loss", sep=""))), opacity = 0.5, title = NULL,  position = "bottomright") 

} else if (input$loss7 == 'log10loss') {

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", input$loss7, sep="")))), popup = markersa$labela,  weight = 0) %>% addPolygons(data = m2, color = ~pal(eval(parse(text=paste("m2$", input$loss7, sep="")))), fillOpacity = 0, weight = 1, popup = markers$label) %>% addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "loss", sep=""))), opacity = 0.5, title = NULL, position = "bottomright") 

} else {



leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", input$loss7, sep="")))), popup = markers$label, weight = 1) 
addLegend(pal = pal, values = ~eval(parse(text=paste("m$", "count", sep=""))), opacity = 0.5, title = NULL, position = "bottomright")

incProgress(1, detail = "Done")
}

})
})
})


output$crop_interaction2 <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({


library(rpart)                                  # Popular decision tree algorithm
#library(rattle)                                        # Fancy tree plot
library(rpart.plot)                             # Enhanced tree plots
library(RColorBrewer)                           # Color selection for fancy tree plot
#library(party)                                 # Alternative decision tree algorithm
#library(partykit)                              # Convert rpart object to BinaryTree
library(caret)                                  # Just a data source for this script
#library(mvnormtest)
# but probably one of the best R packages ever.
data(segmentationData)                          # Get some data
data <- segmentationData[,-c(1,2)]
library(maptools)
library(MASS)
#------

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
} 















#-Loading all commodities for the palouse 1989 - 2015

palouse_sumloss_allcomm <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/palouse_summary_all.csv")
palouse_sumloss_allcomm2  <- aggregate(loss ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)
palouse_count_allcomm2  <- aggregate(count ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)

palouse_sumloss_allcomm2 <- palouse_sumloss_allcomm2[palouse_sumloss_allcomm2$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015

palouse_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")
palouse_sumloss <- aggregate(loss ~ year + damagecause + county,  palouse_sumloss, sum)
palouse_counts <- aggregate(count ~ year + damagecause + county,  palouse_counts, sum)


#-is there a normal signal using just wheat, drought claims across all of the pacific northwest

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought")
qqnorm(palouse_sumloss_drought$loss)


#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#palouse_sumloss2 <- subset(palouse_sumloss, loss > 0)

palouse_sumloss$cube_loss <- Math.cbrt(palouse_sumloss$loss)
palouse_counts$cube_counts <- Math.cbrt(palouse_counts$count)

#use a cube transformation on loss for all commodity claims

palouse_sumloss_allcomm2$cube_loss <- Math.cbrt(palouse_sumloss_allcomm2$loss)

#-use a log transform on the same WHEAT claims data

palouse_sumloss$log_loss <- log(which(!is.na(palouse_sumloss$loss)))

# - plot some qqplots to see how normal the data is

qqnorm(palouse_sumloss$loss)
qqnorm(palouse_sumloss$cube_loss)
qqnorm(palouse_sumloss$log_loss)
qqnorm(palouse_sumloss_allcomm2$cube_loss)
qqnorm(palouse_counts$count)

#box cox transformation


#-factor counties
palouse_sumloss$county = factor(palouse_sumloss$county,
                                levels=unique(palouse_sumloss$county))

#-factor years
#palouse_sumloss$year = factor(palouse_sumloss$year,
#                                levels=unique(palouse_sumloss$year))


#-plot basic interaction plots for WHEAT cube root loss using year as x and damagecause as the line

palouse_counts_county <- subset(palouse_counts, county == "Whitman" | county == "Adams" | county == "Latah" | county == "Spokane")

palouse_counts_county$county <- factor(palouse_counts_county$county)

interaction.plot(x.factor     = palouse_counts_county$year,
                 trace.factor = palouse_counts_county$county, 
                 response     = palouse_counts_county$count, 
                 fun = sum,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "n")


})
})
})



output$cropnormals1 <- renderPlot({
#req(input$state710)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({


library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep("Washington", counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = input$state7)
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_1989, xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)

par(mar=c(5,5,3,3), mgp = c(4, 1, 0))
#layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow=TRUE))

#barplot of one year, average by month

xrangeyear <- subset(xrange, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7 & year == input$year7)
xrangeyeara <- aggregate(xrangeyear$loss, list(xrangeyear$monthcode), FUN = "sum")
colnames(xrangeyeara) <- c("monthcode", "loss")

monthlist <- data.frame(c(1:12))
colnames(monthlist) <- c("monthcode")
xrangeyeara <- data.frame(xrangeyeara)
xrangeyear2 <- join(monthlist, xrangeyeara, by = "monthcode")
xrangeyear2 <- data.frame(xrangeyear2)
#colnames(xrangeyear2)[4] <- "NAME"
xrangeyear3 <- t(cbind(xrangeyear2$monthcode, xrangeyear2$loss))

#barplot(xrangeyear2$loss)

#barplot of normals for 1989-2015 for county/commodity/damage cause 


xrange2 <- aggregate(xrange$loss, list(xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "mean")
colnames(xrange2) <- c("county", "commodity", "damagecause", "monthcode", "loss")

xrange3 <- subset(xrange2, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7)
xrange3 <- subset(xrange3, monthcode != 0)
xrange3_month <- c(1:12)
xrange3_month <- data.frame(xrange3_month)
colnames(xrange3_month) <- c("monthcode")
xrange3 <- join(xrange3_month, xrange3, by = "monthcode" )

#lines(xrange3$loss)

xrange4 <- cbind(c(1:12), xrangeyear2$loss, xrange3$loss)
colnames(xrange4) <- c("monthcode", "single_year", "loss")
xrange4 <- data.frame(xrange4)

xrange5 <- cbind(xrange4$monthcode, xrange4$loss)
xrange6 <- t(xrange5)


#--full range for all years, sum of each month

xrange2full <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "sum")
colnames(xrange2full) <- c("year", "county", "commodity", "damagecause", "monthcode", "loss")

xrange3full <- subset(xrange2full, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7)
xrange3full[order(xrange3full$year, xrange3full$monthcode),]
xrange_yearlist <- as.data.frame(rep(1989:2015, each = 12))
xrange_monthlist <- as.data.frame(rep(1:12, 27))
xrange_yearmonthlist <- cbind(xrange_yearlist, xrange_monthlist)
colnames(xrange_yearmonthlist) <- c("year", "monthcode")

xrange3fullfinal <- join(xrange_yearmonthlist, xrange3full, by=c("year","monthcode"))

xrange3fullfinal2 <- cbind(xrange3fullfinal, as.data.frame(rep(xrange6[2,], 27)))
#xrange3fullfinal2 <- subset(xrange3fullfinal2, county != "<NA>")
xrange3fullfinal2[is.na(xrange3fullfinal2)] <- 0

colnames(xrange3fullfinal2) <- c("year", "monthcode", "county", "commodity", "damagecause", "loss", "normal")
xrange3fullfinal2$anomaly <- xrange3fullfinal2$loss - xrange3fullfinal2$normal







xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
barplot(xrange7[2:3,], cex.main = 1.2, cex.axis = 1, cex.lab = 1, col=c("red", "darkblue"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = paste("Crop Loss Normals (1989-2015) vs. ", input$year7, "\n", input$county7, ", ", input$state7, " ", input$commodity7, " ", input$damage7, " Claims", sep=""), legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", las = 2, font.lab=2, cex.names = 1)

legend("topright", 
       legend = c(paste(input$year7, sep=""), paste("1989-2015", sep="")), 
       fill = c("red", "darkblue"), cex = 1)


xrange7[is.na(xrange7)] <- 0
anomaly <- xrange7[2,] - xrange7[3,]

an1 <- anomaly[c(1:12)]>=0
an2 <- anomaly[c(1:12)]<=0

if ( all(an1, na.rm=TRUE) == TRUE ) {
  
  ylimz <- max(anomaly, na.rm=TRUE)
  ylimzz <- c(-ylimz, ylimz) 
} else {

ylimzz <- c(0, max(anomaly, na.rm=TRUE))      
      
  
    }

if ( all(an2, na.rm=TRUE) == TRUE ) {
  
  ylimz <- min(anomaly, na.rm=TRUE)
  ylimzz <- c(ylimz, abs(ylimz)) 
  } else {

ylimzz <- c(-max(anomaly, na.rm=TRUE), max(anomaly, na.rm=TRUE))
}


#anomaly <- xrange7[2,] - xrange7[3,]
anomaly2 <- t(xrange3fullfinal2$anomaly)
an3 <- anomaly2[c(1:324)]>=0
an4 <- anomaly2[c(1:324)]<=0

if ( all(an3, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- max(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(-ylimz1, ylimz1)
} else {
  
  ylimzz1 <- c(0, max(anomaly2, na.rm=TRUE))
  
  
}

if ( all(an4, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- min(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(ylimz1, abs(ylimz1))
} else {
  
  ylimzz1 <- c(-max(anomaly2, na.rm=TRUE), max(anomaly2, na.rm=TRUE))
}




#barplot(anomaly, col=ifelse(anomaly>0,"red","darkblue"), cex.names = 2, cex.axis = 2, cex.main = 2, ylim = ylimzz, cex.lab = 2, names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=3, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=3)

#legend("topright", 
#       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), cex = 2, 
#       fill = c("darkblue", "red"))

#barplot(xrange3fullfinal2$anomaly, col=ifelse(xrange3fullfinal2$anomaly>0,"red","blue"), cex.names = 2, cex.axis = 2, cex.main = 2, cex.lab = 2, ylim=ylimzz1,  border="white", font.axis=2, beside=T, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=2)

#legend("bottomright", 
#       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), 
#       fill = c("red", "darkblue"))

#plot of one year (sum) for entire state for damagecause/commodity

xrange2a <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause), FUN = "sum")
colnames(xrange2a) <- c("year", "county", "commodity", "damagecause", "loss")

xrangemonth <- subset(xrange2a, commodity == input$commodity7 & damagecause == input$damage7 & year == input$year7)
colnames(xrangemonth)[2] <- "NAME"

m <- merge(counties, xrangemonth, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1
ma <- m

par(mar=c(3,3,3,3))

#plot(ma,col = tt(length(ma))[vec])

#plot for normals for month, state, county.commodity.damage cause, 1989 - 2015 mean

xrangenormals <- aggregate(xrange$loss, list(xrange$commodity, xrange$damagecause, xrange$county), FUN = "sum")
colnames(xrangenormals) <- c("commodity", "damagecause", "county", "loss")
xrangenormals_cause <- subset(xrangenormals, damagecause == input$damage7 & commodity == input$commodity7)
colnames(xrangenormals_cause)[3] <- "NAME"

m <- merge(counties, xrangenormals_cause, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1

par(mar=c(3,3,3,3))

#plot(m,col = tt(length(m))[vec])







})
})
})

output$cropnormals2 <- renderPlot({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit4

isolate({


library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep("Washington", counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = input$state7)
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_1989, xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)

par(mar=c(5,5,3,3), mgp = c(4, 1, 0))
#layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow=TRUE))

#barplot of one year, average by month

xrangeyear <- subset(xrange, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7 & year == input$year7)
xrangeyeara <- aggregate(xrangeyear$loss, list(xrangeyear$monthcode), FUN = "sum")
colnames(xrangeyeara) <- c("monthcode", "loss")

monthlist <- data.frame(c(1:12))
colnames(monthlist) <- c("monthcode")
xrangeyeara <- data.frame(xrangeyeara)
xrangeyear2 <- join(monthlist, xrangeyeara, by = "monthcode")
xrangeyear2 <- data.frame(xrangeyear2)
#colnames(xrangeyear2)[4] <- "NAME"
xrangeyear3 <- t(cbind(xrangeyear2$monthcode, xrangeyear2$loss))

#barplot(xrangeyear2$loss)

#barplot of normals for 1989-2015 for county/commodity/damage cause 


xrange2 <- aggregate(xrange$loss, list(xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "mean")
colnames(xrange2) <- c("county", "commodity", "damagecause", "monthcode", "loss")

xrange3 <- subset(xrange2, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7)
xrange3 <- subset(xrange3, monthcode != 0)
xrange3_month <- c(1:12)
xrange3_month <- data.frame(xrange3_month)
colnames(xrange3_month) <- c("monthcode")
xrange3 <- join(xrange3_month, xrange3, by = "monthcode" )

#lines(xrange3$loss)

xrange4 <- cbind(c(1:12), xrangeyear2$loss, xrange3$loss)
colnames(xrange4) <- c("monthcode", "single_year", "loss")
xrange4 <- data.frame(xrange4)

xrange5 <- cbind(xrange4$monthcode, xrange4$loss)
xrange6 <- t(xrange5)


#--full range for all years, sum of each month

xrange2full <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "sum")
colnames(xrange2full) <- c("year", "county", "commodity", "damagecause", "monthcode", "loss")

xrange3full <- subset(xrange2full, county == input$county7 & commodity == input$commodity7 & damagecause == input$damage7)
xrange3full[order(xrange3full$year, xrange3full$monthcode),]
xrange_yearlist <- as.data.frame(rep(1989:2015, each = 12))
xrange_monthlist <- as.data.frame(rep(1:12, 27))
xrange_yearmonthlist <- cbind(xrange_yearlist, xrange_monthlist)
colnames(xrange_yearmonthlist) <- c("year", "monthcode")

xrange3fullfinal <- join(xrange_yearmonthlist, xrange3full, by=c("year","monthcode"))

xrange3fullfinal2 <- cbind(xrange3fullfinal, as.data.frame(rep(xrange6[2,], 27)))
#xrange3fullfinal2 <- subset(xrange3fullfinal2, county != "<NA>")
xrange3fullfinal2[is.na(xrange3fullfinal2)] <- 0

colnames(xrange3fullfinal2) <- c("year", "monthcode", "county", "commodity", "damagecause", "loss", "normal")
xrange3fullfinal2$anomaly <- xrange3fullfinal2$loss - xrange3fullfinal2$normal







xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
#barplot(xrange7[2:3,], cex.main = 2, cex.axis = 2, cex.lab = 2, col=c("red", "darkblue"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = "Monthly Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", las = 2, font.lab=2, cex.names = 2)

#legend("topright", 
#       legend = c(paste("Crop Loss for", " 2015", sep=""), "Crop Loss Monthly Average 1989-2015"), 
#       fill = c("red", "darkblue"), cex = 2)


xrange7[is.na(xrange7)] <- 0
anomaly <- xrange7[2,] - xrange7[3,]

an1 <- anomaly[c(1:12)]>=0
an2 <- anomaly[c(1:12)]<=0

if ( all(an1, na.rm=TRUE) == TRUE ) {
  
  ylimz <- max(anomaly, na.rm=TRUE)
  ylimzz <- c(-ylimz, ylimz) 
} else {

ylimzz <- c(0, max(anomaly, na.rm=TRUE))      
      
  
    }

if ( all(an2, na.rm=TRUE) == TRUE ) {
  
  ylimz <- min(anomaly, na.rm=TRUE)
  ylimzz <- c(ylimz, abs(ylimz)) 
  } else {

ylimzz <- c(-max(anomaly, na.rm=TRUE), max(anomaly, na.rm=TRUE))
}


#anomaly <- xrange7[2,] - xrange7[3,]
anomaly2 <- t(xrange3fullfinal2$anomaly)
an3 <- anomaly2[c(1:324)]>=0
an4 <- anomaly2[c(1:324)]<=0

if ( all(an3, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- max(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(-ylimz1, ylimz1)
} else {
  
  ylimzz1 <- c(0, max(anomaly2, na.rm=TRUE))
  
  
}

if ( all(an4, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- min(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(ylimz1, abs(ylimz1))
} else {
  
  ylimzz1 <- c(-max(anomaly2, na.rm=TRUE), max(anomaly2, na.rm=TRUE))
}




barplot(anomaly, col=ifelse(anomaly>0,"red","darkblue"), cex.names = 1.2, cex.axis = 1, cex.main = 1.2, ylim = ylimzz, cex.lab = 1, names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=3, main = paste("Monthly Anomalies: Crop Loss Normals (1989-2015) vs. ", input$year7, "\n State of ", input$state7, " ", input$commodity7, " ", input$damage7, sep=""),  legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=3)

legend("topright", 
       legend = c(paste("Positive Anomaly", sep=""), paste("Negative Anomaly", sep="")), cex = 1, 
       fill = c("red", "darkblue"))

#barplot(xrange3fullfinal2$anomaly, col=ifelse(xrange3fullfinal2$anomaly>0,"red","blue"), cex.names = 2, cex.axis = 2, cex.main = 2, cex.lab = 2, ylim=ylimzz1,  border="white", font.axis=2, beside=T, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=2)

#legend("bottomright", 
#       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), 
#       fill = c("red", "darkblue"))

#plot of one year (sum) for entire state for damagecause/commodity

xrange2a <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause), FUN = "sum")
colnames(xrange2a) <- c("year", "county", "commodity", "damagecause", "loss")

xrangemonth <- subset(xrange2a, commodity == input$commodity7 & damagecause == input$damage7 & year == input$year7)
colnames(xrangemonth)[2] <- "NAME"

m <- merge(counties, xrangemonth, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1
ma <- m

par(mar=c(3,3,3,3))

#plot(ma,col = tt(length(ma))[vec])

#plot for normals for month, state, county.commodity.damage cause, 1989 - 2015 mean

xrangenormals <- aggregate(xrange$loss, list(xrange$commodity, xrange$damagecause, xrange$county), FUN = "sum")
colnames(xrangenormals) <- c("commodity", "damagecause", "county", "loss")
xrangenormals_cause <- subset(xrangenormals, damagecause == input$damage7 & commodity == input$commodity7)
colnames(xrangenormals_cause)[3] <- "NAME"

m <- merge(counties, xrangenormals_cause, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1

par(mar=c(3,3,3,3))

#plot(m,col = tt(length(m))[vec])







})
})
})



output$plot7y <- renderPlot({
  req(input$commodity7)
  withProgress(message = 'Working', value = 0, {
input$submit4

isolate({





#--
library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
#counties <- counties[grep(input$county7, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

#newj <- 1
#for (j in climz) {
##  newx <- 1
# for (i in monthz) {
# setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/netcdf/")
# ncfile = paste(input$climate, "_", i, "_", input$year7, ".nc", sep="")
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

i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

#DT2 <- subset(DT, county == input$county7)
DT2 <- subset(DT, commodity == input$commodity7)
DT2 <- subset(DT2, damagecause == input$damage7)

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
#    counties <- subset(counties, STATE_NAME %in% input$state7)
#    counties_one <- subset(counties, NAME %in% input$county7)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 1, byrow=TRUE))

barplot(newmat5$claims, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), cex.names=1, las=3, col="blue", main = paste(input$state7, " crop claim counts ", input$month, " ", input$year7, " ", input$commodity7, "\n", "Annual breakdown of insurance claim counts per month, due to ", input$damage7, sep=""), horiz=FALSE)

#par(mar=c(0,0,3,1)+1)

#plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county7, " County map", sep=""))

#plot(counties_one, col="blue", add=T)

#plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
#axis(side=1, at=c(1:12))
#axis(side=2, at=seq(xxx, xxxx, by = interval))
#lines(newmat5$pr, las=2, col="blue")

  })
})

})



output$countycontrols <- renderUI({

   i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

   setwd(yeardir)
   x <- as.data.frame(read.csv(i, strip.white = TRUE))
   DT <- data.table(x)

   DT2 <- unique(DT$county)
   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( DT2 )

  selectizeInput("county", "Choose a county", uf2[,1], choices = as.vector(uf2[,1]))

})


output$commoditycontrols <- renderUI({
   uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
   elems <- unlist( strsplit( uniquez, "\\." ) )
   uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( uf2 )

   uf3 <- subset(uf2, V1 == input$year7 & V2 == input$month)
  selectizeInput("commodity", "Choose a commodity", uf3[,3], choices = as.vector(uf3[,3]))

})


output$plot4 <- renderPlot({

  withProgress(message = 'Working', value = 0, {
input$submit4

isolate({



 setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/netcdf/pdsi_apr_", input$year7, ".nc", sep=""))

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state7)
  #counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
  setwd(yeardir)
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  DT <- data.table(x)

#--change to lowercase DT2!!
  DT2 <- subset(DT, commodity == input$commodity7)
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
  #b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix, cex.names=1, horiz=TRUE, main = paste(input$state7, " crop loss bar chart ($) \n", " ", plotyear, "\n", input$commodity7, sep=""), cex.axis=1.3, cex.main=1.5, width=4)

map <-  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -80.8858673, lat = 41.1450276, zoom = 5)
 

  #plot(m, col = newmatrix, main = paste(input$state7, " crop loss map ($) \n", " ", plotyear, "\n", input$commodity7, sep=""), cex.main=1.5)


})

})
}) 

output$plot5 = renderPlot({
library(leaflet)

input$submit4

isolate({



setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/netcdf/pdsi_apr_", input$year7, ".nc", sep=""))

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state7)
  #counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
  setwd(yeardir)
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  DT <- data.table(x)

#--change to lowercase DT2!!
  DT2 <- subset(DT, commodity == input$commodity7)
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
  #b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix, cex.names=1, horiz=TRUE, main = paste(input$state7, " crop loss bar chart ($) \n", " ", plotyear, "\n", input$commodity7, sep=""), cex.axis=1.3, cex.main=1.5, width=4)


# plot(m, col = newmatrix, main = paste(input$state7, " crop loss map ($) \n", " ", plotyear, "\n", input$commodity7, sep=""), cex.main=1.5)
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% setView(-1.5, 53.4, 9)


}) 
})
output$myMap = renderLeaflet({


input$submit4

isolate({


setwd("/dmine/data/counties/")


 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state7)


setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/netcdf/pdsi_apr_", input$year7, ".nc", sep=""))

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state7)
  #counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
  setwd(yeardir)
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  DT <- data.table(x)
  # DT2 <- DT
  DT2 <- subset(DT, commodity == input$commodity7)
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


output$myMap_anomaly = renderLeaflet({


input$submit4

isolate({

library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep("Washington", counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state7)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7 , "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = input$state7)
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_1989, xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)

#par(mar=c(8,9,7,5), mgp = c(7, 1, 0))
#layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow=TRUE))

#barplot of one year, average by month

countieslist <- as.data.frame(counties$NAME)
countieslist$anomaly <- NA

clist <- matrix(NA, nrow = nrow(countieslist), ncol = 2)

jj=1
xrangeyearallcounties <- subset(xrange, commodity == input$commodity7  & damagecause == input$damage7 & year == input$year7)

for (ii in unique(xrangeyearallcounties$county)) {
  


xrangeyear <- subset(xrange, county == ii & commodity == input$commodity7 & damagecause == input$damage7 & year == input$year7)
xrangeyeara <- aggregate(xrangeyear$loss, list(xrangeyear$monthcode), FUN = "sum")
colnames(xrangeyeara) <- c("monthcode", "loss")

monthlist <- data.frame(c(1:12))
colnames(monthlist) <- c("monthcode")
xrangeyeara <- data.frame(xrangeyeara)
xrangeyear2 <- join(monthlist, xrangeyeara, by = "monthcode")
xrangeyear2 <- data.frame(xrangeyear2)
#colnames(xrangeyear2)[4] <- "NAME"
xrangeyear3 <- t(cbind(xrangeyear2$monthcode, xrangeyear2$loss))

#barplot(xrangeyear2$loss)

#barplot of normals for 1989-2015 for county/commodity/damage cause 


xrange2 <- aggregate(xrange$loss, list(xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "mean")
colnames(xrange2) <- c("county", "commodity", "damagecause", "monthcode", "loss")

xrange3 <- subset(xrange2, county == ii & commodity == input$commodity7 & damagecause == input$damage7)
xrange3 <- subset(xrange3, monthcode != 0)
xrange3_month <- c(1:12)
xrange3_month <- data.frame(xrange3_month)
colnames(xrange3_month) <- c("monthcode")
xrange3 <- join(xrange3_month, xrange3, by = "monthcode" )

#lines(xrange3$loss)

xrange4 <- cbind(c(1:12), xrangeyear2$loss, xrange3$loss)
colnames(xrange4) <- c("monthcode", "single_year", "loss")
xrange4 <- data.frame(xrange4)

xrange5 <- cbind(xrange4$monthcode, xrange4$loss)
xrange6 <- t(xrange5)


#--full range for all years, sum of each month

xrange2full <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "sum")
colnames(xrange2full) <- c("year", "county", "commodity", "damagecause", "monthcode", "loss")

xrange3full <- subset(xrange2full, county == ii & commodity == input$commdity7 & damagecause == input$damage7)
xrange3full[order(xrange3full$year, xrange3full$monthcode),]
xrange_yearlist <- as.data.frame(rep(1989:2015, each = 12))
xrange_monthlist <- as.data.frame(rep(1:12, 27))
xrange_yearmonthlist <- cbind(xrange_yearlist, xrange_monthlist)
colnames(xrange_yearmonthlist) <- c("year", "monthcode")

xrange3fullfinal <- join(xrange_yearmonthlist, xrange3full, by=c("year","monthcode"))

xrange3fullfinal2 <- cbind(xrange3fullfinal, as.data.frame(rep(xrange6[2,], 27)))
#xrange3fullfinal2 <- subset(xrange3fullfinal2, county != "<NA>")
xrange3fullfinal2[is.na(xrange3fullfinal2)] <- 0

colnames(xrange3fullfinal2) <- c("year", "monthcode", "county", "commodity", "damagecause", "loss", "normal")
xrange3fullfinal2$anomaly <- xrange3fullfinal2$loss - xrange3fullfinal2$normal






xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
#barplot(xrange7[2:3,], cex.main = 2, cex.axis = 2, cex.lab = 2, col=c("red", "darkblue"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = paste("Monthly Crop Loss Comparison: Crop Loss Normals (1989-2015) vs.", input$year7, "\n State of ", input$state7, " ", input$commodity7, " ", input$damage7, sep=""),  legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", las = 2, font.lab=2, cex.names = 2)

#legend("topright", 
#       legend = c(paste("Crop Loss for ", input$year7, sep=""), " Crop Loss Monthly Average 1989-2015"), 
#       fill = c("red", "darkblue"), cex = 2)


xrange7[is.na(xrange7)] <- 0
anomaly <- xrange7[2,] - xrange7[3,]






an1 <- anomaly[c(1:12)]>=0
an2 <- anomaly[c(1:12)]<=0

if ( all(an1, na.rm=TRUE) == TRUE ) {
  
  ylimz <- max(anomaly, na.rm=TRUE)
  ylimzz <- c(-ylimz, ylimz) 
} else {
  
  ylimzz <- c(0, max(anomaly, na.rm=TRUE))      
  
  
}

if ( all(an2, na.rm=TRUE) == TRUE ) {
  
  ylimz <- min(anomaly, na.rm=TRUE)
  ylimzz <- c(ylimz, abs(ylimz)) 
} else {
  
  ylimzz <- c(-max(anomaly, na.rm=TRUE), max(anomaly, na.rm=TRUE))
}


#anomaly <- xrange7[2,] - xrange7[3,]
anomaly2 <- t(xrange3fullfinal2$anomaly)
an3 <- anomaly2[c(1:324)]>=0
an4 <- anomaly2[c(1:324)]<=0

if ( all(an3, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- max(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(-ylimz1, ylimz1)
} else {
  
  ylimzz1 <- c(0, max(anomaly2, na.rm=TRUE))
  
  
}

if ( all(an4, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- min(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(ylimz1, abs(ylimz1))
} else {
  
  ylimzz1 <- c(-max(anomaly2, na.rm=TRUE), max(anomaly2, na.rm=TRUE))
}




#barplot(anomaly, col=ifelse(anomaly>0,"red","darkblue"), cex.names = 2, cex.axis = 2, cex.main = 2, ylim = ylimzz, cex.lab = 2, names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=3, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=3)

#legend("topright", 
#       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), cex = 2, 
#       fill = c("darkblue", "red"))

clist[jj,] <- c(ii, sum(anomaly))
jj = jj + 1


}
colnames(clist) <- c("NAME", "anomaly")


countiesanomaly <- merge(counties, clist, by = "NAME", all=T)

countiesanomaly$anomaly <- as.numeric(as.character(countiesanomaly$anomaly))

#countiesanomaly[is.na(countiesanomaly$anomaly)]



#ylGnBl5<-c("#FFFFCC","#C7E9B4","#7FCDBB","#40B6C4","#2C7FB8" ,"#253494")

#manual.col = colorRampPalette(ylGnBl5)

#color.match = manual.col(length(unique(countiesanomaly$anomaly)))

#countiesanomaly$anomaly[is.na(countiesanomaly$anomaly)] <- 0



#s <- seq(min(countiesanomaly$anomaly), max(countiesanomaly$anomaly), len=nrow(countiesanomaly))

#lookupTable = sort(unique(countiesanomaly$anomaly))

#countiesanomaly$color = color.match[match(countiesanomaly$anomaly, lookupTable)]

#plot(countiesanomaly, col=countiesanomaly$color, border="light gray", lwd=0.5)




cc0 <- as.data.frame(subset(countiesanomaly, anomaly != "NA" & anomaly != 0))

cc0 <- cc0[order(cc0$anomaly),]
cc1 <- subset(countiesanomaly, anomaly < 0)
cc2 <- subset(countiesanomaly, anomaly > 0)

cc1a <- nrow(cc1)
cc2a <- nrow(cc2)

if (cc2a == 0) {
  
  rc2 = colorRampPalette(colors = c("lightpink", "darkred"), space="Lab")(cc1a)
  rampcols = as.data.frame(c(rc2))
  
} else if (cc1a == 0) {
  
  rc1 = colorRampPalette(colors = c("darkblue", "lightblue"), space="Lab")(cc2a)
  rampcols = as.data.frame(c(rc1))
  
  } else {
    
    rc1 = colorRampPalette(colors = c("darkblue", "lightblue"), space="Lab")(cc1a)
    rc2 = colorRampPalette(colors = c("lightpink", "darkred"), space="Lab")(cc2a)
    rampcols = as.data.frame(c(rc1, rc2))
    
  }



colnames(rampcols) <- c("color")
cc0$color <- rampcols$color
cc0a <- cc0[, -c(2:6)]

cc0b <- merge(countiesanomaly, cc0a, by = "NAME")

pal <- colorNumeric(palette = c("blue", "red"),
                    domain = countiesanomaly$anomaly, na.color="lightgray")
exte <- as.vector(extent(countiesanomaly))


label <- paste(sep = "<br/>", cc0b$NAME, round(cc0b$anomaly, 0))
markers <- data.frame(label)

lev <- levels(cc0b$color)
lev2 <- c(lev, "#FFFFFF")
levels(cc0b$color) <- lev2

naIndex <- which(is.na(cc0b$color))
cc0b[naIndex, "color"] <- "#FFFFFF"

leaflet(data = cc0b) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = cc0b$color, fillOpacity = 0.5, color = "black", weight = 1, popup = markers$label)





})
})













output$damageUS_damage_controls <- renderUI({




DT <- data.table(xx_damage)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, year == input$year7)
DT2 <- subset(DT2, commodity == input$commodityUS_damage)
DT2 <- unique(DT2$damagecause)

   uf2 <- matrix( DT2 )
   uf2 <- as.data.frame(DT2)
  selectizeInput("damageUS_damage", "Cause of loss", uf2[,1], choices = as.vector(uf2[,1]), selected = "Drought", multiple = FALSE)

})


output$damage7controls <- renderUI({




i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, commodity == input$commodity7)
DT2 <- subset(DT2, county == input$county7)
DT2 <- unique(DT2$damagecause)

#   i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
#   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

#   setwd(yeardir)
#   x <- as.data.frame(read.csv(i, strip.white = TRUE))
#   DT <- data.table(x)
   #DTa <- subset(DT, county == input$county7)
   #DTa <- subset(DT, county == input$county7 & commodity == input$commodity7)
#   DTa <- subset(DT, commodity == input$commodity7)
#   DT2 <- unique(DTa$damagecause)


   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- matrix( DT2 )
   uf2 <- as.data.frame(DT2)
   #uf3 <- subset(uf2, commodity  == input$commodity7)
  selectizeInput("damage7", "Cause of loss", uf2[,1], choices = as.vector(uf2[,1]), selected = "Drought", multiple = FALSE)

})


output$commodityUS_damage_controls <- renderUI({

DT <- data.table(xx_damage)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, year == input$year7)
#DT2 <- subset(DT2, commodity == input$commodityUS_damage)
DT2 <- unique(DT2$commodity)


   uf3 <- as.data.frame(DT2)

  selectizeInput("commodityUS_damage", "Commodity", uf3[,1], choices = as.vector(uf3[,1]), selected = "WHEAT", multiple = FALSE)

})

output$agcensuscontrols <- renderUI({

DT <- read.csv("/dmine/data/soilses/data/AgCensus2012.csv", header=TRUE)
DTvector <- colnames(DT)
DTvector2 <- DTvector[5:25]


   uf3 <- as.data.frame(DTvector2)

  selectizeInput("agcensuscontrols", "AgCensus Variable", uf3[,1], choices = as.vector(uf3[,1]), selected = "tile_farms", multiple = FALSE)

})



output$eqipcontrols <- renderUI({

withProgress(message = 'Please Wait', value = 0, style="old",  {

DT <- xx_eqip
#DT <- read.csv("/dmine/data/soilses/data/Eqip.csv", header = TRUE) 
#DT  <- aggregate(xx_eqip$Dollars.Paid, by=list(xx_eqip$State, xx_eqip$County, eval(parse(text=paste("xx_eqip$", input$eqipyearcontrols, sep=""))), xx_eqip$practice_name), FUN = "sum")
#colnames(DT) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")

DTvector2 <- unique(DT$practice_name) 
Sys.sleep(1)
incProgress(0.5, detail = "Loading Practice Names")

   uf3 <- as.data.frame(DTvector2)

  selectizeInput("eqipcontrols", "Eqip Practice Name", uf3[,1], choices = as.vector(uf3[,1]), selected = "Residue Management, No-Till/Strip Till", multiple = TRUE) 

})
})


output$eqipyear <- renderUI({
withProgress(message = 'Please Wait', value = 0, style="old",  {


Sys.sleep(1)
incProgress(0.5, detail = "Loading Years")
#xx_eqip <- read.csv("/dmine/data/soilses/data/Eqip.csv", header = TRUE)
xx_eqip2 <- aggregate(xx_eqip$Dollars.Paid, by=list(xx_eqip$State, xx_eqip$County, eval(parse(text=paste("xx_eqip$", input$eqipyearcontrols, sep=""))), xx_eqip$practice_name), FUN = "sum")
colnames(xx_eqip2) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")


DTvector2 <- unique(xx_eqip2$Year)


   uf3 <- as.data.frame(DTvector2)

  selectizeInput("eqipyear", "Eqip Year", uf3[,1], choices = as.vector(uf3[,1]), selected = "2010", multiple = FALSE)

})
})

output$commodity7controls <- renderUI({

i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
#DT2 <- subset(DT2, commodity == input$commodity7)
DT2 <- unique(DT$commodity)

   uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
   elems <- unlist( strsplit( uniquez, "\\." ) )
   uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( uf2 )

   #uf3 <- subset(uf2, V1 == input$year7)

   #uf3 <- subset(uf2, V1 == input$startyear:input$endyear)

   uf3 <- as.data.frame(DT2)

  selectizeInput("commodity7", "Commodity", uf3[,1], choices = as.vector(uf3[,1]), selected = "WHEAT")

})



output$county7controls <- renderUI({

i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, commodity == input$commodity7)
DT2 <- unique(DT2$county)








#   i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
#   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

#   setwd(yeardir)
#   x <- as.data.frame(read.csv(i, strip.white = TRUE))
#   DT <- data.table(x)
#   DTa <- subset(DT, commodity == input$commodity7)
#   DT2 <- unique(DTa$county)
   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( DT2 )

  selectizeInput("county7", "Choose a county", uf2[,1], choices = as.vector(uf2[,1]), selected = "Latah", multiple = FALSE)

})
})
