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

output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        #tempReport <- file.path(tempdir(), "report.Rmd")
        #file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(n = input$year)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
}
)


#loadHandler <- reactive({
#  input$myLoader #create a dependency on the button, per Shiny examples.

  output$plot <- renderPlot({
#req(input$commodity)
withProgress(message = 'Working', value = 0, {

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

#yearzz <- c(2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,
#2002,2002,2002,2002,2002,2002,2002,2002,2002,2002,2002,2002,
#2003,2003,2003,2003,2003,2003,2003,2003,2003,2003,2003,2003,
#2004,2004,2004,2004,2004,2004,2004,2004,2004,2004,2004,2004,
#2005,2005,2005,2005,2005,2005,2005,2005,2005,2005,2005,2005,
#2006,2006,2006,2006,2006,2006,2006,2006,2006,2006,2006,2006,
yearzz <- c(2007,2007,2007,2007,2007,2007,2007,2007,2007,2007,2007,2007,
2008,2008,2008,2008,2008,2008,2008,2008,2008,2008,2008,2008,
2009,2009,2009,2009,2009,2009,2009,2009,2009,2009,2009,2009,
2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,
2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,
2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,
2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,
2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,
2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015)
monthzz <- c(1:12)
monthzzz <- rep(monthzz,9)
palouse6 <- as.data.frame(cbind(yearzz,monthzzz))
colnames(palouse6) <- c("year", "month")


library(devtools)
#library(stringr)
#with_options(c(scipen = 999), str_pad(palouse3$monthcode, 2, pad = "0"))
palouse_month <- sprintf("%02d", palouse3$monthcode)
palouse6_month <- sprintf("%02d", palouse6$month)

palouse3$date <- (paste(palouse3$year, ".", palouse_month, sep=""))
palouse6$date <-paste(palouse6$year, ".", palouse6_month, sep="")
#palouse5 <- data.frame(unique(palouse6$date))
colnames(palouse6) <- c("year", "month", "date")
#palouse3a <- data.frame(palouse3$date)
#colnames(palouse3[,34]) <- c("date")

palouse4 <- subset(palouse3, year >= input$firstyear & year <= input$lastyear)

palouse4 <- subset(palouse4, commodity == input$commodity)

palouse4 <- subset(palouse4, damagecause == input$damage)

#palouse4 <- subset(palouse4, county == input$county)
palouse4 <- data.table(palouse4)
#palouse3$date <- as.numeric(palouse3$date)
#mydates$Months <- factor(mydates$Months, levels = paste(month.abb, c(rep(12, 12), 13)))

library(plyr)

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


combined3cov.df  <- combined2.df[,list(pr=sd(pr)/mean(pr)*100, tmmx=sd(tmmx)/mean(tmmx)*100, tmmx=sd(tmmx)/mean(tmmx)*100, tmmn=sd(tmmn)/mean(tmmn)*100, bi=sd(bi)/mean(bi)*100, th=sd(th)/mean(th)*100, pdsi=sd(pdsi)/mean(pdsi)*100, pet=sd(pet)/mean(pet)*100, erc=sd(erc)/mean(erc)*100, rmin=sd(rmin)/mean(rmin)*100, rmax=sd(rmax)/mean(rmax)*100, srad=sd(srad)/mean(srad)*100, sph=sd(sph)/mean(sph)*100, vs=sd(vs)/mean(vs)*100, fm100=sd(fm100)/mean(fm100)*100, fm1000=sd(fm1000)/mean(fm1000)*100), by = monthyear]
combined3mean.df  <- combined2.df[,list(pr=mean(pr), tmmx=mean(tmmx), tmmn=mean(tmmn), bi=mean(bi), th=mean(th), pdsi=mean(pdsi), pet=mean(pet), erc=mean(erc), rmin=mean(rmin), rmax=mean(rmax), srad=mean(srad), sph=mean(sph), vs=mean(vs), fm100=mean(fm100), fm1000=mean(fm1000)), by = monthyear]

#combined3cov.df <- data.frame(combined3cov.df, na.rm=TRUE)
#combined3mean.df <- combined3mean.df[, input$climate, drop = FALSE]
#combined3cov.df <- combined3cov.df[, input$climate, drop = FALSE]

#variable <- paste("combined3", input$var, ".df$", input$climate,  sep=""), 
#newone <- get(variable)

layout(matrix(c(1,2,1,2,3,4,3,4,5,6,5,6,7,8,7,8,9,10,9,10,11,12,11,12),12, 2, byrow=TRUE), widths=c(1.3,.7), heights=c(1,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5))

#-by year
totalb <- aggregate(loss~date,palouse4,sum)
#palouse_loss_year1 <- palouse4[,list(loss=sum(loss)), by = date]
totalc <- merge(palouse6, totalb, type = "date", all=T)

totalc[is.na(totalc)] <- 0
totalc <- subset(totalc, year >= input$firstyear & year <= input$lastyear)


replace_multiple <- function(x, m){
    len_x <- length(x)
    index_to_replace <- seq(1, len_x, by = m)
     
    #differ <- setdiff(x, index_to_replace)
    #index_to_replace2 <- x[!index_to_replace]
    x[index_to_replace] <- ' '
    return(x)
}

totald <- replace_multiple(totalc$date, 2)

par(mar=c(6,6,4,2)+1)


c = totalc[seq(1, length(totalc),12)]
#bplott <- barplot(totalc$loss)
barplot(totalc$loss, names.arg=totald, col="blue", las=2, cex.names=1, cex.main=1.5, cex.axis=1, main = paste(input$damage, " Damage cause ", input$firstyear, " to ",  input$lastyear, "\n", "Commodity: ",  input$commodity, sep=""))
#axis(1, at=bplott, labels = c$date, las = 2)






#-by county
palouse_loss_county <- palouse4[,list(loss=sum(loss)), by = county]

tt <- colorRampPalette(c("light blue", "dark blue"))

orderedcolors2 <- tt(length(palouse_loss_county$loss))[order(palouse_loss_county$loss)]
palouse_loss_county <- cbind(palouse_loss_county, orderedcolors2)
names(allcounties)[1] <- "county"
palouse_counties <- merge(allcounties, palouse_loss_county, by = 'county')

plot(palouse_counties, col = palouse_loss_county$orderedcolors2, cex.names=1, cex.main=1.5, cex.axis=1, main = paste(input$damage, " Palouse region damage cause ", input$firstyear, " to ",  input$lastyear, "\n", "Commodity: ",  input$commodity, sep=""))


climvar <- input$climate
for (i in climvar) {

plot(eval(parse(text=paste("combined3", input$var, ".df$", i, sep=""))), ylab=i, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""),na.rm=TRUE)
#points(paste("combined3", input$var,  ".df", sep=""))
#lines(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))))
lines(eval(parse(text=paste("combined3", input$var, ".df$", i, sep=""))), ylab=i, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""), na.rm=TRUE)
  library(RGraphics)
  library(gridExtra)
  meanplot <- mean(eval(parse(text=paste("combined3", input$var, ".df$", i , sep=""))))
  meanplot <- mean(eval(parse(text=paste("combined3", input$var, ".df$", i , sep=""))))
    text = paste( i, "mean for time period is: ", meanplot)

library("PerformanceAnalytics")
textplot(text, cex=1.5,  halign="left", valign="center")

}

##plot(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))), ylab=input$climate, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""),na.rm=TRUE)
#points(paste("combined3", input$var,  ".df", sep=""))
#lines(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))))
##lines(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))), ylab=input$climate, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""), na.rm=TRUE)

#plot(combined3mean.df)
 
#barplot(palouse2$loss)




#---------end
#setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries2/")
#climate_summary <- read.csv(paste("/dmine/data/USDA/agmesh-scenarios/palouse/summaries2/Oregon_2008_palouse_summary"))


 
})
})

output$plotmatrix <- renderPlot({
#req(input$commodity)
withProgress(message = 'Working', value = 0, {
   


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

#palouse4 <- subset(palouse4, county == input$county)

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

combined3cov.df  <- combined2.df[,list(pr=sd(pr)/mean(pr)*100, tmmx=sd(tmmx)/mean(tmmx)*100, tmmx=sd(tmmx)/mean(tmmx)*100, tmmn=sd(tmmn)/mean(tmmn)*100, bi=sd(bi)/mean(bi)*100, th=sd(th)/mean(th)*100, pdsi=sd(pdsi)/mean(pdsi)*100, pet=sd(pet)/mean(pet)*100, erc=sd(erc)/mean(erc)*100, rmin=sd(rmin)/mean(rmin)*100, rmax=sd(rmax)/mean(rmax)*100, srad=sd(srad)/mean(srad)*100, sph=sd(sph)/mean(sph)*100, vs=sd(vs)/mean(vs)*100, fm100=sd(fm100)/mean(fm100)*100, fm1000=sd(fm1000)/mean(fm1000)*100), by = monthyear]
combined3mean.df  <- combined2.df[,list(pr=mean(pr), tmmx=mean(tmmx), tmmn=mean(tmmn), bi=mean(bi), th=mean(th), pdsi=mean(pdsi), pet=mean(pet), erc=mean(erc), rmin=mean(rmin), rmax=mean(rmax), srad=mean(srad), sph=mean(sph), vs=mean(vs), fm100=mean(fm100), fm1000=mean(fm1000)), by = monthyear] 


#combined3cov.df  <- combined2.df[,list(pr=sd(pr)/mean(pr)*100, tmmx=sd(tmmx)/mean(tmmx)*100), by = monthyear]
#combined3mean.df  <- combined2.df[,list(pr=mean(pr), tmmx=mean(tmmx)), by = monthyear]

#variable <- paste("combined3", input$var, ".df$", input$climate,  sep=""), 
#newone <- get(variable)

layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))

#-by year
palouse_loss_year1 <- palouse4[,list(loss=sum(loss)), by = date] 
#barplot(palouse_loss_year1$loss)

#-by county
palouse_loss_county <- palouse4[,list(loss=sum(loss)), by = county] 

tt <- colorRampPalette(c("light blue", "red")) 

orderedcolors2 <- tt(length(palouse_loss_county$loss))[order(palouse_loss_county$loss)]
palouse_loss_county <- cbind(palouse_loss_county, orderedcolors2)
names(allcounties)[1] <- "county"
palouse_counties <- merge(allcounties, palouse_loss_county, by = 'county')
#plot(palouse_counties, col = palouse_loss_county$orderedcolors2)

#plot(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))), ylab=input$climate, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""))
#points(paste("combined3", input$var,  ".df", sep=""))
#lines(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))))
#lines(eval(parse(text=paste("combined3", input$var, ".df$", input$climate, sep=""))), ylab=input$climate, xlab=paste(input$firstyear, " to ", input$lastyear, sep=""))


library(dplyr)

newzz <- which( colnames(combined2.df)==input$climate )

#newzz <- data.frame(combined3mean.df[,input$climate])
#plot(combined6mean.df)
#plot(combined3mean.df)
#barplot(palouse2$loss)
#newzz <- select(combined3mean.df, select_if(input$climate))
newzz <- combined3mean.df[,newzz]
plot(newzz)

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
  req(input$commodity)
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
   
   setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/summary"))
   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/palouse/summaries")

   i <- read.csv("2001_2015_palouse_summary")
   DT2 <- unique(i$commodity)
   DT2 <- as.data.frame(DT2)


   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   #uf2 <- as.data.frame( uf2 )

   #uf3 <- subset(uf2, V1 == input$year)

  selectizeInput("commodity", "STEP 1: Choose a commodity", DT2[,1], choices = as.vector(DT2[,1]), selected = "WHEAT", multiple = FALSE)

})


output$damagecontrols <- renderUI({

   #i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
   #yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")

   setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/summary"))
   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/palouse/summaries")

   x <- read.csv("2001_2015_palouse_summary")

   setwd(yeardir)
   #x <- as.data.frame(read.csv(i))
   DT <- data.table(x)
   #DTa <- subset(DT, county == input$county)
   #DTa <- subset(DT, county == input$county & commodity == input$commodity)
   DTa <- subset(DT, commodity == input$commodity)
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

