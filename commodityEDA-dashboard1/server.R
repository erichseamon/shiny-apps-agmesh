library(datasets)
library(rgdal)
library(leaflet)
library(png)
library(jpeg)
library(ncdf)   
library(maptools)
library(data.table)
library(raster)

# Define server logic required to summarize and view the selected
# dataset

shinyServer(function(input, output) {


#observeEvent(
#      eventExpr = input[["submit_loc"]],
#      handlerExpr = {
#        print("PRESSED") #simulation code can go here
#      }
#    )



output$climmatrixgraph <- renderPlot({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  { 

input$submit2

isolate({





library(plyr)
climate_cropcombo_county_xy <- function(commodity1, damage1, climate_variable, response, matrixnumber) {

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

statez = c("Idaho", "Washington", "Oregon")
Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")


combinedlist2 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
combinedlist <- c(Idaho_list1, Washington_list1, Oregon_list1)

#alllist <- c("Idaho", "Oregon", "Washington")


#--Oregon

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
palouse_Oregon_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]
kk="Oregon"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
OR_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Oregon_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#--loop list for county by fip
#countyfiploop <- counties@data$FIPS

#--data frame of county fip list
#countyfiplist <- data.frame(counties@data$FIPS)

#--data frame of county names
#countynames <- data.frame(counties@data$NAME)

#combo of county names and fip for this list
#countylist <- cbind(countynames, countyfiplist)

#--number of rows in county list
#countylistrows <- 12 * nrow(countylist)



#---Washington



setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

wa_counties <- counties[grep("Washington", counties@data$STATE_NAME),]
palouse_Washington_counties <- wa_counties[grep(Washington_list1, wa_counties@data$NAME),]
kk="Washington"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
WA_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Washington_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#-----Idaho


setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
palouse_Idaho_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]
kk="Idaho"
#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
ID_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Idaho_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

counties <- rbind(ID_counties, WA_counties, OR_counties)

c_statelist <- paste(counties$STATE_NAME, "_", counties$NAME, sep="")
varr <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")
dmvectorappend <- data.frame(c(1), c(1), c(1), c(1), c(1), c(1), c(1))
#for (jj in varr) {
for (ll in c_statelist) {
  

  statey <- strsplit(ll, "_" )[[1]]
  state1 <- statey[1]
  county1 <- statey[2]
  state2 <- state.abb[match(statey[1],state.name)]
  
  #jj = "pet"
  #commodity1 <- "WHEAT"
  #damage1 <- "Drought"
  #response <- "cube_root_loss"
  #climate_variable <- jj
  ppp = matrixnumber
  
  
  #design_matrix_construction <- function(state1, county1, commodity1, damage1, climate_variable, response)   {
    #library(plotly)
    #packageVersion('plotly')
    
    
    
    #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss
    
    #monthlist is jan-dec, each repeated 12 times 
    monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))  
    #numlist is 12 months, repeated 12 times
    numlist <- as.data.frame(rep((1:12), times = 12))
    #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
    monthnumlist <- as.data.frame(cbind(monthlist, numlist))
    #renaming columns
    colnames(monthnumlist) <- c("month", "monthcode")
    #put the monthlist all together in one vector
    monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
    #rename the vector
    climmonth <- monthnumlist$combined
    
    designmatrix <- matrix(NA, ncol=12, nrow=12)
    
    #create an empty 144 vector to fill up with correlations between loss and climate
    dmvector <- data.frame(rep(NA, times=144))
    dmvectora <- data.frame(rep(NA, times=144))
    
    
    dmvector <- cbind(dmvector, dmvectora)
    
    
    cl = 0
    #for (ppp in climmonth) {
      cl = cl +1
      
      
      setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix")
      file1 <- read.csv(paste(state2, "_", county1, "_", commodity1, "_", damage1, "_", ppp, ".csv", sep=""))
      climatevar <- as.data.frame(cbind((file1[climate_variable]), file1[2]))
      
      
      setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries")
      file2 <- as.data.frame(read.csv(paste(state2, "_", county1, "_", commodity1, "_", damage1, "_", response, ".csv", sep="")))
      file2 <- subset(file2, state == state2)
      file2 <- subset(file2, county == county1)
      

      
      climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
      colnames(climatevar[3]) <- "zscore"
      kor <- join(climatevar, file2, by = "year")
      kor2 <- subset(kor, damagecause != "NA")
      colnames(kor2)[3] <- "zscore"
      kor3 <- cor(kor2[1], kor2[9])
      
      #insert kor3 into designmatrix iteratively
      
      dmvector <- cbind(kor2[1], kor2[9], kor2[2])
      
      
  
    
    dmvector <- as.data.frame(dmvector)
    colnames(dmvector) <- c(climate_variable, "loss", "year")
    
  
    
  #}
  
  #library(RColorBrewer)
  #coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)
  #heatmap(dmvector3, Rowv=NA, Colv=NA, col = coul)
  #design_matrix_construction(state1a, county1, commodity1, damage1, climate_variable, response)
  
  setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_correlations_xy")
  dmvector$state <- state2
  dmvector$county <- county1
  dmvector$commodity <- commodity1
  dmvector$matrixnumber <- matrixnumber
  #write.csv(dmvector, file = paste(state2, "_", county1, "_", commodity1, "_", damage1, "_", climate_variable, "_", response, "_", ppp, "_correlations_xy.csv", sep=""))
  

    
  names(dmvectorappend) <- names(dmvector)
  dmvectorappend <- rbind(dmvectorappend, dmvector)
  
}

dmvectorappend <- dmvectorappend[-1, ]

return(dmvectorappend)

}




#coldm <- brewer.pal(length(lll), "Spectral")

#palette(c("blue","pink","green"))

matrixy <- paste(input$monthmatrix_end, input$monthmatrix_number, sep="")


dmvectorappend <- climate_cropcombo_county_xy(input$commodity12, input$damage12, input$climate_variable12, input$predictor12, matrixy)


library(RColorBrewer)

lll <- unique(dmvectorappend$county)
lll2 <- length(lll)
coldm <- colorRampPalette(brewer.pal(11, "Spectral"))




color_pallet_function <- colorRampPalette(
  colors = c("red", "orange", "blue"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
  )

num_colors <- length(unique(dmvectorappend$state))
graph_colors <- color_pallet_function(num_colors)




statelll <- unique(dmvectorappend$state)
statelll2 <- length(statelll)
statelll3 <- colorRampPalette(brewer.pal(3, "Spectral"))







#dmvectorappend$clim_zscore <- scale(dmvectorappend[1], center = TRUE, scale = TRUE)
#dmvectorappend$loss_zscore <- scale(dmvectorappend[2], center = TRUE, scale = TRUE)


clim_zscore <- scale(dmvectorappend[1], center = TRUE, scale = TRUE)
loss_zscore <- scale(dmvectorappend[2], center = TRUE, scale = TRUE)

clim_zscore <- as.data.frame(clim_zscore)
loss_zscore <- as.data.frame(loss_zscore)

names(clim_zscore) <- "clim_zscore" 
names(loss_zscore) <- "loss_zscore"

dmvectorappend <- cbind(dmvectorappend, clim_zscore, loss_zscore)

if (input$saveit == "save") {


write.csv(dmvectorappend, paste("/waf/tmp/", input$climate_variable12, "_", dmvectorappend$matrixnumber[1], "_", input$predictor12,  "_climatecorrelation.csv", sep=""))

} 

corr <- cor(dmvectorappend$clim_zscore, dmvectorappend$loss_zscore)
corr <- round(corr, 3)
#par(xpd = T, mar = par()$mar + c(0,0,0,7))

#plot(dmvectorappend[,8], dmvectorappend[,9], type="p", main = paste("All Palouse counties, 1989-2015: ", input$climate_variable12, " vs ", input$predictor12, "\n correlation: ", corr, sep=""), xlab = paste("zscore ", input$climate_variable12, sep=""), ylab = paste("zscore ", input$predictor12, sep=""), col=graph_colors, pch=19)

#legend("topright", legend = unique(dmvectorappend$state),
#       cex = 0.8,
#       pch = 19,
#       fill = graph_colors
#       )


#legend("bottomleft", inset=.02, title="Counties",
#   unique(dmvector$county), fill=coldm, horiz=TRUE, cex=0.8)

dmvectorappend <- as.data.frame(dmvectorappend)

library(ggplot2)
#require(ggplot2)

#state <- dmvectorappend$state

#eventReactive(input$submit, {

RR <- round(corr^2, 3)

ggplot2::qplot(dmvectorappend$clim_zscore, dmvectorappend$loss_zscore,  data = dmvectorappend, main = paste("All Palouse counties, 1989-2015: ", input$climate_variable12, " vs ", input$predictor12,  "\n correlation: ", corr, "- R2: ", RR, sep=""), main.cex=1.5, xlab = paste("zscore ", input$climate_variable12, sep=""), ylab = paste("zscore ", input$predictor12, sep=""), colour = state) + theme(plot.title = element_text(hjust = 0.5))
# })

#p1 <- ggplot(dmvectorappend, aes_string(y = clim_zscore, x = loss_zscore)

#p1 + geom_point(aes(color = state)) 

})


})
})
















output$climmatrix <- renderPlot({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  {
input$submit2
isolate({
library(gplots)
library(plyr)
library(dplyr)
    #library(plotly)
    #packageVersion('plotly')
    
     #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss
  
  #monthlist is jan-dec, each repeated 12 times 
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))  
  #numlist is 12 months, repeated 12 times
  numlist <- as.data.frame(rep((1:12), times = 12))
  #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  #renaming columns
  colnames(monthnumlist) <- c("month", "monthcode")
  #put the monthlist all together in one vector
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  #rename the vector
  climmonth <- monthnumlist$combined
  
  designmatrix <- matrix(NA, ncol=12, nrow=12)
  
  #create an empty 144 vector to fill up with correlations between loss and climate
  dmvector <- as.data.frame(rep(NA, times=144))
  
  cl = 0
  for (ppp in climmonth) {
    cl = cl +1
  
  
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix")
    file1 <- read.csv(paste(input$state12, "_", input$county12, "_", input$commodity12, "_", input$damage12, "_", ppp, ".csv", sep=""))
    climatevar <- as.data.frame(cbind((file1[input$climate_variable12]), file1[2]))
  
  
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries")
    file2 <- as.data.frame(read.csv(paste(input$state12, "_", input$county12, "_", input$commodity12, "_", input$damage12, "_", input$predictor12, ".csv", sep="")))
    file2 <- subset(file2, state == input$state12)
    file2 <- subset(file2, county == input$county12)
    
   climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
   colnames(climatevar[3]) <- "zscore"
    kor <- join(climatevar, file2, by = "year")
    kor2 <- subset(kor, damagecause != "NA")
   colnames(kor2)[3] <- "zscore"
  # kor2[9] <- as.numeric(kor2$zscore)
   kor3 <- cor(kor2[1], kor2[9])
  
   #insert kor3 into designmatrix iteratively
  
   dmvector[cl,] <- kor3

  }
  
  dmvector <- as.data.frame(dmvector)
  colnames(dmvector) <- "correlations"




dmvector2 <- (matrix(dmvector$correlations, 12, 12, TRUE) ) 
  dmvector2 <- dmvector2[nrow(dmvector2):1, ]
  dmvector3 <- dmvector2[4:12,]





dmv <- which.max(abs( dmvector[1:108,1]) )
dmv <- as.data.frame(dmv)
colnames(dmv)[1] <- "row"

#dmvector1a <- max(dmvector$correlations)
#dmvector1b <- data.frame(which=dmvector, correlations=dmvector[dmvector1a, ])

monthnumlist2 <- cbind(as.data.frame(c(1:144)), monthnumlist)
colnames(monthnumlist2)[1] <- "row"

monthnumlist3 <- subset(monthnumlist2, row == dmv$row)



#makeRects <- function(tfMat){
require(utils)
cAbove <- expand.grid(1:12, 1:9)[monthnumlist3$row,]

monthzzz <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 108)
  
  setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_pngs")
 
#  png(filename=paste(state1, "_", county1, "_", damage1, "_", climate_variable12, "_designmatrix.png", sep=""))
  lblcol <- c(9:1)
 dmvector3a <- round(dmvector3, digits = 2)


newone <<- as.data.frame(expand.grid(1:12, 1:9)[monthnumlist3$row,])


  nba_heatmap <- heatmap.2(dmvector3, Rowv=NA, Colv=NA, col=topo.colors(16), scale="none", dendrogram = "none", trace="none", labRow = rev(monthzzz), tracecol = "black", key = TRUE, cellnote = dmvector3a, notecol = "black", notecex = 1.5, key.ylab = "", key.xlab = "loss correlation range", key.title = "", main=paste(input$climate_variable12, " vs ", input$predictor12, "\n", input$county12, " County ", input$state12, " ", input$damage12, "\n", "MAX Correlation in cell: ",  monthnumlist3$combined, sep=""), add.expr=points(newone$Var1,newone$Var2, border="red",pch=15, cex = 5.5 , col=rgb(139/255, 0, 0, 0.5)))


dmvector3




#  dev.off()
  
  

  
  





})






})

})

output$climmatrixtable <- renderDataTable({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  {
input$submit2
isolate({

library(gplots)
library(plyr)
library(dplyr)
    #library(plotly)
    #packageVersion('plotly')

     #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss

  #monthlist is jan-dec, each repeated 12 times
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))
  #numlist is 12 months, repeated 12 times
  numlist <- as.data.frame(rep((1:12), times = 12))
  #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  #renaming columns
  colnames(monthnumlist) <- c("month", "monthcode")
  #put the monthlist all together in one vector
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  #rename the vector
  climmonth <- monthnumlist$combined

  designmatrix <- matrix(NA, ncol=12, nrow=12)

  #create an empty 144 vector to fill up with correlations between loss and climate
  dmvector <- as.data.frame(rep(NA, times=144))

  cl = 0

 
cellselect <- paste(input$monthmatrix_end, input$monthmatrix_number, sep="")

ppp <- cellselect

#  for (ppp in climmonth) {
    cl = cl +1


    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix")
    file1 <- read.csv(paste(input$state12, "_", input$county12, "_", input$commodity12, "_", input$damage12, "_", ppp, ".csv", sep=""))
    climatevar <- as.data.frame(cbind((file1[input$climate_variable12]), file1[2]))


    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries")
    file2 <- as.data.frame(read.csv(paste(input$state12, "_", input$county12, "_", input$commodity12, "_", input$damage12, "_", input$predictor12, ".csv", sep="")))
    file2 <- subset(file2, state == input$state12)
    file2 <- subset(file2, county == input$county12)

   climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
   colnames(climatevar[3]) <- "zscore"
    kor <- join(climatevar, file2, by = "year")
    kor2 <- subset(kor, damagecause != "NA")
   colnames(kor2)[3] <- "zscore"
  # kor2[9] <- as.numeric(kor2$zscore)
   kor3 <- cor(kor2[1], kor2[9])


#insert kor3 into designmatrix iteratively

#   dmvector[cl,] <- kor3
#  }

#  dmvector <- as.data.frame(dmvector)
#  colnames(dmvector) <- "correlations"
#  dmvector2 <- (matrix(dmvector$correlations, 12, 12, TRUE) )
#  dmvector2 <- dmvector2[nrow(dmvector2):1, ]
#  dmvector3 <- dmvector2[4:12,]

#dmvector4 <- data.table(round(dmvector3, digits = 3))

#dmvector4

colzz <- c(1,2,9)
korprint <- data.table(kor2[colzz])
#climatevar4 <- data.table(climatevar2)

})

})
})



output$climmatrixtable2 <- renderDataTable({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  {

input$submit2
isolate({

library(gplots)
library(plyr)
library(dplyr)
    #library(plotly)
    #packageVersion('plotly')

     #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss

  #monthlist is jan-dec, each repeated 12 times
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))
  #numlist is 12 months, repeated 12 times
  numlist <- as.data.frame(rep((1:12), times = 12))
  #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  #renaming columns
  colnames(monthnumlist) <- c("month", "monthcode")
  #put the monthlist all together in one vector
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  #rename the vector
  climmonth <- monthnumlist$combined

  designmatrix <- matrix(NA, ncol=12, nrow=12)

  #create an empty 144 vector to fill up with correlations between loss and climate
  dmvector <- as.data.frame(rep(NA, times=144))

  cl = 0
 
cellselect <- paste(input$monthmatrix_end, input$monthmatrix_number, sep="")
 
  ppp <- cellselect



#  for (ppp in climmonth) {
    cl = cl +1


    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix")
    file1 <- read.csv(paste(input$state12, "_", input$county12, "_", input$commodity12, "_", input$damage12, "_", ppp, ".csv", sep=""))
    climatevar <- as.data.frame(cbind((file1[input$climate_variable12]), file1[2]))


    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries")
    file2 <- as.data.frame(read.csv(paste(input$state12, "_", input$county12, "_", input$commodity12, "_", input$damage12, "_", input$predictor12, ".csv", sep="")))
    file2 <- subset(file2, state == input$state12)
    file2 <- subset(file2, county == input$county12)

   climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
   colnames(climatevar[3]) <- "zscore"
    kor <- join(climatevar, file2, by = "year")
    kor2 <- subset(kor, damagecause != "NA")
   colnames(kor2)[3] <- "zscore"
  # kor2[9] <- as.numeric(kor2$zscore)
   kor3 <- cor(kor2[1], kor2[9])


#insert kor3 into designmatrix iteratively

#   dmvector[cl,] <- kor3
#  }

#  dmvector <- as.data.frame(dmvector)
#  colnames(dmvector) <- "correlations"
#  dmvector2 <- (matrix(dmvector$correlations, 12, 12, TRUE) )
#  dmvector2 <- dmvector2[nrow(dmvector2):1, ]
#  dmvector3 <- dmvector2[4:12,]

#dmvector4 <- data.table(round(dmvector3, digits = 3))

#dmvector4


korprint <- data.table(cbind(kor2[3], kor2[9]))
#climatevar4 <- data.table(climatevar2)

})

})

})




output$climmatrixtable3 <- renderText({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  {
input$submit2
isolate({

library(gplots)
library(plyr)
library(dplyr)
    #library(plotly)
    #packageVersion('plotly')

     #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss

  #monthlist is jan-dec, each repeated 12 times
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))
  #numlist is 12 months, repeated 12 times
  numlist <- as.data.frame(rep((1:12), times = 12))
  #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  #renaming columns
  colnames(monthnumlist) <- c("month", "monthcode")
  #put the monthlist all together in one vector
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  #rename the vector
  climmonth <- monthnumlist$combined

  designmatrix <- matrix(NA, ncol=12, nrow=12)

  #create an empty 144 vector to fill up with correlations between loss and climate
  dmvector <- as.data.frame(rep(NA, times=144))

  cl = 0
  
  cellselect <- paste(input$monthmatrix_end, input$monthmatrix_number, sep="")

#truly <- climmonth[cellselect]
paste("Singular Matrix test for ", cellselect, "\n", input$county12, ", ", input$state12, sep="")
#paste("<br>Singular Matrix test for</br>", kor3 , sep="")

})

})

})







output$climmatrixtable4 <- renderText({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  {
input$submit2
isolate({

library(gplots)
library(plyr)
library(dplyr)
    #library(plotly)
    #packageVersion('plotly')

     #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss

  #monthlist is jan-dec, each repeated 12 times
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))
  #numlist is 12 months, repeated 12 times
  numlist <- as.data.frame(rep((1:12), times = 12))
  #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  #renaming columns
  colnames(monthnumlist) <- c("month", "monthcode")
  #put the monthlist all together in one vector
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  #rename the vector
  climmonth <- monthnumlist$combined

  designmatrix <- matrix(NA, ncol=12, nrow=12)

  #create an empty 144 vector to fill up with correlations between loss and climate
  dmvector <- as.data.frame(rep(NA, times=144))

  cl = 0

  cellselect <- paste(input$monthmatrix_end, input$monthmatrix_number, sep="")

#truly <- climmonth[cellselect]
paste("Correlations for all counties, using singular matrix: ", cellselect, sep="")
#paste("<br>Singular Matrix test for</br>", kor3 , sep="")


})

})

})











output$climatematrixmap <- renderPlot({
 withProgress(message = 'Please Wait', value = 0,  {

#input$submit2
#isolate({
library(RColorBrewer)
library(dplyr)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_correlations/")


if (input$predictor12  == "loss") {

predictor <- "crop_commodity_loss"

}else {

predictor <- input$predictor12
}



files  <- list.files(pattern = predictor)
filey <- do.call(rbind, strsplit(files, '[_]'))

filey <- subset(filey, filey[,5] == input$climate_variable12)

colnames(filey) <- c("state", "county", "commodity", "damage", "climate", "crop1", "crop2", "response", "crop3")
filey <- as.data.frame(filey)
data <- with(filey, paste(state, "_", county, "_", commodity, "_", damage, "_", climate, "_", crop1, "_", crop2, "_", response, "_", crop3, sep=""))






tables <- lapply(data, read.csv, header = TRUE)



tables <- lapply(tables, function(x) { x["X"] <- NULL; x }) #--remove first index row from each list

tables <- lapply(tables, function(x) arrange(x, -row_number())) #--(flips matrix - puts jan as 1st row and sept as 9th row)


monthly <- match(input$monthmatrix_end, tolower(month.abb))



#!!!!!!fix-row by column, or number of months by ending month
table2 <- lapply(tables, function(x) x[monthly, as.numeric(input$monthmatrix_number)])


table3 <- data.frame(matrix(unlist(table2), nrow=length(table2), byrow=T))
colnames(table3) <- "correlations"
#combined <- do.call(rbind , tables)

table4 <- cbind(filey, table3)

#if (input$predictor12  == "loss") {

#input$predictor12 <- "crop_commodity_loss"

#}

table5 <- table4[c(2:5,10)]

colnames(table5) <- c("NAME", "COMMODITY", "DAMAGE", "climate", "correlations")

#table5$STATE_NAME <-  state.name[match(table5[,1],state.abb)]





setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

statez = c("Idaho", "Washington", "Oregon")
Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")


combinedlist2 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
combinedlist <- c(Idaho_list1, Washington_list1, Oregon_list1)

#alllist <- c("Idaho", "Oregon", "Washington")


#--Oregon

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
palouse_Oregon_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]
kk="Oregon"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
OR_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Oregon_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#--loop list for county by fip
#countyfiploop <- counties@data$FIPS

#--data frame of county fip list
#countyfiplist <- data.frame(counties@data$FIPS)

#--data frame of county names
#countynames <- data.frame(counties@data$NAME)

#combo of county names and fip for this list
#countylist <- cbind(countynames, countyfiplist)

#--number of rows in county list
#countylistrows <- 12 * nrow(countylist)



#---Washington



setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

wa_counties <- counties[grep("Washington", counties@data$STATE_NAME),]
palouse_Washington_counties <- wa_counties[grep(Washington_list1, wa_counties@data$NAME),]
kk="Washington"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
WA_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Washington_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#-----Idaho


setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
palouse_Idaho_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]
kk="Idaho"
#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
ID_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Idaho_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

counties <- rbind(ID_counties, WA_counties, OR_counties)




counties2 <- merge(counties, table5, by = "NAME" )





#colorbrew <- list(color = brewer.pal(26, c("green", "blue", "yellow")))
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 108)


colorss = colorRampPalette(brewer.pal(26,"Spectral"))

finalcol <- colorss(len <- length(counties2$correlations))
finalcol2 <- topo.colors(length(counties2$correlations))[order(order(counties2$correlations))]

cellselect <- paste(input$monthmatrix_end, input$monthmatrix_number, sep="")

par(mfrow=c(1,4))
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE)) 
par(mar = c(1,1,1,1) + 0.1)
plot(counties2, col = finalcol2, xaxs="i", yaxs="i")
text(coordinates(counties2), labels=round(counties2$correlations, 2), cex=1.5, col = "black")


maxx <- max(counties2$correlations)
minn <- min(counties2$correlations)

colfunc <- colorRampPalette(c("red", "blue"))
legend_image <- rev(as.raster(matrix(topo.colors(20), ncol=1)))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Correlation Range')
text(x=1.5, y = seq(0,1,l=5), labels = seq(round(minn, 2),round(maxx, 2), l=5), cex=1.5)
rasterImage(legend_image, .5, 0,1 ,1)



#})


})
})




















output$NASS <- renderPlot({
#req(input$state10)
  withProgress(message = 'Please Wait', value = 0,  {
library(RCurl)
library(jsonlite)

file <- read.table(paste("/dmine/data/NASS/NASS_", input$state10, "_1989_2015", sep=""))

file_frame <- as.data.frame(file)
file_name2 <- subset(file_frame, file_frame$data.Value != "NA")
file_name2 <- subset(file_name2, file_name2$data.county_name != "")
file_name2 <- subset(file_name2, file_name2$data.county_name != "OTHER (COMBINED) COUNTIES")
file_name2 <- subset(file_name2, file_name2$data.source_desc == "SURVEY")
file_name2 <- subset(file_name2, file_name2$data.county_name == input$county10)
file_name2 <- subset(file_name2, file_name2$data.statisticcat_desc == input$statdesc)


file_name2$data.Value <- (gsub(",","", file_name2[,'data.Value']))
file_name5 <- subset(file_name2, data.Value > 0)
file_name5 <- as.data.frame(file_name5)


x <- as.numeric(file_name5$data.Value)
y <- file_name5$data.year
boxplot(x~y, xlab="Year", ylab=paste(input$statdesc, sep=""), main=paste(input$state10, " ", input$county10, " ", "WHEAT for ", input$statdesc, sep=""),  las=3)

})
})


output$plot5aa <- renderPlot({
#req(input$commodity)
  withProgress(message = 'Please Wait', value = 0,  {

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
  withProgress(message = 'Please Wait', value = 0, {

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


output$plot5xx <- renderPlot({
  #req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {

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

output$plotpairtable <- renderDataTable({
#  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {

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
#myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
#myfiles_allyears[1:11] <- scale(myfiles_allyears[1:11], center = TRUE, scale = TRUE)

#data <- myfiles_allyears[1:7]
#data_orig <- myfiles_allyears[1:6]
yearpred <- myfiles_allyears[input$predictor]
data <- cbind(myfiles_allyears[input$climate], yearpred, county=myfiles_allyears$county, year=myfiles_allyears$year)




})
})



output$plot5nn <- renderPlot({
#  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {


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



output$plot5nn3 <- renderPlot({
#  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {


library(DAAG)
library(stats)
library(neuralnet)
library(boot)
library(plyr)

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



#data <- myfiles_allyears[1:7]
#data_orig <- myfiles_allyears[1:6]
yearpred <- myfiles_allyears[input$predictor]
data <- cbind(myfiles_allyears[input$climate], yearpred)
#colnames(data)[12] <- c(input$predictor)
colnames(data[ncol(data)]) <- c(input$predictor)


cv.error <- NULL
k <- 10

library(plyr) 

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))



for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
 n <- names(train.cv)
f <- as.formula(paste(input$predictor, " ~", paste(n[!n %in% input$predictor], collapse = " + "))) 
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
#  pr.nn <- compute(nn,test.cv[,1:6])
#  pr.nn <- pr.nn$net.result*(max(data$countratio)-min(data$countratio))+min(data$countratio)
  
#  test.cv.r <- (test.cv$countratio)*(max(data$countratio)-min(data$countratio))+min(data$countratio)
 
pr.nn <- compute(nn,test.cv[input$climate])

pr.nn <- pr.nn$net.result*(max(data[input$predictor])-min(data[input$predictor]))+min(data[input$predictor])
test.cv.r <- (test.cv[input$predictor])*(max(data[input$predictor])-min(data[input$predictor]))+min(data[input$predictor])




 
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
}

 avenn <- mean(cv.error)

boxplot(cv.error,xlab=paste("NN mean CV error =", avenn, sep=""), col='cyan',
         border='blue',names='CV error (MSE)',
         main='CV error (MSE) for NN',horizontal=TRUE)
 
})
})






output$plot5nn4 <- renderPlot({
#  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0,  {




set.seed(500)
library(MASS)


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



#data <- myfiles_allyears[1:7]
#data_orig <- myfiles_allyears[1:6]
yearpred <- myfiles_allyears[input$predictor]
data <- cbind(myfiles_allyears[input$climate], yearpred)
#colnames(data)[12] <- c(input$predictor)
colnames(data[ncol(data)]) <- c(input$predictor)

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]


library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)







plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
})
})








output$plot5nn2 <- renderPlot({
#  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0,  {


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

output$plot5ensemble <- renderPlot({
  #req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {

library(DAAG)
library(stats)
library(neuralnet)
library(mlbench)
library(caret)
library(caretEnsemble)

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

#data <- myfiles_allyears[1:7]
#data_orig <- myfiles_allyears[1:6]
yearpred <- myfiles_allyears[input$predictor]
data <- cbind(myfiles_allyears[1:7], loss=myfiles_allyears$loss_unscaled)


intrain <- createDataPartition(y = data$loss, p = .75, list = FALSE )
training <- data[intrain,]
testing <- data[-intrain,]


control <- trainControl(method="repeatedcv", number=10, repeats=10) 
algorithmList <- c('gbm', 'neuralnet')
set.seed(500)

models <- caretList(loss~., data=training, trControl=control, methodList=algorithmList)
#plot(models, rep="best")
results <- resamples(models)
#summary(results)
#plot(myfiles_allyears)
#dotplot(results)

})
})


output$plot6 <- renderPlot({
req(input$commodity)

  library(rnoaa)

  withProgress(message = 'Please Wait', value = 0,  {

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

  withProgress(message = 'Please Wait', value = 0,  {

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
  withProgress(message = 'Please Wait', value = 0,  {
    
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
       withProgress(message = 'Please Wait', value = 0,  {
    
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
  withProgress(message = 'Please Wait', value = 0,  {

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
  withProgress(message = 'Please Wait', value = 0, {

library(DAAG)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state,  "/summaries/annual_county_summaries/", sep=""))
#files <- list.files(pattern = "\\_WHEAT_drought$")
#myfiles = do.call(rbind, lapply(files, function(x) 
#  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year") 
#myfiles$prpet <- (myfiles$pr - myfiles$pet)
myfiles <- read.csv("WHEAT_drought_summary")


#myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, erc, countratio, loss, acres, count, county, year))
myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

#myfiles_allyears <- subset(myfiles, , c(tmmn, rmin, rmax, fm100, fm1000, pr, pdsi, pet, tmmx, erc, loss, acres, county, year))


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
 text(0.5, 0.5, txt, cex = 2)
  text(.8, .8, Signif, cex=2, col=2) 
  #text(0.5, 0.5, txt, cex = cex * r) 
  #text(.8, .8, Signif, cex=cex, col=2) 
}



#myfiles_allyears <- subset(myfiles_allyears, year == 2014) 
yearpred <- myfiles_allyears[input$predictor] 
data <- cbind(myfiles_allyears[input$climate], yearpred)
#colnames(data)[12] <- c(input$predictor)

colnames(data[ncol(data)]) <- c(input$predictor)



pairs(data, lower.panel=panel.smooth, upper.panel=panel.cor) 
#pairs(myfiles_allyears[c(1,2,3,4,5,6,8)], lower.panel=panel.smooth, upper.panel=panel.cor)


#pairs(myfiles_allyears[c(1,2,3,4,5,6,8)], lower.panel=panel.smooth, upper.panel=panel.cor)
  })
})



output$plot7yy <- renderPlot({
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {





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
counties <- counties[grep(input$county7, counties@data$NAME),]

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



setwd("/dmine/data/counties/")

    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state7)
    counties_one <- subset(counties, NAME %in% input$county7)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))                 

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 1, byrow=TRUE))

 

barplot(newmat5$claims, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), cex.names=1, las=3, col="blue", main = paste(input$state7, " crop claim counts  \n", input$month7, " ", input$year7, "\n", input$commodity7, sep=""), horiz=FALSE)

#par(mar=c(0,0,3,1)+1)

#plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county, " County map", sep=""))

#plot(counties_one, col="blue", add=T)

#plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
#axis(side=1, at=c(1:12))
#axis(side=2, at=seq(xxx, xxxx, by = interval))
#lines(newmat5$pr, las=2, col="blue")

  })
})


output$plot8gg <- renderDataTable({
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {

    i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")
      
monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    monthz <- data.frame(monthz)

    setwd(yeardir)
    x <- as.data.frame(read.csv(i, strip.white = TRUE))
    DT <- data.table(x)

    DT2 <- subset(DT, county == input$county7)
    DT2 <- subset(DT2, commodity == input$commodity7)
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




output$plot7ddd <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {





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
    counties <- counties[grep(input$county7, counties@data$NAME),]

    monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

  
  i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

    setwd(yeardir)
    x <- as.data.frame(read.csv(i, strip.white = TRUE))
    DT <- data.table(x)

    DT2 <- subset(DT, county == input$county7)
    DT2 <- subset(DT2, commodity == input$commodity7)
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



    setwd("/dmine/data/counties/")

    counties <- readShapePoly('UScounties.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    counties <- subset(counties, STATE_NAME %in% input$state7)
    counties_one <- subset(counties, NAME %in% input$county7)
    #layout(matrix(c(1,2),1, 2, byrow=TRUE))

    #par(mar=c(4,4,4,5))
    #par(mfrow = c(1, 2))

    #par(mar=c(0,3,3,2)+1)
    #par(mfrow=c(1,2))
    layout(matrix(c(1,2,3,4),1, 2, byrow=TRUE))



    barplot(newmat5$claims, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), cex.names=1, las=3, col="blue", main = paste(input$state7, " crop claim counts  \n", input$month7, " ", input$year7, "\n", input$commodity7, sep=""), horiz=FALSE)

    par(mar=c(0,0,3,1)+1)

    plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county7, " County map", sep=""))

    plot(counties_one, col="blue", add=T)

    #plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
    #axis(side=1, at=c(1:12))
    #axis(side=2, at=seq(xxx, xxxx, by = interval))
    #lines(newmat5$pr, las=2, col="blue")

  })
})






output$plot7dd <- renderPlot({
 # req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {


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
  withProgress(message = 'Please Wait', value = 0, {
    
    
    
    
    
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



output$plot8x <- renderDataTable({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {
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




output$plot8g <- renderDataTable({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {
    
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



#---------------------------



output$ghcnd <- renderPlot({
withProgress(message = 'Please Wait', value = 0, {

library(rnoaa)
library(maptools)
library(sp)

setwd("/dmine/data/ghcnd/ghcnd_admin/")
ghcnd_stations <- read.csv("ghcnd-stations_revised.csv")

xy <- ghcnd_stations[,c(3,2)]

#coordinates(ghcnd_stations) <- ~long + lat
summary(ghcnd_stations)


spdf <- SpatialPointsDataFrame(coords = xy, data = ghcnd_stations,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

columns <- c("ID", "YEAR", "MONTH", "ELEMENT", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN", "EVAP", "MNPN", "MXPN", "SN*#", "SX*#", "TOBS", "WDMV", "WESF", "WT**", "WTEQ", "VALUE1", "MFLAG1", "QFLAG1", "SFLAG1", "VALUE2", "MFLAG2", "QFLAG2", "SFLAG2")


setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                         proj4string=CRS
                         ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- counties[grep("Whitman", counties@data$NAME),]

subset_stations <- spdf[counties, ]

subset_stations_data <- meteo_pull_monitors(subset_stations$station)

datetxt <- subset_stations_data$date
datetxt <- as.Date(datetxt)
df <- data.frame(year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

subset_stations_data <- cbind(subset_stations_data, df)

subsetstations_month <- subset(subset_stations_data, month == input$month )


library(doBy)
tmax <- summaryBy(tmax ~ year, data = subsetstations_month, 
                 FUN = list(mean, max, min, median, sd))

library(doBy)
prcp <- summaryBy(prcp ~ year, data = subsetstations_month, 
          FUN = list(mean, max, min, median, sd))

par(mar=c(6,6,4,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2),1, 2, byrow=TRUE))

barplot(prcp$prcp.mean, main = "Title")

barplot(tmax$tmax.mean, names.arg = tmax$year, las=3)


})
})













output$croploss1b <- renderPlot({
withProgress(message = 'Please Wait', value = 0, {

#--bringing in county shapefile
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))

setwd(monthdir)





i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")

cpi <- data.frame(read.csv("/dmine/data/FRED/cpi/CPIAUCSL2001_2015.csv", header = TRUE, strip.white = TRUE))
ick <- subset(cpi, year == input$year)
input.monthz <- as.numeric(input$month)
sick <- as.data.frame(subset(ick, month == tolower(month.abb[input.monthz])))
colnames(sick) <- c("ratio")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
counties <- subset(counties, STATE_NAME %in% input$state)
  #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
  x <- as.data.frame(read.csv(i, strip.white = TRUE))

  sickk <- as.data.frame(rep(sick$ratio, each=nrow(x)))
  colnames(sickk) <- c("ratio")



  colnames(x) <- c("UNIQUEID", "YEAR", "NAME", "COMMODITYCODE", "MONTHCODE", "COMMODITY", "DAMAGECAUSE", "ACRES", "LOSS")
 #x$LOSS <- x$LOSS * sickk$ratio



  #u <- data.frame(trimws(x$county))
  #colnames(u) <- c("NAME")
  #z <- cbind(x,u)
  m <- merge(counties, x, by='NAME')
  m$LOSS[is.na(m$LOSS)] <- 0
  m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  m$ACRES[is.na(m$ACRES)] <- 0

  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("light blue",  "dark blue"), space = "Lab")
  mz <- subset(m, LOSS != 0)
  mzacres <- subset(m, ACRES > 0)
  lengacres <- length(m$ACRES)
  leng <- length(m$LOSS)
  len2 <- tt(len <- length(mz$LOSS))
  len2acres <- tt(len <- length(mzacres$ACRES))
  len2a <- length(mz$LOSS)
  len2a <- length(mzacres$ACRES)
  len3 <- tt(len <- length(m$LOSS))

  orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
  orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
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

  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

  for (jj in 1:leng){

    if (m$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
      newmatrix_acres[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj]
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }

  }

  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  newmatrix <- c(newmatrix)

  newmatrix_acres[newmatrix_acres==0] <- NA
  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
#newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix_acres[newmatrix_acres == NA] <- 0
  newmatrix_acres <- c(newmatrix_acres)


  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,6,4,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2),1, 2, byrow=TRUE))
  #--turn image horizontal

  plotmonth <- month.abb[x$MONTHCODE[1]]
  plotyear <- x$YEAR[1]
  plotcommodity <- x$COMMODITY[1]

  midpoint_loss <- (max(mz$LOSS) + min(mz$LOSS)/2)
  midpoint_acres <- (max(mzacres$ACRES) + min(mzacres$ACRES)/2)

  b <- barplot(mz$LOSS, names.arg = mz$NAME, las=2, cex.names = 1, cex.axis = 1,  col = newmatrix2, main = paste(input$state, " crop loss bar chart ($)  \n", plotmonth, " ", plotyear, "\n", input$commodity, sep=""), cex.main = 1, horiz=TRUE)
  #text(bb, midpoint_loss, labels=mz$LOSS, srt=90)
  plot(m, col = newmatrix, main = paste(input$state, " crop loss map ($)  \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), cex.main = 1, cex.axis = 1.5, cex.names = 1.5)

  #bb <- barplot(mzacres$ACRES, names.arg = mz$NAME, cex.names = 1.5, cex.axis = 1.5, las=2, col = newmatrix2acres, main = paste(input$state, " crop loss in ACRES bar chart  \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), cex.main = 1.5, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), cex.main = 1.5, cex.axis = 1.5, cex.names = 1.5)


})
})



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


output$TAMU <- renderDataTable(options = list(scrollX = TRUE, pageLength = 10),{

    withProgress(message = 'Please Wait', value = 0, {

setwd(paste("/dmine/data/TAMU/readings/", sep=""))
DT <- read.csv("TAMU_pnw_merged.csv", strip.white = TRUE)
DT <- data.frame(DT)
DT
})
})


output$USDA2 <- renderDataTable(options = list(scrollX = TRUE, pageLength = 10),{

    withProgress(message = 'Please Wait', value = 0,  {

input$submit3

isolate({


if (names(dev.cur()) != "null device") dev.off()
pdf(NULL)



setwd(paste("/dmine/data/USDA/agmesh-scenarios/Allstates", sep=""))
DT <- read.csv(paste(input$year, "_monthly_usda_", input$state, "_summary", sep=""), strip.white = TRUE)
DT <- data.frame(DT)
colnames(DT)[17] <- "ac"
colnames(DT)[16] <- c("loss")
DT_sum <- sum(DT$loss)
DT_mean <- mean(DT$loss)
DT_count <- nrow(DT)
DT_values <- paste(input$state, " ", input$year, sep="")
DT21 <- rbind(DT_values, DT_sum, DT_mean, DT_count)
DT21 <- as.data.frame(DT21)
row.names(DT21) <- c("year_and_state", "loss_summary", "mean_summary", "count_summary")
DT21$Description <- c("Year and State", "Loss Summary", "Mean Loss", "Total Claim Counts")
colnames(DT21)[1] <- c("Summary Values")
#colnames(DT21) <- paste("Summary Values", input$year, " ", input$state, sep="")
DT22 <- DT21[,c(2,1)]
DT22

})

})
})




output$USDA <- renderDataTable(options = list(scrollX = TRUE, pageLength = 10),{

    withProgress(message = 'Please Wait', value = 0,  {

input$submit3

isolate({

if (names(dev.cur()) != "null device") dev.off()
pdf(NULL)

layout(matrix(c(1,2),1, 2, byrow=TRUE))
setwd(paste("/dmine/data/USDA/agmesh-scenarios/Allstates", sep=""))
DT <- read.csv(paste(input$year, "_monthly_usda_", input$state, "_summary", sep=""), strip.white = TRUE)
DT <- data.frame(DT)
DT

})

})
})


output$USDAdetail <- renderDataTable(options = list(scrollX = TRUE, pageLength = 10),{

    withProgress(message = 'Please Wait', value = 0, {
if (names(dev.cur()) != "null device") dev.off()
pdf(NULL)


setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep=""))
DT <- read.csv(paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep=""), strip.white = TRUE)
DT <- data.frame(DT)
DT
})
})









output$lossdamage2 <- renderPlot({

    withProgress(message = 'Please Wait', value = 0, {

input$submit

isolate({

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_1989_2015_county_nosum",sep="")

setwd(monthdir2)

files <- list.files(pattern = paste(input$year2, ".*\\.", input$commodity, ".csv$", sep=""))
myfiles = lapply(files, read.csv, strip.white = TRUE, header = TRUE)
x <- do.call(rbind, myfiles)
DT <- data.table(x)










    setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/summaries/", sep=""))

 #   DT <- read.csv(paste(input$year2, "_monthly_usda_gridmet_post2001_", input$state2, sep=""), strip.white = TRUE)

   # DT <- data.table(DT)

    #DTnew <- data.frame(tolower(DT$commodity))

    #capFirst <- function(s) {

    #  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
    #}
   #zz <- as.numeric(input$month)
   #      plotmonth <- month.abb[zz]
    #DTnew2 <- data.frame(capFirst(DTnew$tolower.DT.commodity.))
    #colnames(DTnew2) <- c("commodity")
    #DTnew2$commodity <- as.character(DTnew2$commodity)

    #DTnew2$commodity[DTnew2$commodity == "All other crops"] <- "All Other Crops"

    #DTnew3 <- cbind(DT, DTnew2)
    #DTnew3$commodity <- DTnew3$commodity2

   # commodity_result <- subset(DT, commodity == input$commodity & year == input$year2)
    library(lattice)
    bw2 <- bwplot(damagecause ~ log(loss), data=DT, scales=list(x=list(rot=90, cex = 1.5), y=list(cex=1.2)), main = paste(input$state2, " damage cause for ", input$year2, "\n", "Commodity: ",  input$commodity, sep=""), cex.names = 1, cex.main = 1.4, cex.axis = 2)
print(bw2, position=c(0, 0, .5, 1))
})

})
})

output$lossdamage1 <- renderPlot({

       withProgress(message = 'Please Wait', value = 0, {

input$submit

isolate({
 
         setwd("/dmine/data/counties/")
 
         counties <- readShapePoly('UScounties.shp',
                                   proj4string=CRS
                                   ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
         projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
         counties <- subset(counties, STATE_NAME %in% input$state2)
         #counties_one <- subset(counties, NAME %in% input$county)
         yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/summaries/", sep="")
         setwd(yeardir)
         zz <- as.numeric(input$month)
         plotmonth <- month.abb[zz]


monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_1989_2015_county_nosum",sep="")

setwd(monthdir2)
i <- paste(input$year2, ".", input$month, ".", input$commodity, ".csv", sep="")

        commodity_year <- read.csv(i, strip.white = TRUE)



#         commodity <- read.csv(paste(input$year2, "_monthly_usda_gridmet_post2001_", input$state2, sep=""), strip.white = TRUE)

 #        commodity_year <- subset(commodity, commodity == input$commodity & year == input$year2 & monthcode == input$month)
         par(mfrow=c(2,2))
         #layout(matrix(c(1,2,3),1, 3, byrow=TRUE))

#plot(counties)
         tr <- plot(counties, main = paste("Crop Commodity Statewide Monthly Loss Report", "State: ", input$state2, "   Month ", input$month, "  Year: ", input$year2, "   Commodity: ", input$commodity, sep=""))
         library(lattice)
         bw1 <- bwplot(damagecause ~ log(loss), data=commodity_year, scales=list(x=list(rot=90, cex = 1.5), y=list(cex=1.2)), main = paste(input$state2, " damages cause for ", plotmonth, " ",  input$year2, "\n", "Commodity: ",  input$commodity, sep=""), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1)
print(bw1, position=c(0, 0, .5, 1))

         #print(tr2, position = c(0, 0, 0.5, 1), more = TRUE)
         #print(tra, position = c(0.5, 0, 1, 1))
         #plot(counties_one, col="blue", add=T)


})


       })
     })


output$croploss2 <- renderPlot({

  withProgress(message = 'Please Wait', value = 0, {

 setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state2)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/summaries/", sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/netcdf/pdsi_apr_", input$year2, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))

setwd(yeardir)
i <- paste(input$year2, "_monthly_usda_gridmet_post2001_", input$state2, sep="")
#i <- paste(input$year2, ".", input$month, ".", input$commodity, ".csv", sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state2)
  #counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
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
  DT2$acres <- as.numeric(DT2$acres)
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
  tt <- colorRampPalette(c("light blue", "dark blue"), space = "Lab")
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


  orderedcolors2 <- tt(length(m$loss))[order(m$loss)]
  orderedcolors3 <- tt(length(m$acres))[order(m$acres)]
  m[["loss"]][is.na(m[["loss"]])] <- 0
  m[["acres"]][is.na(m[["acres"]])] <- 0


  #newframe <- data.frame(m$LOSS)
  xx <- 1
  newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
#orderedloss <- order(m$loss)
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


  msub <- subset(m, loss != 0)

  xx <- 1
  newmatrix_sub <- matrix(data = NA, nrow = leng, ncol = 1)
  leng_sub <- length(msub$loss)
  for (jj in 1:leng_sub){
    if (msub$loss[jj] == 0) {
      #print("yes this worked, added 0")

      newmatrix_sub[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix_sub[jj,] <- len3[jj]
      newmatrix_sub[jj,] <- orderedcolors2[xx]
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
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,6,3,2)+1)
  par(mfrow=c(2,3))
  layout(matrix(c(1,2,3),1, 3, byrow=TRUE))
  #--turn image horizontal
  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]

  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)
  #par(mar=c(6,6,4,2)+1)
  b <- barplot(msub$loss, names.arg = msub$county, las=2, col = newmatrix_sub, cex.names=1.5, horiz=TRUE, main = paste(input$state2, " crop loss bar chart ($) \n", " ", plotyear, " ", input$commodity, sep=""), cex.axis=1.9, cex.main=2, width=4)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  plot(m, col = newmatrix, main = paste(input$state2, " crop loss map ($) \n", " ", plotyear, " ", input$commodity, sep=""), cex.main=2)









  #bb <- barplot(m$acres, names.arg = m$county, las=2, col = newmatrix_acres, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(input$state2, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})

})

output$croploss1a <- renderPlot({

  withProgress(message = 'Please Wait', value = 0, {

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
  DT2$acres <- as.numeric(DT2$acres)
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
  tt <- colorRampPalette(c("light blue", "dark blue"), space = "Lab")
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


  orderedcolors2 <- tt(length(m$loss))[order(m$loss)]
  orderedcolors3 <- tt(length(m$acres))[order(m$acres)]
  m[["loss"]][is.na(m[["loss"]])] <- 0
  m[["acres"]][is.na(m[["acres"]])] <- 0




  #newframe <- data.frame(m$LOSS)
  xx <- 1
  newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
#orderedloss <- order(m$loss)
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
  b <- barplot(m$loss, names.arg = m$county, las=2, col = newmatrix, cex.names=1, horiz=TRUE, main = paste(input$state, " crop loss bar chart ($) \n", " ", plotyear, "\n", input$commodity, sep=""), cex.axis=1.3, cex.main=1.5, width=4)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  plot(m, col = newmatrix, main = paste(input$state, " crop loss map ($) \n", " ", plotyear, "\n", input$commodity, sep=""), cex.main=1.5)

  #bb <- barplot(m$acres, names.arg = m$county, las=2, col = newmatrix_acres, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
})

})







output$croploss12 <- renderPlot({
withProgress(message = 'Please Wait', value = 0, {

#--bringing in county shapefile

input$submit

isolate({

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state2)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
#uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))

setwd(monthdir2)
#setwd(paste("/dmine/data/USDA/agmesh_scenarios/", input$state2, "/summaries4/", sep=""))

#i <- paste(input$year2, ".", input$month, ".", input$commodity, ".csv", sep="")

#files <- list.files(pattern = ".*\\.WHEAT.csv$")
files <- list.files(pattern = paste(input$year2, ".*\\.", input$commodity, ".csv$", sep=""))
myfiles = lapply(files, read.csv, strip.white = TRUE, header = TRUE)
x <- do.call(rbind, myfiles)
x <- as.data.frame(x)



##cpi <- data.frame(read.csv("/dmine/data/FRED/cpi/CPIAUCSL1989_2015.csv", header = TRUE, strip.white = TRUE))
##sick <- subset(cpi, year == input$year2)
#input.monthz <- as.numeric(input$month)
#sick <- as.data.frame(subset(ick, month == input.monthz))
#sick <- as.data.frame(subset(ick, month == tolower(month.abb[input.monthz])))

##sickmean <- mean(sick$ratio)
##colnames(sick) <- c("ratio")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state2)
  #counties <- counties[grep(input$state2, counties@data$STATE_NAME),]

  #setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month", sep=""))
  setwd(monthdir2)

#  x <- as.data.frame(read.csv(i, strip.white = TRUE))

##  sickk <- as.data.frame(rep(sickmean, each=nrow(x)))
##  colnames(sickk) <- c("ratio")
  # x <- x[,-c(8)]  #--remove ID field



colnames(x) <- c("ID", "YEAR", "MONTHCODE", "COMMODITY", "NAME", "LOSS")

# colnames(x) <- c("UNIQUEID", "YEAR", "NAME", "COMMODITYCODE", "MONTHCODE", "COMMODITY", "DAMAGECAUSE", "ACRES", "LOSS")
## x$LOSS <- x$LOSS * sickk$ratio

#---

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
  DT2 <- subset(DT, COMMODITY == input$commodity)
  DT2loss <- DT2[,list(LOSS=sum(LOSS)), by = NAME]
  DT6 <- DT2loss
  m <- merge(counties, DT6, by='NAME')



 #u <- data.frame(trimws(x$county))
  #colnames(u) <- c("NAME")
  #z <- cbind(x,u)
  #m <- merge(counties, x, by='NAME')
  m$LOSS[is.na(m$LOSS)] <- 0
  #m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  #m$ACRES[is.na(m$ACRES)] <- 0

  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("light blue",  "dark blue"), space = "Lab")
  mz <- subset(m, LOSS != 0)
  #mzacres <- subset(m, ACRES > 0)
  #lengacres <- length(m$ACRES)
  leng <- length(m$LOSS)
  len2 <- tt(len <- length(mz$LOSS))
  #len2acres <- tt(len <- length(mzacres$ACRES))
  len2a <- length(mz$LOSS)
  #len2a <- length(mzacres$ACRES)
  len3 <- tt(len <- length(m$LOSS))


orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
#  orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
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



# xx <- 1
#  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

#  for (jj in 1:leng){

#    if (m$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
#      newmatrix_acres[jj,] <- 0
#    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj]
#      newmatrix_acres[jj,] <- orderedcolors3[xx]
#      xx <- xx + 1
#    }

#  }

  msub <- subset(m, LOSS != 0)

  xx <- 1
  newmatrix_sub <- matrix(data = NA, nrow = leng, ncol = 1)
  leng_sub <- length(msub$LOSS)
  for (jj in 1:leng_sub){
    if (msub$LOSS[jj] == 0) {
      #print("yes this worked, added 0")

      newmatrix_sub[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix_sub[jj,] <- len3[jj]
      newmatrix_sub[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }}






  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  newmatrix <- c(newmatrix)


#  newmatrix_acres[newmatrix_acres==0] <- NA
#  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
#  newmatrix_acres[newmatrix_acres == NA] <- 0
#  newmatrix_acres <- c(newmatrix_acres)


  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))


par(mar=c(6,6,3,2)+1)
  par(mfrow=c(2,3))
  layout(matrix(c(1,2,3),1, 3, byrow=TRUE))
  #--turn image horizontal
  plotmonth <- month.abb[x$monthcode[1]]
  plotyear <- x$year[1]
  plotcommodity <- x$commodity[1]

  midpoint_loss <- (max(m$loss) + min(m$loss)/2)
  midpoint_acres <- (max(m$acres) + min(m$acres)/2)
  #par(mar=c(6,6,4,2)+1)
  b <- barplot(msub$LOSS, names.arg = msub$NAME, las=2, col = newmatrix_sub, cex.names=1.5, horiz=TRUE, main = paste(input$state2, " crop loss bar chart ($) \n", " ", plotyear, " ", input$commodity, sep=""), cex.axis=1.9, cex.main=2, width=4)
  #text(bb, midpoint_loss, labels=mz$loss, srt=90)
  plot(m, col = newmatrix, main = paste(input$state2, " crop loss map ($) \n", " ", plotyear, " ", input$commodity, sep=""), cex.main=2)
})

})
})



output$croploss11 <- renderPlot({
withProgress(message = 'Please Wait', value = 0, {

#--bringing in county shapefile

input$submit

isolate({

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state2)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))

setwd(monthdir2)


i <- paste(input$year2, ".", input$month, ".", input$commodity, ".csv", sep="")

cpi <- data.frame(read.csv("/dmine/data/FRED/cpi/CPIAUCSL1989_2015.csv", header = TRUE, strip.white = TRUE))
ick <- subset(cpi, year == input$year2)
input.monthz <- as.numeric(input$month)
sick <- as.data.frame(subset(ick, month == input.monthz))
#sick <- as.data.frame(subset(ick, month == tolower(month.abb[input.monthz])))
colnames(sick) <- c("ratio")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state2)
  #counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
  
  #setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month", sep=""))
  setwd(monthdir2)
  
  x <- as.data.frame(read.csv(i, strip.white = TRUE))

  sickk <- as.data.frame(rep(sick$ratio, each=nrow(x)))
  colnames(sickk) <- c("ratio")
  x <- x[,-c(8)]  #--remove ID field



colnames(x) <- c("ID", "YEAR", "MONTHCODE", "COMMODITY", "NAME", "LOSS")

# colnames(x) <- c("UNIQUEID", "YEAR", "NAME", "COMMODITYCODE", "MONTHCODE", "COMMODITY", "DAMAGECAUSE", "ACRES", "LOSS")
 x$LOSS <- x$LOSS * sickk$ratio



  #u <- data.frame(trimws(x$county))
  #colnames(u) <- c("NAME")
  #z <- cbind(x,u)
  m <- merge(counties, x, by='NAME')
  m$LOSS[is.na(m$LOSS)] <- 0
  #m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  #m$ACRES[is.na(m$ACRES)] <- 0

  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("light blue",  "dark blue"), space = "Lab")
  mz <- subset(m, LOSS != 0)
  #mzacres <- subset(m, ACRES > 0)
  #lengacres <- length(m$ACRES)
  leng <- length(m$LOSS)
  len2 <- tt(len <- length(mz$LOSS))
  #len2acres <- tt(len <- length(mzacres$ACRES))
  len2a <- length(mz$LOSS)
  #len2a <- length(mzacres$ACRES)
  len3 <- tt(len <- length(m$LOSS))


orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
#  orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
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



# xx <- 1
#  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

#  for (jj in 1:leng){

#    if (m$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
#      newmatrix_acres[jj,] <- 0
#    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj]
#      newmatrix_acres[jj,] <- orderedcolors3[xx]
#      xx <- xx + 1
#    }

#  }

  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  newmatrix <- c(newmatrix)

#  newmatrix_acres[newmatrix_acres==0] <- NA
#  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
#  newmatrix_acres[newmatrix_acres == NA] <- 0
#  newmatrix_acres <- c(newmatrix_acres)


  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,6,4,2)+1)
  par(mfrow=c(2,3))
  layout(matrix(c(1,2,3),1, 3, byrow=TRUE))
  #--turn image horizontal

  plotmonth <- month.abb[x$MONTHCODE[1]]
  plotyear <- x$YEAR[1]
  plotcommodity <- x$COMMODITY[1]

  midpoint_loss <- (max(mz$LOSS) + min(mz$LOSS)/2)
#  midpoint_acres <- (max(mzacres$ACRES) + min(mzacres$ACRES)/2)

  b <- barplot(mz$LOSS, names.arg = mz$NAME, las=2, cex.names = 1.5, cex.axis=1.9, col = newmatrix2, main = paste(input$state2, " crop loss bar chart ($)  \n", plotmonth, " ", plotyear, " ", input$commodity, sep=""), cex.main = 2, horiz=TRUE)
  #text(bb, midpoint_loss, labels=mz$LOSS, srt=90)
  plot(m, col = newmatrix, main = paste(input$state2, " crop loss map ($)  \n", plotmonth, " ", plotyear, " ", plotcommodity, sep=""), cex.main = 2, cex.axis = 1.9, cex.names = 1.5)

  #bb <- barplot(mzacres$ACRES, names.arg = mz$NAME, cex.names = 1.5, cex.axis = 1.5, las=2, col = newmatrix2acres, main = paste(input$state2, " crop loss in ACRES bar chart  \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), cex.main = 1.5, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(input$state2, " crop loss acres \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), cex.main = 1.5, cex.axis = 1.5, cex.names = 1.5)

})

})
})
















output$croploss1 <- renderPlot({
withProgress(message = 'Please Wait', value = 0, {

#--bringing in county shapefile
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state2)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/netcdf/pdsi_apr_", input$year2, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))

setwd(monthdir)


i <- paste(input$year2, ".", input$month, ".", input$commodity, ".csv", sep="")

cpi <- data.frame(read.csv("/dmine/data/FRED/cpi/CPIAUCSL2001_2015.csv", header = TRUE, strip.white = TRUE))
ick <- subset(cpi, year == input$year2)
input.monthz <- as.numeric(input$month)
sick <- as.data.frame(subset(ick, month == tolower(month.abb[input.monthz])))
colnames(sick) <- c("ratio")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state2)
  #counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month", sep=""))
  x <- as.data.frame(read.csv(i, strip.white = TRUE))

  sickk <- as.data.frame(rep(sick$ratio, each=nrow(x)))
  colnames(sickk) <- c("ratio")



  colnames(x) <- c("UNIQUEID", "YEAR", "NAME", "COMMODITYCODE", "MONTHCODE", "COMMODITY", "DAMAGECAUSE", "ACRES", "LOSS")
 #x$LOSS <- x$LOSS * sickk$ratio



  #u <- data.frame(trimws(x$county))
  #colnames(u) <- c("NAME")
  #z <- cbind(x,u)
  m <- merge(counties, x, by='NAME')
  m$LOSS[is.na(m$LOSS)] <- 0
  m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  m$ACRES[is.na(m$ACRES)] <- 0

  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("light blue",  "dark blue"), space = "Lab")
  mz <- subset(m, LOSS != 0)
  mzacres <- subset(m, ACRES > 0)
  lengacres <- length(m$ACRES)
  leng <- length(m$LOSS)
  len2 <- tt(len <- length(mz$LOSS))
  len2acres <- tt(len <- length(mzacres$ACRES))
  len2a <- length(mz$LOSS)
  len2a <- length(mzacres$ACRES)
  len3 <- tt(len <- length(m$LOSS))


orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
  orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
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

  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

  for (jj in 1:leng){

    if (m$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
      newmatrix_acres[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj]
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }

  }

  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  newmatrix <- c(newmatrix)

  newmatrix_acres[newmatrix_acres==0] <- NA
  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix_acres[newmatrix_acres == NA] <- 0
  newmatrix_acres <- c(newmatrix_acres)


  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,6,4,2)+1)
  par(mfrow=c(2,3))
  layout(matrix(c(1,2,3),1, 3, byrow=TRUE))
  #--turn image horizontal

  plotmonth <- month.abb[x$MONTHCODE[1]]
  plotyear <- x$YEAR[1]
  plotcommodity <- x$COMMODITY[1]

  midpoint_loss <- (max(mz$LOSS) + min(mz$LOSS)/2)
  midpoint_acres <- (max(mzacres$ACRES) + min(mzacres$ACRES)/2)

  b <- barplot(mz$LOSS, names.arg = mz$NAME, las=2, cex.names = 1.5, cex.axis=1.9, col = newmatrix2, main = paste(input$state2, " crop loss bar chart ($)  \n", plotmonth, " ", plotyear, " ", input$commodity, sep=""), cex.main = 2, horiz=TRUE)
  #text(bb, midpoint_loss, labels=mz$LOSS, srt=90)
  plot(m, col = newmatrix, main = paste(input$state2, " crop loss map ($)  \n", plotmonth, " ", plotyear, " ", plotcommodity, sep=""), cex.main = 2, cex.axis = 1.9, cex.names = 1.5)

  #bb <- barplot(mzacres$ACRES, names.arg = mz$NAME, cex.names = 1.5, cex.axis = 1.5, las=2, col = newmatrix2acres, main = paste(input$state2, " crop loss in ACRES bar chart  \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), cex.main = 1.5, horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(input$state2, " crop loss acres \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), cex.main = 1.5, cex.axis = 1.5, cex.names = 1.5)


})
})




















  output$plot <- renderPlot({
req(input$commodity)
withProgress(message = 'Please Wait', value = 0, {

#--bringing in county shapefile
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))

setwd(monthdir)


i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")

  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% input$state)
  #counties <- counties[grep(input$state, counties@data$STATE_NAME),]
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  
  colnames(x) <- c("UNIQUEID", "YEAR", "NAME", "COMMODITYCODE", "MONTHCODE", "COMMODITY", "DAMAGECAUSE", "ACRES", "LOSS")
  #u <- data.frame(trimws(x$county))
  #colnames(u) <- c("NAME")
  #z <- cbind(x,u)
  #x <- subset(x, damagecause == input$damage)
  m <- merge(counties, x, by='NAME')
  m$LOSS[is.na(m$LOSS)] <- 0
  m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  m$ACRES[is.na(m$ACRES)] <- 0
  
  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("blue", "orange", "red"), space = "Lab")
  mz <- subset(m, LOSS != 0)
  mzacres <- subset(m, ACRES > 0)
  lengacres <- length(m$ACRES)
  leng <- length(m$LOSS)
  len2 <- tt(len <- length(mz$LOSS))
  len2acres <- tt(len <- length(mzacres$ACRES))
  len2a <- length(mz$LOSS)
  len2a <- length(mzacres$ACRES)
  len3 <- tt(len <- length(m$LOSS))
  
  orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
  orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
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
  
  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
  
  for (jj in 1:leng){
    
    if (m$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
      newmatrix_acres[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj] 
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }
    
  }
  
  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  newmatrix <- c(newmatrix)
  
  newmatrix_acres[newmatrix_acres==0] <- NA
  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix_acres[newmatrix_acres == NA] <- 0
  newmatrix_acres <- c(newmatrix_acres)
  
  
  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  #png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,3,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
  #--turn image horizontal
  
  plotmonth <- month.abb[x$MONTHCODE[1]]
  plotyear <- x$YEAR[1]
  plotcommodity <- x$COMMODITY[1]
  
  midpoint_loss <- (max(mz$LOSS) + min(mz$LOSS)/2)
  midpoint_acres <- (max(mzacres$ACRES) + min(mzacres$ACRES)/2)
  
  b <- barplot(mz$LOSS, names.arg = mz$NAME, las=2, col = newmatrix2, main = paste(input$state, " crop loss in $ bar chart  \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), horiz=TRUE)
  #text(bb, midpoint_loss, labels=mz$LOSS, srt=90)
  plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""))
  
  bb <- barplot(mzacres$ACRES, names.arg = mz$NAME, las=2, col = newmatrix2acres, main = paste(input$state, " crop loss in ACRES bar chart  \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""), horiz=TRUE)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  plot(m, col = newmatrix_acres, main = paste(input$state, " crop loss acres \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""))
  
 
})
})



output$plot2aa <- renderDataTable({
req(input$commdity)
  withProgress(message = 'Please Wait', value = 0, {
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
    #withProgress(message = 'Please Wait', value = 0, {

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
    #withProgress(message = 'Please Wait', value = 0, {

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
  withProgress(message = 'Please Wait', value = 0, {

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
withProgress(message = 'Please Wait', value = 0, {


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
  withProgress(message = 'Please Wait', value = 0, {

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
  withProgress(message = 'Please Wait', value = 0, {
    
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
  withProgress(message = 'Please Wait', value = 0, {
    
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
    DT <- subset(DT, year == input$year)
    DT2 <- subset(DT, commodity == input$commodity)
    #DT2 <- subset(DTnew3, commodity == input$commodity)
    #DT3 <- data.frame(DT2$acres, DT2$loss)
    #DT4 <- cbind(x, DT3)
    DT2loss <- DT2[,list(loss=sum(loss)), by = county]
    DT2$acres <- as.numeric(DT2$acres)
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
    cols <- c("blue")  
 
    options(scipen=5)
    b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = cols, horiz=TRUE, cex.names = 0.9, cex.axis = 0.8, main = paste(input$commodity, " - Damage Report", sep=""))
    #text(bb, midpoint_loss, labels=mz$loss, srt=90)
    #plot(m, col = newmatrix, main = paste(input$state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
    par(mar=c(0,0,3,1)+1)
    
    #setwd("/dmine/data/counties/")
    
    #counties <- readShapePoly('UScounties.shp',
    #                          proj4string=CRS
    #                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    #projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    #counties <- subset(counties, STATE_NAME %in% input$state)
    #counties_one <- subset(counties, NAME %in% input$county)
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

  withProgress(message = 'Please Wait', value = 0, {

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

  withProgress(message = 'Please Wait', value = 0, {

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
  withProgress(message = 'Please Wait', value = 0, {
    
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
       withProgress(message = 'Please Wait', value = 0, {
    
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
  withProgress(message = 'Please Wait', value = 0, {

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
  withProgress(message = 'Please Wait', value = 0, {





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














output$plot7z <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {

input$submit
isolate({

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
#counties <- counties[grep(input$county, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/summaries/", sep="")

setwd(yeardir)

temp = list.files(pattern="*post2001*")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
x <- as.data.frame(ziggy.df)


x$commodity <- lapply(x$commodity, as.character)
x$commodity <- do.call(rbind , x$commodity)
x$commodity <- trim(x$commodity)

x$damagecause <- lapply(x$damagecause, as.character)
x$damagecause <- do.call(rbind , x$damagecause)
x$damagecause <- trim(x$damagecause)




#x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- x
#DT <- subset(DT, year == "2009")
#DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT, commodity == input$commodity)
#DT2 <- subset(DT2, damagecause == "Drought")

#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1
newmat5a <- NULL

for (mm in input$startyear:input$endyear) {
  
  newmat2 <- matrix(, ncol = 1, nrow = 12)
  
  
  
#--loss
newii <- 1
for (ii in monthzz) {
  DT3 <- subset(DT2, year == mm)
  
  nez <- subset(DT3, monthcode == ii)
  newmat2[ii, newjj] <- nrow(nez)
  newii <- newii + 1
}


newmat3 <- matrix(, ncol = 1, nrow = 12)

#--acres

newii <- 1
for (ii in monthzz) {
  DT4 <- subset(DT, year == mm)
  nez <- subset(DT4, monthcode == ii)
  newmat3[ii, newjj] <- sum(as.numeric(nez[,31]))
  newii <- newii + 1
}



newmat4 <- cbind(newmat2, newmat3)
colnames(newmat4) <- c("claims", "acres")
newmat <- as.data.frame(c(1:12))
newmatyear <- as.data.frame(c(1:12))
newmatyear$year <- mm
#newmat5 <- as.matrix(newmat4)
newmat5 <- cbind(newmatyear, newmat4)
colnames(newmat5) <- c("month", "year", "claims", "acres")

#rownames(newmat5) <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

newmat5a <- rbind(newmat5a, newmat5)
}

newmat5a$claims[newmat5a$claims == 0] <- NA
newmat5a <- subset(newmat5a, claims != "NA")
layout(matrix(c(1,2),1, 2, byrow=TRUE))
#-by month
boxplot(claims ~ month, data = newmat5a, names =unique(newmat5a$month), ylab = "claim frequency", xlab = "Months", las = 2, main= paste(input$state2, " Claim Frequency by Month, ", input$startyear, "-", input$endyear, " for ", input$commodity, sep=""))
stripchart(claims ~ month, data = newmat5a, 
           vertical = TRUE, method = "jitter", 
           pch = 21, col = "maroon", bg = "bisque", 
          add = TRUE )
#plot(DT2$loss)

#-all years
#boxplot(claims ~ year, data = newmat5a, names =unique(newmat5a$year), ylab = "claim frequency", xlab = "Years", las = 2, main= "Idaho Claim Frequency by Year")
#stripchart(claims ~ year, data = newmat5a, 
#           vertical = TRUE, method = "jitter", 
#           pch = 21, col = "maroon", bg = "bisque", 
#           add = TRUE) 


})


  })
})





















output$plot7zzz <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {

input$submit

isolate({

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
#counties <- counties[grep(input$county, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/month_1989/", sep="")

setwd(yeardir)

temp = list.files(pattern=paste(input$state2, sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
x <- as.data.frame(ziggy.df)
#x <- x[,c(1,3,2,4,5,6,7,8,9,10,11,12,13,14,15,17,16)]

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/month_2001/", sep="")

setwd(yeardir)

temp = list.files(pattern=paste(input$state2, sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xx <- as.data.frame(ziggy.df)

x <- rbind(x,xx)

x$commodity <- lapply(x$commodity, as.character)
x$commodity <- do.call(rbind , x$commodity)
x$commodity <- trim(x$commodity)

x$damagecause <- lapply(x$damagecause, as.character)
x$damagecause <- do.call(rbind , x$damagecause)
x$damagecause <- trim(x$damagecause)

#x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- x
#DT <- subset(DT, year == "2009")
#DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT, commodity == input$commodity)
#DT2 <- subset(DT2, damagecause == "Drought")

#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1
newmat5a <- NULL

for (mm in input$startyear:input$endyear) {

  newmat2 <- matrix(, ncol = 1, nrow = 12)



#--loss
newii <- 1
for (ii in monthzz) {
  DT3 <- subset(DT2, year == mm)

  nez <- subset(DT3, monthcode == ii)
  newmat2[ii, newjj] <- nrow(nez)
  newii <- newii + 1
}


#newmat3 <- matrix(, ncol = 1, nrow = 12)

#--acres

#newii <- 1
#for (ii in monthzz) {
#  DT4 <- subset(DT, year == mm)
#  nez <- subset(DT4, monthcode == ii)
#  newmat3[ii, newjj] <- sum(as.numeric(nez[,31]))
#  newii <- newii + 1
#}



newmat4 <- cbind(newmat2)
colnames(newmat4) <- c("claims")
newmat <- as.data.frame(c(1:12))
newmatyear <- as.data.frame(c(1:12))
newmatyear$year <- mm
#newmat5 <- as.matrix(newmat4)
newmat5 <- cbind(newmatyear, newmat4)
colnames(newmat5) <- c("month", "year", "claims")

#rownames(newmat5) <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")



newmat5a <- rbind(newmat5a, newmat5)
}

newmat5a$claims[newmat5a$claims == 0] <- NA
newmat5a <- subset(newmat5a, claims != "NA")
layout(matrix(c(1,2),1, 2, byrow=TRUE))
#-by month
#boxplot(claims ~ month, data = newmat5a, names =unique(newmat5a$month), ylab = "claim frequency", xlab = "Months", las = 2, main= "Washington Claim Frequency by Month, 2010-2015")
#stripchart(claims ~ month, data = newmat5a,
#           vertical = TRUE, method = "jitter",
#           pch = 21, col = "maroon", bg = "bisque",
#           add = TRUE)

#-all years


boxplot(claims ~ year, data = newmat5a, names =unique(newmat5a$year), ylab = "claim frequency", xlab = "Years", las = 2, main= paste(input$state2, " Claim Frequency by Year ", input$startyear, "-", input$endyear, sep=""))
stripchart(claims ~ year, data = newmat5a,
           vertical = TRUE, method = "jitter",
           pch = 21, col = "maroon", bg = "bisque",
           add = TRUE)

})


})
})










output$plot7zz <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {

input$submit

isolate({

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
#counties <- counties[grep(input$county, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/summaries/", sep="")

setwd(yeardir)

temp = list.files(pattern="*post2001*")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
x <- as.data.frame(ziggy.df)


x$commodity <- lapply(x$commodity, as.character)
x$commodity <- do.call(rbind , x$commodity)
x$commodity <- trim(x$commodity)

x$damagecause <- lapply(x$damagecause, as.character)
x$damagecause <- do.call(rbind , x$damagecause)
x$damagecause <- trim(x$damagecause)




#x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- x
#DT <- subset(DT, year == "2009")
#DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT, commodity == input$commodity)
#DT2 <- subset(DT2, damagecause == "Drought")

#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1
newmat5a <- NULL

for (mm in input$startyear:input$endyear) {
  
  newmat2 <- matrix(, ncol = 1, nrow = 12)
  
  
  
#--loss
newii <- 1
for (ii in monthzz) {
  DT3 <- subset(DT2, year == mm)
  
  nez <- subset(DT3, monthcode == ii)
  newmat2[ii, newjj] <- nrow(nez)
  newii <- newii + 1
}


newmat3 <- matrix(, ncol = 1, nrow = 12)

#--acres

newii <- 1
for (ii in monthzz) {
  DT4 <- subset(DT, year == mm)
  nez <- subset(DT4, monthcode == ii)
  newmat3[ii, newjj] <- sum(as.numeric(nez[,31]))
  newii <- newii + 1
}



newmat4 <- cbind(newmat2, newmat3)
colnames(newmat4) <- c("claims", "acres")
newmat <- as.data.frame(c(1:12))
newmatyear <- as.data.frame(c(1:12))
newmatyear$year <- mm
#newmat5 <- as.matrix(newmat4)
newmat5 <- cbind(newmatyear, newmat4)
colnames(newmat5) <- c("month", "year", "claims", "acres")

#rownames(newmat5) <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

newmat5a <- rbind(newmat5a, newmat5)
}

newmat5a$claims[newmat5a$claims == 0] <- NA
newmat5a <- subset(newmat5a, claims != "NA")
layout(matrix(c(1,2),1, 2, byrow=TRUE))
#-by month
#boxplot(claims ~ month, data = newmat5a, names =unique(newmat5a$month), ylab = "claim frequency", xlab = "Months", las = 2, main= "Washington Claim Frequency by Month, 2010-2015")
#stripchart(claims ~ month, data = newmat5a, 
#           vertical = TRUE, method = "jitter", 
#           pch = 21, col = "maroon", bg = "bisque", 
#           add = TRUE)

#-all years
boxplot(claims ~ year, data = newmat5a, names =unique(newmat5a$year), ylab = "claim frequency", xlab = "Years", las = 2, main= paste(input$state2, " Claim Frequency by Year ", input$startyear, "-", input$endyear, sep=""))
stripchart(claims ~ year, data = newmat5a, 
           vertical = TRUE, method = "jitter", 
           pch = 21, col = "maroon", bg = "bisque", 
           add = TRUE) 

})


})
})









output$plot7d <- renderPlot({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {
    
    
    
    
    
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

output$plot8g4 <- renderDataTable({
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {
#--

input$submit4

isolate({

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
counties <- counties[grep(input$county7, counties@data$NAME),]

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

i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")
#i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")
#yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))

DT <- data.table(x)

if (input$year7 <= 2000) {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss", "dummy")

} else {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

}

#DT <- subset(DT, state == input$state7)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, county == input$county7)
DT2 <- subset(DT2, commodity == input$commodity7)
DT2 <- subset(DT2, damagecause == input$damage7)

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

#  newii <- 1
#  for (ii in monthzz) {
#    nez <- subset(DT2, monthcode == ii)
#    newmat3[ii, newjj] <- sum(nez[,31])
#    newii <- newii + 1
#  }


    newmat4 <- cbind(monthz, newmat2)
    colnames(newmat4) <- c("MONTH", "CLAIMS")
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

#par(mfrow=c(2,2))
    
newmat4


})

})
})







output$plot8ynew <- renderDataTable({
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {
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
counties <- counties[grep(input$county7, counties@data$NAME),]

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

i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")
#i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")
#yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))

DT <- data.table(x)

if (input$year7 <= 2000) {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss", "dummy")

} else {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

}

#DT <- subset(DT, state == input$state7)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
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

#  newii <- 1
#  for (ii in monthzz) {
#    nez <- subset(DT2, monthcode == ii)
#    newmat3[ii, newjj] <- sum(nez[,31])
#    newii <- newii + 1
#  }

newmat4 <- cbind(monthz, newmat2)
newmat5 <- as.data.frame(newmat4)
colnames(newmat5) <- c("MONTH", "CLAIMS")
DT3 <- data.table(newmat5)
DT3
})
})








output$plot8annual <- renderDataTable({
 # req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {

input$submit4

isolate({

library(dplyr)


setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
i <- "PNW_summary_counts.csv"
ii <- "PNW_summary_sumloss.csv"
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
x2 <- as.data.frame(read.csv(ii, strip.white = TRUE))
x <- cbind(x, x2$loss)
colnames(x)[9] <- "loss"
x$county <- trimws(x$county[])
x$commodity <- trimws(x$commodity[])
x$damagecause <- trimws(x$damagecause[])
x$state <- trimws(x$state[])

#x <- subset(x, year == input$year7)
x <- subset(x, state == state.abb[match(input$state7, state.name)])
x <- subset(x, county == input$county7)
x <- subset(x, commodity == input$commodity7)
x <- subset(x, damagecause == input$damage7)

xx <- aggregate(count ~ year + state + county + commodity + damagecause, x, sum)
xx2 <- aggregate(loss ~ year + state + county + commodity + damagecause, x, sum)

yearvector <- as.data.frame(c(1989:2015))
monthvector <- as.data.frame(c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
colnames(monthvector) <- "month"
colnames(yearvector) <- "year"

xmerge <- join(yearvector, xx, by = "year")
xmerge2 <- join(yearvector, xx2, by = "year")

xmerge3 <- cbind(xmerge$year, xmerge$count, xmerge2$loss)
colnames(xmerge3) <- c("Year", "Insurance Claim Counts/Year", "Insurance Loss/Year")
xmerge3[is.na(xmerge3)] <- 0



xmerge3 <- data.table(xmerge3)

xmerge3


})

})
})









output$plot8fff <- renderDataTable({
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {
#--

input$submit4

isolate({

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
counties <- counties[grep(input$county7, counties@data$NAME),]

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

i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")
#i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")
#yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))

DT <- data.table(x)

if (input$year7 <= 2000) {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss", "dummy")

} else {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

}

#DT <- subset(DT, state == input$state7)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, county == input$county7)
DT2 <- subset(DT2, commodity == input$commodity7)
DT2 <- subset(DT2, damagecause == input$damage7)

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

#  newii <- 1
#  for (ii in monthzz) {
#    nez <- subset(DT2, monthcode == ii)
#    newmat3[ii, newjj] <- sum(nez[,31])
#    newii <- newii + 1
#  }

newmat4 <- cbind(monthz, newmat2)
newmat5 <- as.data.frame(newmat4)
colnames(newmat5) <- c("Month", "Loss - $")
DT3 <- data.table(newmat5)
DT3

})

})
})


output$plot7e <- renderPlot({
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {
#--

input$submit4

isolate({

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
counties <- counties[grep(input$county7, counties@data$NAME),]

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

i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")
#i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")
#yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))

DT <- data.table(x)

if (input$year7 <= 2000) {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss", "dummy")

} else {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

}

#DT <- subset(DT, state == input$state7)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, county == input$county7)
DT2 <- subset(DT2, commodity == input$commodity7)
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

#  newii <- 1
#  for (ii in monthzz) {
#    nez <- subset(DT2, monthcode == ii)
#    newmat3[ii, newjj] <- sum(nez[,31])
#    newii <- newii + 1
#  }

newmat4 <- newmat2
#newmat4 <- cbind(newmat2, newmat3)
colnames(newmat4) <- c("claims")
#colnames(newmat4) <- c("loss", "acres")
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
    counties <- subset(counties, STATE_NAME %in% input$state7)
    counties_one <- subset(counties, NAME %in% input$county7)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 2, byrow=TRUE))




barplot(newmat5$claims, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), las=2, col="blue", main = paste(input$state7, " crop loss $ \n", input$month7, " ", input$year7, "\n", input$commodity, sep=""), horiz=FALSE)

par(mar=c(0,0,3,1)+1)

plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county7, " County map", sep=""))

plot(counties_one, col="blue", add=T)

#plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
#axis(side=1, at=c(1:12))
#axis(side=2, at=seq(xxx, xxxx, by = interval))
#lines(newmat5$pr, las=2, col="blue")

})

  })
})








output$plot7y3 <- renderPlot({
  #req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {
#--

input$submit4

isolate({

library(maptools)
library(data.table)
library(plyr)
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
counties <- counties[grep(input$county7, counties@data$NAME),]

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

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
i <- "PNW_summary_counts.csv"
ii <- "PNW_summary_sumloss.csv"
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
x2 <- as.data.frame(read.csv(ii, strip.white = TRUE))
x <- cbind(x, x2$loss)
colnames(x)[9] <- "loss"
x$county <- trimws(x$county[])
x$commodity <- trimws(x$commodity[])
x$damagecause <- trimws(x$damagecause[])
x$state <- trimws(x$state[])

#x <- subset(x, year == input$year7)
x <- subset(x, state == state.abb[match(input$state7, state.name)])
x <- subset(x, county == input$county7)
x <- subset(x, commodity == input$commodity7)
x <- subset(x, damagecause == input$damage7)

xx <- aggregate(count ~ year + state + county + commodity + damagecause, x, sum)
xx2 <- aggregate(loss ~ year + state + county + commodity + damagecause, x, sum)

yearvector <- as.data.frame(c(1989:2015))
monthvector <- as.data.frame(c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
colnames(monthvector) <- "month"
colnames(yearvector) <- "year"

xmerge <- join(yearvector, xx, by = "year")
xmerge2 <- join(yearvector, xx2, by = "year")











#DT <- data.table(x)

#if (input$year7 <= 2000) {

#colnames(DT) <- c("ID", "month", "year", "damagecause", "county", "state", "commodity", "count", "loss")

#} else {

#colnames(DT) <- c("ID", "month", "year", "damagecause", "county", "state", "commodity", "count", "loss")

#}

#DT$count <- as.numeric(DT$count)




#DT <- subset(DT, state == input$state7)

#DT$county <- trimws(DT$county[])
#DT$commodity <- trimws(DT$commodity[])
#DT$damagecause <- trimws(DT$damagecause[])
##DT2 <- subset(DT, county == input$county7)
#DT2 <- subset(DT, commodity == input$commodity7)
#DT2 <- subset(DT2, damagecause == input$damage7)

#newmat2 <- matrix(, ncol = 1, nrow = 12)
#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
#monthzz <- unique(DT2$monthcode)
#newjj <- 1

#--loss
# newii <- 1
# for (ii in monthzz) {
#   nez <- subset(DT2, monthcode == ii)
#   newmat2[ii, newjj] <- nrow(nez)
#   newii <- newii + 1
# }

#newmat3 <- matrix(, ncol = 1, nrow = 12)

#--acres

#  newii <- 1
#  for (ii in monthzz) {
#    nez <- subset(DT2, monthcode == ii)
#    newmat3[ii, newjj] <- sum(nez[,31])
#    newii <- newii + 1
#  }

#newmat4 <- newmat2
##newmat4 <- cbind(newmat2, newmat3)
#colnames(newmat4) <- c("claims")
##colnames(newmat4) <- c("loss", "acres")
#newmat <- as.data.frame(c(1:12))
##newmat5 <- as.matrix(newmat4)
#newmat5 <- cbind(newmat, newmat4)
#rownames(newmat5) <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


#newmat5[is.na(newmat5)] <- 0

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
    counties <- subset(counties, STATE_NAME %in% input$state7)
    counties_one <- subset(counties, NAME %in% input$county7)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 2, byrow=TRUE))

yv <- c(1989:2015)
barplot(xmerge$count, names.arg=yv, cex.names=1, las=3, col="blue", xlab = "Years", ylab = "Insurance claim count",  main = paste(input$state7, " crop claim counts, 1989-2015  \n", input$month7, " ", input$county7, " county", "\n", input$commodity7, " loss due to ", input$damage7, sep=""), horiz=FALSE)

barplot(xmerge2$loss, names.arg=yv, cex.names=1, las=3, col="blue", xlab = "Years", ylab = "Insurance claim loss ($)",  main = paste(input$state7, " crop claim loss ($), 1989-2015  \n", input$month7, " ", input$county7, " county", "\n", input$commodity7, " loss due to ", input$damage7,  sep=""), horiz=FALSE)

par(mar=c(0,0,3,1)+1)


})

  })
})












output$plot7c <- renderPlot({
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {
#--

input$submit4

isolate({

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(input$state7, counties@data$STATE_NAME),]
counties <- counties[grep(input$county7, counties@data$NAME),]

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

i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")
#i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")
#yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))

DT <- data.table(x)

if (input$year7 <= 2000) {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss", "dummy")
  
} else {

colnames(DT) <- c("ID", "year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
  
}

#DT <- subset(DT, state == input$state7)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT2 <- subset(DT, county == input$county7)
DT2 <- subset(DT2, commodity == input$commodity7)
DT2 <- subset(DT2, damagecause == input$damage7)

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

#  newii <- 1
#  for (ii in monthzz) {
#    nez <- subset(DT2, monthcode == ii)
#    newmat3[ii, newjj] <- sum(nez[,31])
#    newii <- newii + 1
#  }

newmat4 <- newmat2
#newmat4 <- cbind(newmat2, newmat3)
colnames(newmat4) <- c("loss")
#colnames(newmat4) <- c("loss", "acres")
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
    counties <- subset(counties, STATE_NAME %in% input$state7)
    counties_one <- subset(counties, NAME %in% input$county7)





#layout(matrix(c(1,2),1, 2, byrow=TRUE))

#par(mar=c(4,4,4,5))
#par(mfrow = c(1, 2))                 

#par(mar=c(0,3,3,2)+1)
  #par(mfrow=c(1,2))
  layout(matrix(c(1,2,3,4),1, 2, byrow=TRUE))




barplot(newmat5$loss, names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), las=2, col="blue", main = paste(input$state7, " crop loss $ \n", input$month7, " ", input$year7, "\n", input$commodity, sep=""), horiz=FALSE)

par(mar=c(0,0,3,1)+1)

plot(counties, main = paste("Annual Loss and Acreage Report\n", input$county7, " County map", sep=""))

plot(counties_one, col="blue", add=T)

#plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
#axis(side=1, at=c(1:12))
#axis(side=2, at=seq(xxx, xxxx, by = interval))
#lines(newmat5$pr, las=2, col="blue")


})

  })
})


output$plot8 <- renderDataTable({
  req(input$commodity)
  withProgress(message = 'Please Wait', value = 0, {
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
  withProgress(message = 'Please Wait', value = 0, {
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
    DT <- subset(DT, year == input$year)
    DT <- subset(DT, commodity == input$commodity)
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
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {

i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")



monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
monthz <- data.frame(monthz)

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

DT2 <- subset(DT, county == input$county7)
DT2 <- subset(DT2, commodity == input$commodity7)
DT2 <- subset(DT2, damagecause == input$damage7)

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
  req(input$commodity7)
  withProgress(message = 'Please Wait', value = 0, {
    
    i <- paste(input$year7, "_monthly_usda_gridmet_post2001_", input$state7, sep="")
    yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/summaries/", sep="")
    
    
    
    monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    monthz <- data.frame(monthz)
    
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
  withProgress(message = 'Please Wait', value = 0, {
    
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

output$county12controls <- renderUI({
req(input$state12)
withProgress(message = 'Please Wait', value = 0, {


Oregon_list1 <- c("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa")
Idaho_list2 <- c("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai")
Washington_list1 <- c("Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin")



if (input$state12 == "WA") {

list <-as.data.frame(Washington_list1)

}


if (input$state12 == "ID") {

list <-as.data.frame(Idaho_list2)

}


if (input$state12 == "OR") {

list <- as.data.frame(Oregon_list1)

}

selectizeInput("county12", "Choose a county", list, choices = as.vector(list), selected = as.vector(list[,1]), multiple = FALSE)



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

output$damage8controls <- renderUI({

#i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")

setwd(yeardir)

files  <- list.files(pattern = input$state8)
tables <- lapply(files, read.csv, header = TRUE, strip.white = TRUE)
allfiles <- do.call(rbind , tables)

allfiles <- as.data.frame(allfiles)

#x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(allfiles)


DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
DT22 <- subset(DT, state = input$state8)
DT2 <- subset(DT22, commodity == input$commodity8)
#DT2 <- subset(DT2, county == input$county8)
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
  selectizeInput("damage8", "Cause of loss", uf2[,1], choices = as.vector(uf2[,1]), selected = "Drought", multiple = FALSE)

})







output$commodity8controls <- renderUI({




#i <- paste(input$year7, "_monthly_usda_", input$state7, "_summary", sep="")

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")

setwd(yeardir)


files  <- list.files(pattern = input$state8)
tables <- lapply(files, read.csv, header = TRUE, strip.white = TRUE)
allfiles <- do.call(rbind , tables)

allfiles <- as.data.frame(allfiles)






#x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(allfiles)

DT$county <- trimws(DT$county[])
DT$commodity <- trimws(DT$commodity[])
DT$damagecause <- trimws(DT$damagecause[])
#DT2 <- subset(DT2, commodity == input$commodity7)
DT2 <- unique(DT$commodity)

   #uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state7, "/month_positive/", sep=""))
   #elems <- unlist( strsplit( uniquez, "\\." ) )
   #uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   #uf2 <- as.data.frame( uf2 )

   #uf3 <- subset(uf2, V1 == input$year7)

   #uf3 <- subset(uf2, V1 == input$startyear:input$endyear)

   uf3 <- as.data.frame(DT2)

  selectizeInput("commodity8", "Commodity", uf3[,1], choices = as.vector(uf3[,1]), selected = "WHEAT")

})








output$video <- renderUI({
statecom <- input$state8
inputcom <- input$commodity8
damagecom <- input$damage8
inputcom <- gsub(" ", "", inputcom, fixed = TRUE)

lll2 <- gsub(" ", "_", damagecom)
lll2 <- gsub("/", "_", lll2)
lll2 <- gsub("\\(", "_", lll2)
lll2 <- gsub("\\)", "_", lll2)
lll2 <- gsub(" ", "_", lll2)

damagecom <- lll2



  dir <- paste("http://dmine.io/waf/predictor_USDA/agmesh-scenarios/", input$state8, "/month_png2/timelapse/",input$state8, "_",  inputcom, "_", damagecom, "_",  "timelapse.mp4", sep="")
  tags$video(src = dir, type = "video/mp4", autoplay = NA, controls = NA, width = "900")
})



output$county10controls <- renderUI({
withProgress(message = 'Please Wait', value = 0, {
i <- paste("/dmine/data/NASS/NASS_", input$state10, "_1989_2015", sep="")

#yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")

DT <- read.table(i, strip.white = TRUE)
#DT <- data.table(x)

DT$county <- trimws(DT$data.county_name[])




file_name2 <- subset(DT, DT$data.Value != "NA")
file_name2 <- subset(file_name2, file_name2$data.county_name != "")
file_name2 <- subset(file_name2, file_name2$data.county_name != "OTHER (COMBINED) COUNTIES")
file_name2 <- subset(file_name2, file_name2$data.county_name != "NA")
#file_name2 <- subset(file_name2, file_name2$data.source_desc == input$census_survey)
#file_name2 <- subset(file_name2, file_name2$data.statisticcat_desc == "YIELD")


DT2 <- unique(file_name2$data.county_name)
selectizeInput("county10", "County", file_name2$data.county_name, choices = as.vector(DT2),  multiple = FALSE)

})
})

output$statdesccontrols <- renderUI({
withProgress(message = 'Please Wait', value = 0, {
i <- paste("/dmine/data/NASS/NASS_", input$state10, "_1989_2015", sep="")

#yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/Allstates/", sep="")

DT <- read.table(i, strip.white = TRUE)
#DT <- data.table(x)

DT$county <- trimws(DT$data.county_name[])




file_name2 <- subset(DT, DT$data.Value != "NA")
file_name2 <- subset(file_name2, file_name2$data.county_name != "")
file_name2 <- subset(file_name2, file_name2$data.county_name != "OTHER (COMBINED) COUNTIES")
#file_name2 <- subset(file_name2, file_name2$data.source_desc == input$census_survey)
#file_name2 <- subset(file_name2, file_name2$data.statisticcat_desc == "YIELD")
file_name2 <- subset(file_name2, file_name2$data.county_name == input$county10 )

DT2 <- unique(file_name2$data.statisticcat_desc)



 selectizeInput("statdesc", "NASS Variables", file_name2$data.county_name, choices = as.vector(DT2), selected = "PRODUCTION", multiple = FALSE)


})
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











output$commoditycontrols <- renderUI({
   uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_1989_2015_positive/", sep=""))
   elems <- unlist( strsplit( uniquez, "\\." ) )
   uf2 <- matrix( elems , ncol = 4 , byrow = TRUE )
   uf2 <- as.data.frame( uf2 )
   
   uf3 <- subset(uf2, V1 == input$startyear:input$endyear)
  selectizeInput("commodity", "Commodity", uf3[,3], choices = as.vector(uf3[,3]), selected = "WHEAT")

})


output$damagecontrols <- renderUI({



   i <- paste(input$year, "_monthly_usda_gridmet_post2001_", input$state, sep="")
   yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/summaries/", sep="")

   setwd(yeardir)
   x <- as.data.frame(read.csv(i, strip.white = TRUE))
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
  selectizeInput("damage", "Cause of loss", uf2[,1], choices = as.vector(uf2[,1]), selected = "Drought", multiple = FALSE)

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

