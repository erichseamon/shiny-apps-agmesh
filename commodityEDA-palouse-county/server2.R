
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

output$plotpairtable <- renderDataTable({
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



output$plot5nn3 <- renderPlot({
#  req(input$commodity)
  withProgress(message = 'Working', value = 0, {


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
  withProgress(message = 'Working', value = 0, {




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

output$plot5ensemble <- renderPlot({
  #req(input$commodity)
  withProgress(message = 'Working', value = 0, {

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

