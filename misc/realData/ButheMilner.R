#Buthe and Milner replication replication file
library(foreign)
library(lmtest)
library(kinship)
library(Formula)
library(plm)
library(sandwich)
library(MASS)
library(maxLik)
library(corpcor)
library(xtable)
source("vcovSACCchange.R")
setwd("../Code")
source("bootstrapIM.normal.R")

#Function for Arellano standard errors, consistent with Buthe/Milner analysis
######################################################################
clx <- function(fm, pr, time, cluster){
  library(sandwich)
  library(lmtest)
  T <- length(unique(time))
  N <- length(cluster)
  dfc <- (N)/(N - T)
  #dfc <- 1
  u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)
  coeftest(fm, vcovCL)
}

#Grab already detrended data
setwd("../Data")
fdi <- read.dta("MilnerButhe2.dta")

#Replication of Table 1, Model 4, no date^2
############################################
vars.4 <- c("fdi_detrended", "lag_ln_pop_det", "lag_ln_gdp_pc_95d_det", "lag_gdp_gr_det",
            "date", "country", "lag_bits_cuml_restricted_det", "lag_polconiii_2002_det",
            "lag_polinstability_wvo", "lag_gattwto_det", "lag_pta_cuml_det",
            "fdi_inflow_unctad_gdp", "ctylabel", "lag_gattwto_det")
fdi4 <- na.omit(fdi[,vars.4])
fdi.plm <- pdata.frame(fdi4, index=c("country", "date"), row.names=TRUE)
formula <- fdi_detrended ~ lag_pta_cuml_det + lag_gattwto_det + lag_bits_cuml_restricted_det+
  lag_polconiii_2002_det + lag_polinstability_wvo + lag_ln_pop_det + lag_ln_gdp_pc_95d_det +
  lag_gdp_gr_det + factor(country) - 1
formula1 <- fdi_detrended ~ 1+ lag_pta_cuml_det + lag_gattwto_det + lag_bits_cuml_restricted_det +
  lag_polconiii_2002_det + lag_polinstability_wvo + lag_ln_pop_det + lag_ln_gdp_pc_95d_det +
  lag_gdp_gr_det
se4 <- lm(formula, data=fdi4)
se4p <- plm(formula1, data=fdi.plm)

set.seed(02138)
#IMtest <- bootstrapIM.normal(formula1, fdi.plm, 99, 50, time=fdi4$date)
#IMtest$pval
#Im test pvalue .01

#Altered model, with quadratically detrended variables
########################################################
vars.4 <- c("fdi_inflow_unctad_gdp", "lag_pta_cuml", "lag_gattwto", "lag_trade_pgdp", "lag_bits_cuml_restricted", "lag_polconiii_2002", "lag_polinstability_wvo", "lag_ln_pop", "lag_ln_gdp_pc_95d", "lag_gdp_gr","date", "country", "fdi_detrended", "lag_pta_cuml_det", "ctylabel", "region_wb")
fdi4 <- na.omit(fdi[,vars.4])
fdi4$date2 <- fdi4$date^2
fdi4$date3 <- fdi4$date^3

for(i in 1:14){
  lm.temp <- lm(fdi4[,c(vars.4[i])] ~ fdi4$date*factor(fdi4$country) + fdi4$date2*factor(fdi4$country) + factor(fdi4$country)-1)
  fdi4$temp <- lm.temp$residuals
  names(fdi4)[names(fdi4)=="temp"] <- paste(vars.4[i], "_det2", sep="")
}


fdi.plm <- pdata.frame(fdi4, index=c("country", "date"), row.names=TRUE)

newformula <- fdi_detrended_det2 ~ lag_pta_cuml_det2 + lag_gattwto_det2 + lag_bits_cuml_restricted_det2 + lag_polconiii_2002_det2 + lag_ln_pop_det2 + lag_polinstability_wvo_det2 + lag_ln_gdp_pc_95d_det2 + lag_gdp_gr_det2 
t1m4 <- plm(newformula, data=fdi.plm, model="within")

newformula2 <- fdi_detrended_det2 ~ lag_pta_cuml_det2 + lag_gattwto_det2 + lag_bits_cuml_restricted_det2 + lag_polconiii_2002_det2 + lag_ln_pop_det2 + lag_polinstability_wvo_det2 + lag_ln_gdp_pc_95d_det2 + lag_gdp_gr_det2 + factor(country) -1
se4.2 <- lm(newformula2, data=fdi4)

newformula3  <- fdi_detrended_det2 ~ lag_pta_cuml_det2 + lag_gattwto_det2 + lag_bits_cuml_restricted_det2 + lag_polconiii_2002_det2 + lag_ln_pop_det2 + lag_polinstability_wvo_det2 + lag_ln_gdp_pc_95d_det2 + lag_gdp_gr_det2
se4.3 <- lm(newformula3, data=fdi4)

#IMtest <- bootstrapIM.normal(newformula3, fdi.plm, 99, 50, time=fdi4$date)
#IMtest$pval
#.35

#Figure 7: Comparison of Detrending Strategies
 ########################################################
par(mfrow=c(1,3))
#Plot of Raw data Trends, part a
meanfdi <- tapply(fdi4$fdi_inflow_unctad_gdp, fdi4$date, mean)
meanpta <- tapply(fdi4$lag_pta_cuml, fdi4$date, mean)
names(meanfdi) <- names(meanpta) <-  seq(1970, 1999)
#plot(names(meanfdi), meanfdi, xlab="Date", ylab="FDI Inflows as % of GDP, Cumulative PTAs", type="l")
plot(names(meanfdi), meanfdi, xlab="Date", ylab="", type="l")
lines(names(meanpta), meanpta, col="red")
text(1988, 3, "Mean PTAs", col="red", cex=1.15)
text(1984, 1.55, "Mean FDI Inflows", cex=1.15)

#Plot of Milner Buthe Detrending, part b
meanfdi <- tapply(fdi4$fdi_detrended, fdi4$date, mean)
meanpta <- tapply(fdi4$lag_pta_cuml_det, fdi4$date, mean)
names(meanfdi) <- names(meanpta) <-  seq(1970, 1999)
#plot(names(meanfdi), meanfdi, xlab="Date", ylab="Detrended FDI Inflows as % of GDP, Cumulative PTAs", type="l")
plot(names(meanfdi), meanfdi, xlab="Date", ylab="", type="l")
lines(names(meanpta), meanpta, col="red")
text(1976, -.25, "Mean PTAs", col="red", cex=1.15)
text(1978, .8, "Mean FDI Inflows", cex=1.15)

#Plot of Our Detrending, part c
meanfdi <- tapply(fdi4$fdi_inflow_unctad_gdp_det2, fdi4$date, mean)
meanpta <- tapply(fdi4$lag_pta_cuml_det2, fdi4$date, mean)
names(meanfdi) <- names(meanpta) <-  seq(1970, 1999)
#plot(names(meanfdi), meanfdi, xlab="Date", ylab="Detrended FDI Inflows as % of GDP, Cumulative PTAs", type="l", ylim=c(-1.2,1.2))
plot(names(meanfdi), meanfdi, xlab="Date", ylab="", type="l", ylim=c(-1.2,1.2))
lines(names(meanpta), meanpta, col="red")
text(1987, 0.5, "Mean PTAs", col="red", cex=1.15)
text(1975, 0.6, "Mean FDI", cex=1.15)


#Figure 8: Chile, Singapore, and Egypt
############################################
countries <- sort(table(fdi4$ctylabel), decreasing=T)
par(mfrow=c(1,3))
for(i in c(9,55,15)){
	y <- residuals(se4)[fdi4$ctylabel==names(countries)[i]]
	dat <- subset(fdi4, fdi4$ctylabel==names(countries[i]))	
	lm1 <- lm(y ~ dat$date + I(dat$date^2))
	y.loess <- loess(y ~ dat$date)
	plot(y ~ dat$date, cex=1, pch=16, main=names(countries[i]), xlab="Time", ylab="Residuals", ylim=c(-6,6))
	y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
	y.loess <- y.loess[order(y.loess$dat.date),]
	lines(y.loess$dat.date, y.loess$V2, col="black")
	#curve(lm1$coefficients[1] + lm1$coefficients[2]*x + lm1$coefficients[3]*x^2, add=T, col="red")
	y <- residuals(se4.2)[fdi4$ctylabel==names(countries)[i]]
	dat <- subset(fdi4, fdi4$ctylabel==names(countries[i]))	
	lm1 <- lm(y ~ dat$date + I(dat$date^2))
	y.loess <- loess(y ~ dat$date)
	#plot(y ~ dat$date, cex=1, pch=16, main=names(countries[i]), xlab="Time", ylab="Residuals", ylim=c(-5,5))
	points(y ~ dat$date, cex=1, pch=16, col="red")
	y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
	y.loess <- y.loess[order(y.loess$dat.date),]
	lines(y.loess$dat.date, y.loess$V2, col="red")
	#curve(lm1$coefficients[1] + lm1$coefficients[2]*x + lm1$coefficients[3]*x^2, add=T, col="red")
}


#Figure 9:  Standardized Residuals for All Countries
#######################################################
countries <- sort(table(fdi4$ctylabel), decreasing=T)
par(mfrow=c(1,1))
countriest <- countries[countries>20]
col <- rainbow(length(countriest))

#Orginal model residuals
plot(c(100,100), col="white", ylim=c(-3,3), xlim=c(1970,2000), xlab="Time", ylab="Standardized Residuals", main="Std Residuals: Authors' Detrending")
for(i in 1:length(countriest)){
  y <- residuals(se4)[fdi4$ctylabel==names(countries)[i]]/sd(residuals(se4)[fdi4$ctylabel==names(countries)[i]])
  dat <- subset(fdi4, fdi4$ctylabel==names(countries[i]))
  lm1 <- lm(y ~ dat$date + I(dat$date^2))
  y.loess <- loess(y ~ dat$date, span=2)
                                         #plot(y ~ dat$date, cex=1, pch=16, main=names(countries[i]), xlab="Time", ylab="Residuals")
  y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
  y.loess <- y.loess[order(y.loess$dat.date),]
  if(lm1$coefficients[3]>0 & summary(lm1)$coefficients[3,3]<.1){
    lines(y.loess$dat.date, y.loess$V2, col="black", lwd=2, lty=2)
  }
  if(summary(lm1)$coefficients[3,3]>.1){
    lines(y.loess$dat.date, y.loess$V2, col="black", lwd=2, lty=2)
  }
  
  if(lm1$coefficients[3]<0 & summary(lm1)$coefficients[3,3]<.1){
    lines(y.loess$dat.date, y.loess$V2, col="black", lwd=2, lty=2)
  }
}

#Revised model residuals
countriest <- countries[countries>20]
col <- rainbow(length(countriest))
plot(c(100,100), col="white", ylim=c(-3,3), xlim=c(1970,2000), xlab="Time", ylab="Standardized Residuals", main="Std Residuals: Altered Detrending")
for(i in 1:length(countriest)){
  y <- residuals(se4.2)[fdi4$ctylabel==names(countries)[i]]/sd(residuals(se4.2)[fdi4$ctylabel==names(countries)[i]])
  dat <- subset(fdi4, fdi4$ctylabel==names(countries[i]))
  lm1 <- lm(y ~ dat$date + I(dat$date^2))
  y.loess <- loess(y ~ dat$date, span=2)
                                        #plot(y ~ dat$date, cex=1, pch=16, main=names(countries[i]), xlab="Time", ylab="Residuals")
  y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
  y.loess <- y.loess[order(y.loess$dat.date),]
  if(lm1$coefficients[3]>0 & summary(lm1)$coefficients[3,3]<.1){
    lines(y.loess$dat.date, y.loess$V2, col="red", lwd=2, lty=2)
  }
  if(summary(lm1)$coefficients[3,3]>.1){
    lines(y.loess$dat.date, y.loess$V2, col="red", lwd=2, lty=2)
  }
  
  if(lm1$coefficients[3]<0 & summary(lm1)$coefficients[3,3]<.1){
    lines(y.loess$dat.date, y.loess$V2, col="red", lwd=2, lty=2)
  }
}


#Figure 10: Comparison of Model Results
##############################################
par(mfrow=c(1,1))
par(mar=c(8,4,4,2))
plot(c(se4$coefficients[2]),1, xlim=c(-1.5,2.5),ylim=c(.5,2.5),pch=16, xlab="Change in FDI from joining the GATT/WTO", axes=F, ylab="", col='red1')

#Plot points and ci's for original model
ci <- c(1.08 -qt(.95, 23)*.41 , qt(.95, 23)*.41 + 1.08)
lines(ci, c(1,1), col="red1")
lines(confint(se4, level=.95)[2,], c(1.1,1.1), lty=1)
points(c(se4$coefficients[2]),1.1, col="black", pch=16)
points(c(se4.2$coefficients[2]),2, pch=16, col="red1")

#Plot points and ci's for altered model
ci <- c((se4.2$coefficients[2] - (qt(.95, 23)*.36)) , ((qt(.95, 23)*.36) +se4.2$coefficients[2]))
lines(ci, c(2,2), col="red1")
lines(confint(se4.2, level=.95)[2,], c(2.1,2.1), lty=1)
points(c(se4.2$coefficients[2]),2.1, col="black", pch=16)

#Plot axes and make it pretty
axis(1)
#axis(2, at=c(0,4),labels=c("", ""), las=1)
text(1.08,.94, "Robust SE's", col="red1", cex=.95)
text(1.08,1.16, "Classical SE's", col="black", cex=.95)
text(-.28,1.94, "Robust SE's", col="red1", cex=.95)
text(-.28,2.16, "Classical SE's", col="black", cex=.95)
text(-.9,2.05, '{', cex=2.2,family = 'Helvetica Neue UltraLight')
text(-1.1,2.05, "Altered \n Model")
text(2.1,1.05, "Authors'\n Model")
text(1.95, 1.05, "}", cex=2,2.2,family = 'Helvetica Neue UltraLight')
lines(c(0,0), c(-1,2.5), lty=2)
arrows(0,-.18,2,-.18, code=2, length=.15, col="forestgreen", lwd=3, xpd=T)
mtext("Increases FDI", side=1, at=1, padj=8)
arrows(0,-.18,-1,-.18, code=2, length=.15, col="royalblue4", lwd=3, xpd=T)
mtext("Decreases FDI", at=-.5, padj=8, side=1)


