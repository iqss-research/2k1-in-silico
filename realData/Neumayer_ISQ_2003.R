remove(list=ls())
library(foreign)
library(mvtnorm)
library(sandwich)
library(lmtest)
library(MASS)
library(maxLik)
library(xtable)
setwd("../Data")
source("../Code/bootstrapIM.normal.R")

#Read in data
############################################
dat <- read.dta("Article for ISQ (aid).dta")
dat <- na.omit(dat[,c("multish", "lnpop", "lnpopsq", "lngdp", "lncolony", "lndist", "freedom", "militexp", "arms", "year83", "year86", "year89", "year92")])

#Replicate Neumayer
############################################
lm1 <- lm(multish ~ lnpop + lnpopsq + lngdp +  lncolony + lndist + freedom +militexp + arms + year83 + year86 + year89 + year92, data=dat)
summary(lm1)
n <- nrow(model.matrix(lm1))
k <- ncol(model.matrix(lm1))
coeftest(lm1, n/(n-k)*sandwich(lm1))

#Parametric bootstrap for first equation
############################################
B <- 1000
formula <- multish ~ lnpop + lnpopsq + lngdp +  lncolony + lndist + freedom +militexp + arms + year86 + year89 + year92 
data <- dat
set.seed(02138)
lm1IM <- bootstrapIM.normal(formula, dat, 99,50)
#lm1IM p-value .019

#Regular vs. Robust SE's
############################################
se.nr <- as.matrix(summary(lm1)$coefficients)[,2]
se.r <- as.matrix(coeftest(lm1, n/(n-k)*sandwich(lm1)))[,2]

#Our version
############################################
dat$transform <- (dat$multish^.18-1)/.18
#dat$transform <- log(dat$multish)
lm1.fix <- lm(transform ~ lnpop + lnpopsq + lngdp +  lncolony + lndist + freedom +militexp + arms + year83 + year86 + year89 + year92, data=dat)
summary(lm1.fix)

#Parametric bootstrap for our equation
############################################
dat2 <- dat[!is.nan(dat$transform),]
formula <- transform ~ lnpop + lnpopsq + lngdp +  lncolony + lndist + freedom +militexp + arms + year86 + year89 + year92
set.seed(02139)
lm2IM <- bootstrapIM.normal(formula, dat2, 99,50)
lm2IM$pval
#lm2IM p-value .51

###Tables to compare the two
############################################
out <- as.matrix(summary(lm1)$coefficients)[1:8,1:2]
colnames(out)[2] <- "Regular Std. Error"
out <- cbind(out,as.matrix(coeftest(lm1,sandwich)[1:8,2]))
colnames(out)[3] <- "Robust Std. Error"
xtable(out)

out <- as.matrix(summary(lm1.fix)$coefficients)[1:8,1:2]
colnames(out)[2] <- "Regular Std. Error"
out <- cbind(out,as.matrix(coeftest(lm1.fix,sandwich)[1:8,2]))
colnames(out)[3] <- "Robust Std. Error"
xtable(out)

se.nr.fix <- as.matrix(summary(lm1.fix)$coefficients)[,2]
se.r.fix <- as.matrix(coeftest(lm1.fix, n/(n-k)*sandwich(lm1.fix)))[,2]


#Coefficients plot we didn't end up using in the paper
############################################
plot(se.nr.fix-se.r.fix, seq(1,length(se.r.fix)), xlim=c(-2.2,2), col=rgb(0,0,1,alpha=.75), pch=16, xlab="Regular SE's - Robust SE's", ylab="", axes=F)
points(se.nr-se.r, seq(1,length(se.r)),col=rgb(1,0,0,alpha=.75), pch=16)
lines(c(0,0), c(0,length(se.r)+1), lty=2)
axis(1)
axis(2, at=1:length(se.r.fix), labels=names(lm1$coefficients), las=1, cex.axis=.8)
text(-1.8,1.5, "Author's Model", col="red")
text(.5,2, "Altered Model", col="blue")

#Fitted vs. residuals, original
####################################
plot(lm1$fitted,lm1$residuals, pch=16, xlab="Fitted Values", ylab="Residuals", main="Fitted vs Residuals, Author's Model", ylim=c(-5,11), cex=.75)
upper <- curve(1.5+.6*x + .2*x^2, -1,6, add=T, lty=2, col="red")
lower <- curve(-.4 -.9*x + .02*x^2, -1,6, add=T, lty=2, col="red")
polygon(c(upper$x, lower$x[length(lower$x):1]), c(upper$y, lower$y[length(lower$y):1]), col=rgb(1,0,0,alpha=.1), border=F)

#Fitted vs. residuals, altered model
####################################
plot(lm1.fix$fitted,lm1.fix$residuals, pch=16, xlab="Fitted Values", ylab="Residuals",main="Fitted vs Residuals, Altered Model")

#Figure 5 (c)
####################################
plot(lm1,2, pch=16, main="Q-Q Plot, Author's Model", caption="")

#Figure 5 (d)
###################################
plot(lm1.fix,2, pch=16,main="Q-Q Plot, Altered Model", caption="")

#Figure 5 (a), with a polygon added
####################################
plot(dat$lnpop,lm1$residuals, pch=16, xlab="Log Population", ylab="Residuals", main="Population vs Residuals, Author's Model", cex=.75, ylim=c(-5,11))
upper <- curve(-2.8+ -.14*x + .03*x^2, 11,22, add=T, lty=2, col="red")
lower <- curve(0 +.14*x - .015*x^2, 11,22, add=T, lty=2, col="red")
polygon(c(upper$x, lower$x[length(lower$x):1]), c(upper$y, lower$y[length(lower$y):1]), col=rgb(1,0,0,alpha=.1), border=F)

#Figure 5 (b)
####################################
plot(dat$lnpop[as.numeric(names(lm1.fix$residuals))],lm1.fix$residuals, pch=16, xlab="Log Population", ylab="Residuals",main="Population vs Residuals, Altered Model", cex=.75)
y.loess <- loess(lm1.fix$residuals ~ dat$lnpop[as.numeric(names(lm1.fix$residuals))])

#Histograms for Figure 4
####################################
freq <- hist(dat$multish, main="Multilataral Aid Flows", xlab="Multilateral Aid Flows", col="red", breaks=100, plot=F)
#freq$counts <- ifelse(freq$mids>3 & freq$counts!=0, freq$counts + 4, freq$counts) 
plot(freq,main="Original", xlab="Multilateral Aid Flows", col="red", breaks=100)
hist(dat$transform, main="Transformed", xlab="Multilateral Aid Flows", col="red", breaks=100)

#Figure 6
###############################
ll.normal <- function(par,y,X){
	beta <- par[1:ncol(X)]
	sigma2 <- exp(par[ncol(X)+1])
	-1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2)) 
	}

#Author's model, using lm1
it <- seq(11,16, by=.1)
save <- matrix(ncol=3, nrow=length(it))
for(i in 1:length(it)){
  betas <- mvrnorm(10000, lm1$coefficients, n/(n-k)*sandwich(lm1))
  covs <- apply(model.matrix(lm1),2,mean)
                                        #Regular
  covs[2] <- it[i]
  covs[3] <- covs[2]^2
  mu.at.mean <- covs%*%t(betas)
  save[i,] <- c(mean(mu.at.mean), quantile(mu.at.mean,.025), quantile(mu.at.mean,.975))
}
plot(it, save[,1])

#Simulate altered model for population 11-16
X <- model.matrix(lm1.fix)
y <- na.omit(dat$transform)
opt1 <- optim(par = rep(0, ncol(X) + 1), fn = ll.normal, y = y, 
                 X = X, control = list(fnscale = -1), method = "BFGS", hessian = TRUE)

it <- seq(11,16, by=.1)
save2 <- matrix(ncol=3, nrow=length(it))
for(i in 1:length(it)){
  betas <- mvrnorm(10000, opt1$par, solve(-opt1$hessian))
  covs <- apply(model.matrix(lm1.fix),2,mean)
#Regular
  covs[2] <- it[i]
  covs[3] <- covs[2]^2
  mu.at.mean <- covs%*%t(betas[,1:(ncol(betas)-1)])
  params <- cbind(drop(mu.at.mean), exp(betas[,ncol(betas)]))
  #untransformed y
  y <- apply(params,1, function (x) rnorm(1000, x[1], x[2]))
  #transformed y
  y <- (y*.18 +1)^(1/.18)
  print(paste(i, "out of", length(it)))
#save2[i,] <- c(mean(as.vector(y)), quantile(as.vector(y), .025), quantile(as.vector(y), .975)) 
  save2[i,] <- c(mean(apply(y,2,mean, na.rm=T)), quantile(apply(y,2,mean, na.rm=T),.025), quantile(apply(y,2,mean, na.rm=T),.975))
}

#Plot everything
it <- seq(11,16, by=.1)
plot(it, save[,1], type="l", col="blue", lwd=2, xlab="Log Population", ylab="Expected Aid as a Percentage of GDP", xaxs="i", yaxs="i", ylim=c(0,2), xlim=c(11,15))
lines(it, save[,2], col="blue", lty=2)
lines(it, save[,3], col="blue", lty=2)
polygon(c(it,sort(it,decreasing=T)), c(save[,2], save[nrow(save):1,3]), col=rgb(0,0,1,alpha=.5), border=F)

it <- seq(11,16, by=.1)
lines(it, save2[,1], col="red", lwd=2)
polygon(c(it,sort(it,decreasing=T)), c(save2[,2], sort(save2[,3], decreasing=T)), col=rgb(1,0,0,alpha=.5), border=F)
lines(it, save2[,2], col="red", lty=2)
lines(it, save2[,3], col="red", lty=2)
text(12.1,1.5, "Author's Model", col="blue")
text(14.25,.5, "Altered Model", col="red")

#B-P test
##################################
ncvTest(lm1)
ncvTest(lm1.fix)

##Extra:  numerically finding robust matrix:
#####################################
ll.normalopt <- function(par,y,X){
	beta <- par[1:ncol(X)]
	sigma2 <- exp(par[ncol(X)+1])
	-1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2)) 
	}

ll.normal <- function(par,y,X, sigma){
	beta <- par[1:length(X)]
	sigma2 <- sigma
	-1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2)) 
	}

sigma <- sum(lm1$residuals^2)/(nrow(model.matrix(lm1))-2)
y <- dat$multish

out <- apply(cbind(y,model.matrix(lm1)),1,function(x) numericGradient(ll.normal, lm1$coefficients, y=x[1], X=x[2:length(x)], sigma=sigma))

meat <- out%*%t(out)
bread <- vcov(lm1)%*%meat%*%vcov(lm1)
