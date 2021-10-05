library(foreign)
library(MASS)
library(lmtest)
library(stringr)
library(sandwich)
library(maxLik)
library(corpcor)
library(VGAM)

#Read in replication dataset
dat <- read.dta("realData/DreherandJensenJLEreplication.dta")

#Na omit the data
dat <- na.omit(dat[,c("sum","un_per_l", "election_av", "un_el_av", "gdp_r_wk","gg_oecd_l","libor_l", "mg_l","imf_c_liab_l",  "country","year")])

setwd("../Code/ReplicationFolder/Code")
source("bootstrapIM.poisson.R")
#Replicate the model
lm7 <- glm(sum ~ un_per_l*election_av  + gdp_r_wk +gg_oecd_l + libor_l + mg_l + factor(country) + factor(year), data=dat, family="poisson")
summary(lm7)
form <- sum ~ un_per_l*election_av  + gdp_r_wk +gg_oecd_l + libor_l + mg_l + factor(country) + factor(year)

set.seed(02138)
IMtest <- bootstrapIM.poisson(form, dat, 99,50)
IMtest$pval
#pval .019

#Pure negative binomial of the model (We don't use this in the paper, this is just for fun)
lm7.2 <- glm.nb(sum ~ un_per_l*election_av  + gdp_r_wk +gg_oecd_l + libor_l + mg_l + factor(country) + factor(year), data=dat)
summary(lm7.2)

#Code up zero truncated negative binomial from scratch

#NB functions 
dnb <- function(y, lambda, alpha){  
  dnbinom(y, size=alpha, mu=lambda, log=T)
}

pnb <- function(cut, lambda, alpha){
  sum <-0
  pnbinom(cut, size=alpha, mu=lambda, lower.tail=F, log.p=T)
}

#Zero-truncated NB likelihood function, where cut is truncation point
ztnb <- function(par, y, X, cut){
	end <- (length(par))-1
	theta <- par[1:end]
	alpha <- exp(par[length(par)])
	lambda <- exp(drop(X %*% theta))
        zeros <- pnb(cut, lambda,alpha)
	ll <- sum(dnb(y, lambda, alpha) - zeros)
	return(ll)
}

#X matrix
X <- cbind(model.matrix(lm7)[,!is.na(as.vector(lm7$coefficients))])
y <- dat$sum

#Optimize
out <- optim(c(rep(.01,ncol(X)),.01), ztnb, y=y, X=X,cut=4, control=list(fnscale=-1,maxit=10000), method="BFGS", hessian=T)

library(ucminf)
source("bootstrapIM.nb.R")
IMtestztnb <- bootstrapIM.ztnb(form, X, dat, 99,50)
IMtestztnb$pval
#pval .78

vcov <- solve(-out$hessian)

#Calculate the numeric gradient
out2 <- apply(cbind(y,X),1,function(x) numericGradient(ztnb, out$par, y=x[1], X=x[2:(length(x))], cut=4))

#Calculate the sandwich estimator
meat <- out2%*%t(out2)
bread <- vcov%*%meat%*%vcov

#Compare two SEs
sqrt(diag(bread))
sqrt(diag(vcov))

#Make some pretty tables to show this
library(xtable)
table1 <- cbind(lm7$coefficients, as.matrix(summary(lm7)$coefficients)[,2], as.matrix(coeftest(lm7, sandwich))[,2])
colnames(table1) <- c("Coefficient", "Regular", "Robust")
table1 <- table1[c(1:7, length(lm7$coefficients)),]
xtable(table1)

table <- cbind(out$par, sqrt(diag(vcov)), sqrt(diag(bread)))
colnames(table) <- c("Coefficient", "Regular", "Robust")
table <- table[c(1:7, 56),]
rownames(table) <- rownames(table1)
xtable(table)

#Do some simulations to create a plot.  The question is -- what is the probability that the exepected
#decrease in conditions due to an election is greater than three?
set.seed(01238)
it <- seq(.32,.6,by=.01)
save <- matrix(nrow=1000, ncol=length(it))
ploty <- matrix(nrow=100,ncol=length(it))
save2 <- matrix(nrow=10000, ncol=length(it))
ploty2 <- matrix(nrow=100,ncol=length(it))
#For 100 sims, over each percent vote with the U.S. (it)
for(j in 1:100){
  for(i in 1:length(it)){
    #Draw beta 
    betas <- mvrnorm(1000, lm7$coefficients[c(1:55,58)], vcov(lm7))
    covs <- apply(model.matrix(lm7)[,c(1:55,58)],2,median)

    #Assign a country and year
    covs[10] <- 1
    covs[50] <- 1
    
    #Assign percent vote with US and interaction
    covs[2] <- it[i]
    covs[3] <- 1
    covs[length(covs)] <- covs[2]

    #Get lambdas and draw from Poisson
    lambda1 <- exp(covs%*%t(betas))
    params = drop(lambda1)
    y.p <- apply(as.matrix(params),1,function (x) rpois(1000, x))
    covs[3] <- 0
    covs[length(covs)] <-0
    lambda1 <- exp(covs%*%t(betas))
    params = drop(lambda1)
    y.p2 <- apply(as.matrix(params),1,function (x) rpois(1000, x))
    save[,i] <- apply(y.p - y.p2, 1 ,function(x) mean(x))

    ##Plot diagnostic
    betas <- mvrnorm(1000, out$par, vcov)
    covs <- apply(X,2,median)
    covs[2] <- it[i]
    covs[3] <- 1
    covs[10] <- 1
    covs[50] <- 1
    covs[length(covs)] <- covs[2]

    #Find lambdas and simulate from negative binomial
    lambda1 <- exp(covs%*%t(betas[,1:(length(out$par)-1)]))
    params = cbind(drop(lambda1), betas[,ncol(betas)])
    y <- apply(as.matrix(params),1,function (x) rnegbin(1000, x[1], theta=exp(x[2])))
    covs[3] <- 0
    covs[length(covs)] <-0
    lambda1 <- exp(covs%*%t(betas[,1:(length(out$par)-1)]))
    params = cbind(drop(lambda1), betas[,ncol(betas)])
    y2 <- apply(as.matrix(params),1,function (x) rnegbin(1000, x[1], theta=exp(x[2])))
    
    save2[,i] <- apply(y-y2,1,function (x) mean(x))
    if(i%%10==0) print(i)
  }
  print(j)
  ploty[j,] <- apply(save,2,function (x) sum(x< -3)/nrow(save))
  ploty2[j,] <- apply(save2,2,function (x) sum(x< -3)/nrow(save2))
}

it <- seq(.32,.6,by=.01)
plot(c(0,0),ylim=c(0,.23), xlim=c(.38,.6), xlab="Percent Vote with U.S. in U.N.", ylab="Probability That Decrease in Conditions is > 3")
y.loess <- loess( apply(ploty2[11:73,],2,function (x) mean(x)) ~ it, span=1.25)
y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
y.loess <- y.loess[order(y.loess$it),]
lines(y.loess$it, y.loess$V2, col="red", lwd=2)
y.loess <- loess( apply(ploty2[11:73,],2,function (x) quantile(x,.95)) ~ it, span=1.25)
y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
y.loess <- y.loess[order(y.loess$it),]
lines(y.loess$it, y.loess$V2, col="red", lwd=2, lty=2)
y.loess <- loess( apply(ploty2[11:73,],2,function (x) quantile(x,.05)) ~ it, span=1.25)
y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
y.loess <- y.loess[order(y.loess$it),]
lines(y.loess$it, y.loess$V2, col="red", lwd=2, lty=2)

y.loess <- loess(apply(ploty[11:73,],2,function (x) mean(x)) ~ it)
y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
y.loess <- y.loess[order(y.loess$it),]
lines(y.loess$it, y.loess$V2, col="blue", lwd=2)
y.loess <- loess( apply(ploty[11:73,],2,function (x) quantile(x,.05)) ~ it, span=1.25)
y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
y.loess <- y.loess[order(y.loess$it),]
lines(y.loess$it, y.loess$V2, col="blue", lwd=2, lty=2)
y.loess <- loess( apply(ploty[11:73,],2,function (x) quantile(x,.95)) ~ it, span=1.25)
y.loess <- data.frame(cbind(y.loess$x, y.loess$fitted))
y.loess <- y.loess[order(y.loess$it),]
lines(y.loess$it, y.loess$V2, col="blue", lwd=2, lty=2)


text(.4,.01, "Author's Model", col="blue")
text(.54,.19, "Altered Model", col="red")

col <- rainbow(ncol(save2))
plot(c(-10,10), ylim=c(0,1), xlim=c(-10,0))
for(j in 1:ncol(save2)){
yout <- NULL
it <- 1
for(i in -10:0){
	yout[it] <- sum(save2[,j]< i)/nrow(save2)
	it <- it + 1
}
lines(-10:0, yout, col=col[j])
text(-10, j/10, paste(j), col=col[j])
}

plot(density(apply(y.p-y.p2,2,mean), bw=1), xlim=c(-20,7), main="", xlab="Expected Conditions With Election - Conditions Without Election", col="forestgreen", lwd=2)
lines(density(apply(y-y2,2,mean), bw=1), col="red", lwd=2)
lines(c(mean(apply(y-y2,2,mean)), mean(apply(y-y2,1,mean))), c(0, .3), col="red", lty=2)
lines(c(mean(apply(y.p-y.p2,2,mean)), mean(apply(y.p-y.p2,1,mean))), c(0, .4), col="forestgreen", lty=2)
text(4,.25, "Author's Model", col="forestgreen")
text(-9,.1, "Altered Model", col="red")
