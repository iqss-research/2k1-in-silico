ll.poisson.bsIM <- function(par,y,X){
          beta <- par[1:length(X)]
          -y*log(exp(X%*%beta)) + exp(X%*%beta)
        }

bootstrapIM.poisson <- function(formula, data, B, B2){
  lm1 <- glm(formula, data=data, family="poisson")
  X <- model.matrix(lm1)
  y <- data[,c(paste(formula[[2]]))]
  ok <- !is.na(lm1$coefficients)
  X <- X[,ok]
  beta <- lm1$coefficients[ok]
  mu <- exp(X%*%beta)
  grad <- apply(cbind(y,X),1,function(x) numericGradient(ll.poisson.bsIM, beta, y=x[1], X=x[2:length(x)]))
  meat <- grad%*%t(grad)
  bread <- -solve(vcov(lm1))
  Dhat <- diag(nrow(X)^(-1/2)*(meat + bread))[1:7]
  D <- list()
  Dbar <- rep(0, length(Dhat))
  for(i in 1:B){
    yB <- rpois(nrow(data), lambda=mu)
    lm1B <- glm(yB ~ model.matrix(lm1)-1, family="poisson")
    ok <- !is.na(lm1B$coefficients)
    X <- model.matrix(lm1B)[,ok]
    grad <- apply(cbind(yB,X),1,function(x) numericGradient(ll.poisson.bsIM, lm1B$coefficients[ok], y=x[1], X=x[2:length(x)]))
    meat <- grad%*%t(grad)
    bread <- -solve(vcov(lm1B))
    D[[i]] <- diag(nrow(X)^(-1/2)*(meat + bread))[1:7]
    
    #Test to make sure robust matrix is being correctly calculated
    #print(paste("First", diag(solve(bread)%*%meat%*%solve(bread))[1:7]))
    #cov.m1 <- vcovHC(lm1B, type="HC0")
    #print(paste("Second", diag(cov.m1)[1:7]))
                  
    Dbar <- D[[i]] + Dbar
    DBbar <- rep(0, length(Dhat))
    DB <- list()
    
    #Bootstrap for VB of B
    for(j in 1:B2){
      yB2 <- rpois(nrow(data), mu)
      lm1B2 <- glm(yB2 ~ model.matrix(lm1)-1, family="poisson")
      ok <- !is.na(lm1B2$coefficients)
      XB2 <- model.matrix(lm1B2)[,ok]
      grad <- apply(cbind(yB2,XB2),1,function(x)
                    numericGradient(ll.poisson.bsIM, lm1B2$coefficients[ok], y=x[1], X=x[2:length(x)]))
      meat <- grad%*%t(grad)
      bread <- -solve(vcov(lm1B2))
      DB[[j]] <- diag(nrow(X)^(-1/2)*(meat + bread))[1:7]
      DBbar <- DB[[j]] + DBbar
    }          
    DBbar <- DBbar/B2
    VBb <- matrix(0, nrow=length(DBbar), ncol=length(DBbar))
    for(j in 1:B2){
      VBb <- VBb + (DB[[j]] - DBbar)%*%t(DB[[j]]-DBbar)
    }
    VBb <- VBb/(B2-1)
    #invVBb <- invcov.shrink(VBb)
    invVBb <- solve(VBb)
    T[i] <- t(D[[i]])%*%invVBb%*%D[[i]]
    print(i)
    print(T[i])
    if(i%%100==0) print(i)
  }
  
  Dbar <- Dbar/B

  Vb <- matrix(0, nrow=length(Dbar), ncol=length(Dbar))
  for(i in 1:B){
    Vb <- Vb + (D[[i]] - Dbar)%*%t(D[[i]]-Dbar)
  }
  
  Vb <- Vb/(B-1)
  
  #invVb <- invcov.shrink(Vb)
  invVb <- solve(Vb)
  
  print(summary(T))

  omegaB <- t(Dhat)%*%invVb%*%Dhat
  print("omegaB")
  print(omegaB)
  pb = (B+1-sum(T< as.numeric(omegaB)))/(B+1)
  
  return(list(stat=omegaB, pval=pb))
}
  
