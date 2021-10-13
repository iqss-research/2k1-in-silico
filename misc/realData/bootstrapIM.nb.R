dnb <- function(y, lambda, alpha){
    dnbinom(y, size=alpha, mu=lambda, log=T)
  }

pnb <- function(cut, lambda, alpha){
  sum <-0
  pnbinom(cut, size=alpha, mu=lambda, lower.tail=F, log.p=T)                                       
}

ll.ztnb.bs.IM <- function(theta, y, X, alpha, cut){
  lambda <- exp(drop(X %*% theta))
  zeros <- log(1-pnb(cut, lambda, alpha))
  -(dnb(y, lambda, alpha) - zeros)
}

ztnb <- function(par, y, X, cut){
  end <- (length(par))-1
  theta <- par[1:end]
  alpha <- exp(par[length(par)])
  lambda <- exp(drop(X %*% theta))
  zeros <- pnb(cut, lambda,alpha)
  ll <- sum(dnb(y, lambda, alpha) - zeros)
  return(ll)
}

bootstrapIM.ztnb <- function(formula, X, data, B,B2){
  y <- data[,c(paste(formula[[2]]))]
  out <- optim(c(rep(.01,ncol(X)),.01), ztnb, y=y, X=X,cut=4.1,control=list(fnscale=-1,maxit=10000), method="BFGS", hessian=T)
  ok <- !is.na(out$par)
  theta <- out$par[ok][-length(out$par[ok])]
  alpha <- exp(out$par[ok][length(out$par[ok])])
  lambda <- exp(drop(X%*%theta))
  grad <- apply(cbind(y,X),1,function(x) numericGradient(ll.ztnb.bs.IM, theta, y=x[1], X=x[2:length(x)], alpha=alpha, cut=4.1))
  meat <- grad%*%t(grad)
  bread <- out$hessian[ok,ok][1:(length(out$par[ok])-1),1:(length(out$par[ok])-1)]
  Dhat <- diag(nrow(X)^(-1/2)*(meat + bread))[1:7]  
  D <- list()
  
  Dbar <- rep(0, length(Dhat))
  for(i in 1:B){
    yB <- rnegbin(nrow(data), lambda, alpha)
    print(length(yB[yB<5]))
    while(length(yB[yB<5])>0){
      yB[yB<5] <- rnegbin(length(yB[yB<5]), lambda, alpha)
    }
    XB <- X[!is.na(yB),]
    yB <- yB[!is.na(yB)]
    outB <- optim(c(rep(.01,ncol(XB)),.01), ztnb, y=yB, X=XB,cut=4.1,control=list(fnscale=-1,maxit=10000), method="BFGS", hessian=T)
    ok <- !is.na(out$par)
    thetaB <- outB$par[ok][-length(outB$par[ok])]
    alphaB <- exp(outB$par[ok][length(outB$par[ok])])
    grad <- apply(cbind(yB,XB),1,function(x) numericGradient(ll.ztnb.bs.IM, thetaB, y=x[1], X=x[2:length(x)], alpha=alphaB, cut=4.1))
    meat <- grad%*%t(grad)
    bread <- outB$hessian[ok,ok][1:(length(outB$par[ok])-1),1:(length(outB$par[ok])-1)]
    D[[i]] <- diag(nrow(XB)^(-1/2)*(meat + bread))[1:7]
    
    Dbar <- D[[i]] + Dbar
    DBbar <- rep(0, length(Dhat))
    DB <- list()
    for(j in 1:B2){
      yB2 <- rnegbin(nrow(data), lambda, alpha)
      while(length(yB2[yB2<5])>0){
        yB2[yB2<5] <- rnegbin(length(yB2[yB2<5]), lambda, alpha)
      }

      XB2 <- X[!is.na(yB2),]
      yB2 <- yB2[!is.na(yB2)]
      outB2 <- optim(c(rep(.01,ncol(XB2)),.01), ztnb, y=yB2, X=XB2,cut=4.1,control=list(fnscale=-1,maxit=10000), method="BFGS", hessian=T)
      ok <- !is.na(outB2$par)
      
      thetaB2 <- outB2$par[ok][-length(outB2$par[ok])]
      alphaB2 <- exp(outB2$par[ok][length(outB2$par[ok])])
      grad <- apply(cbind(yB2,XB2),1,function(x) numericGradient(ll.ztnb.bs.IM, thetaB2, y=x[1], X=x[2:length(x)], alpha=alphaB2, cut=4.1))
    
      meat <- grad%*%t(grad)
      bread <- outB2$hessian[ok,ok][1:(length(outB2$par[ok])-1),1:(length(outB2$par[ok])-1)]
      DB[[j]] <- diag(nrow(XB2)^(-1/2)*(meat + bread))[1:7]

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
    if(i%%10==0) print(i)
  }
  
  Dbar <- Dbar/B

  Vb <- matrix(0, nrow=length(Dbar), ncol=length(Dbar))
  for(i in 1:B){
    Vb <- Vb + (D[[i]] - Dbar)%*%t(D[[i]]-Dbar)
  }
  
  Vb <- Vb/(B-1)
  
  #invVb <- invcov.shrink(Vb)
  invVb <- solve(Vb)

  omegaB <- t(Dhat)%*%invVb%*%Dhat
  print("omegaB")
  print(omegaB)
  pb = (B+1-sum(T< as.numeric(omegaB)))/(B+1)

  return(list(stat=omegaB, pval=pb))
}
  
