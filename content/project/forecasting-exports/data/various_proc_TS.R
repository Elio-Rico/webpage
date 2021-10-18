# =============================================================
# MacroEconometrics course
# Various procedures
# Jean-Paul Renne, 2016
# =============================================================

autocov <- function(X,n){
  T <- length(X)
  X.1 <- X[1:(T-n)] - mean(X)
  X.2 <- X[(n+1):T] - mean(X)
  return(1/T * sum(X.1 * X.2))
}

make.F <- function(phi){
  # Make F matrix for an AR process
  p <- length(phi)
  F <- matrix(0,p,p)
  F[1,] <- phi
  if(p>1){
    F[2:p,1:(p-1)] <- diag(p-1)
  }
  return(F)
}

make.dyn.mult <- function(phi,max.dyn.mult){
  # Compute dynamic mutlipliers for an AR process
  vec.dyn.mult <- NULL
  F <- make.F(phi)
  p <- length(phi)
  F.j <- diag(p)
  vec.j <- 0:max.dyn.mult
  for(j in vec.j){
    vec.dyn.mult <- c(vec.dyn.mult,F.j[1,1])
    F.j <- F.j %*% F
  }
  return(vec.dyn.mult)
}


sim.arma <- function(c,phi,theta,sigma,T,y.0,nb.sim=1,make.IRF=0){
  # nb.sim samples of length T are simulated, all
  # of them begin with y_0 = y.0, y.0 is of dimension p x 1
  # sigma is a standard deviation
  # Warning:
  #     ->>>> for an AR process, set theta=1.
  #     ->>>> that is, to simulate an ARMA(p,q), theta has to be of length (q+1).
  # By default, only one simulation is done (nb.sim=1).
  #
  # If make.IRF=0,
  #     then it means that the user wants to compute IRFs with a maximum horizon T.
  p <- length(phi)
  q <- length(theta)
  if(length(y.0)!=p){
    print("y.0 should have the same length as phi.")
    return(NULL)
  }
  if(make.IRF==0){# In that case, the user wants to perform standard simulations
    eps <- sigma*matrix(rnorm(nb.sim*T),T,nb.sim) # These are all the shocks that will be used
    eps.t <- matrix(0,q,nb.sim) # eps.t is of dimension (q x nb.sim)
    # eps.t will change at each iteration, the i^th column corresponds to simulation i.
    # At date t, the i^th column of eps.t contains, for simulation i: (epsilon[t],epsilon[t-1],...,epsilon[t-q+1])
  }else{# In that case, the user wants to compute IRFs
    eps <- matrix(0,T,nb.sim)
    eps[1,] <- 1 # This is the initial impulsion
    eps.t <- matrix(0,q,nb.sim) # eps.t is of dimension (q x nb.sim)
  }
  F <- make.F(phi) # This is the usual F matrix
  Y <- NULL
  y.00 <- matrix(y.0,p,nb.sim)
  y.t <- y.00
  for(t in 1:T){
    if(q>1){
      eps.t <- rbind(eps[t,],matrix(eps.t[1:(q-1),],q-1,nb.sim))
    }else{
      eps.t <- matrix(eps[t,],nrow=1)
    }
    theta.eps.t <- matrix(theta,nrow=1) %*% eps.t
    theta.eps.t <- rbind(theta.eps.t,matrix(0,p-1,nb.sim))
    y.t <- c + F %*% y.t + theta.eps.t
    Y <- rbind(Y,y.t[1,])
  }
  return(Y)
}

make.PHI <- function(Phi){
  p <- length(Phi)
  n <- dim(Phi[[1]])[1]
  PHI <- matrix(0,n*p,n*p)
  if(p>1){
    PHI[(n+1):(n*p),1:((p-1)*n)] <- diag((p-1)*n)
  }
  for(i in 1:p){
    PHI[1:n,((i-1)*n+1):(i*n)] <- Phi[[i]]
  }
  return(PHI)
}

simul.VAR <- function(c,Phi,B,nb.sim,y0.star,indic.IRF=0,u.shock=0){
  # This function simulates a VAR model, initial condition = y0.star
  # Phi is a list, each element of which is a Phi_i matrix. Hence it has p elements if we consider a VAR(p)
  p <- length(Phi)
  n <- dim(Phi[[1]])[1]
  # Check that right dimension for y0.star:
  if((indic.IRF==0)&(length(y0.star)!=(n*p))){
    print("The dimension of y0.star should be (np x 1) where p is the number of lags in the VAR and n is the dimension of y")
    return(0)
  }
  if((indic.IRF!=0)&(length(u.shock)!=n)){
    print("If you want to compute IRFs, u.shock has to be of length n, where n is the number of dependent variables")
  }
  PHI <- make.PHI(Phi)
  c.star <- c(c,rep(0*c,p-1))
  B.star <- matrix(0,n*p,n*p)
  B.star[1:n,1:n] <- B
  y <- y0.star
  Y <- NULL
  for(t in 1:nb.sim){
    if(indic.IRF==0){
      y <- c.star + PHI %*% y + B.star %*% rnorm(n*p)
    }else{
      if(t==1){
        y <- B.star %*% c(u.shock,rep(0*u.shock,p-1))
      }else{
        y <- PHI %*% y
      }
    }
    Y <- rbind(Y,c(y))
  }
  return(Y[,1:n])
}





