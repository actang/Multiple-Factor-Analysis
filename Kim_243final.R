#install.packages("MFAg")
library("MFAg")
library("boot")
url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
data <- read.csv(url)

mfa <- function(data, sets, supplData, ncomps = NULL, center = TRUE, scale = TRUE){

  Y <- list()
  X <- list()
  G <- list()
  alpha1 <- NA
  a <- NA

  # Compute weight matrix A
  for(K in 1:length(sets)){
    tempK <- as.matrix(data[ ,sets[[K]]])
    Y[[K]] <- tempK
    X[[K]] <- scale(Y[[K]], center = center, scale = scale) / sqrt(length(sets) + 1)

    SVD <- svd(X[[K]])
    D.K <- diag(SVD$d)
    V.K <- SVD$v
    U.K <- SVD$u

    # factor scores
    G[[K]] <- U.K %*% D.K

    # weight matrix A
    alpha1[K] <- (SVD$d[1])^(-2)
    a <- na.omit(append(a, rep(alpha1[K], length(sets[[K]]))))
    A <- diag(a)
  }

  # mass matrix M
  M <-  diag(rep(1/nrow(data), nrow(data)))

  # GSVD of X
  X <- do.call("cbind", X)
  gSVD <- GSVD(X, PLin = rep(1/nrow(data), nrow(data)), PCol = a)
  P <- gSVD$u
  Q <- gSVD$v
  Del <- diag(gSVD$d)

  Singular <- gSVD$d
  Eigen <- (Singular)^2
  inertia <- Eigen/sum(Eigen) * 100 # percentage

  # pick n components (dimensions) <- need change -- not 1:n but 1: ---
  P.n <- P[,1:ncomps]
  Del.n <- Del[1:ncomps, 1:ncomps]
  Q.n <- Q[,1:ncomps]

  # Factor scores
  Fscores <- P.n %*% Del.n # each row = wine, col = component

  # Partial factor scores
  Partial_fs <- list()
  for(K in 1:length(sets)){
    Partial_fs[[K]] <- length(sets) * alpha1[K] * X[,sets[[K]]-1] %*% Q.n[sets[[K]]-1, ]
  }

  # Contribution of an observation to a dimension

  lamda <- matrix(0,nrow = nrow(Fscores), ncol = ncol(Fscores))
  for (i in 1:ncol(Fscores)){
    lamda[,i] <- (M %*% Fscores^2)[,i]
  }

  cntr_obs <- matrix(0,nrow = nrow(Fscores), ncol = ncol(Fscores))
  for (i in 1:ncol(Fscores)){
    cntr_obs[,i] <- lamda[,i]/sum(lamda[,i])
  }

  # Calculating the contributions of a variable to a dimension
  cntr_var <- a * Q.n^2

  # Calculating the contributions of a table to a dimension
  cntr_tbl <- matrix(nrow = length(sets),ncol = ncomps)
  for (i in 1:length(sets)){
    for (j in 1:ncol(Q.n)){
    cntr_tbl[i,j] <- sum(cntr_var[sets[[i]]-1,j])
    }
  }

  # Calculating the partial inertia
  partial_inertia <- matrix(nrow = length(sets),ncol = ncomps)
  for(i in 1:ncomps){
    partial_inertia[,i] <- conts[,i]*Eigen[i]
  }

  # Calculating the scaled matrix for supplementary data
  Y_suppl <- scale(data[,supplData], center = center, scale = scale) / sqrt(length(sets) + 1)
  SVD_suppl <- svd(Y_suppl)
  D_suppl.K <- diag(SVD_suppl$d)
  X_suppl <- Y_suppl/D_suppl.K[1,1]

  # Calculating supplementary loadings
  Q_suppl <- t(X_suppl) %*% M %*% P %*% solve(Del)
  Q_suppl_ncomp <- Q_suppl[,1:ncomps]

  # Computing supplementary factor scores

  F_suppl <- length(sets)*X_suppl %*% Q_suppl_ncomp

  # Computing the bootstrapped values

  nBootstrap <- 1000
  boot <- replicate(nBootstrap,sample(1:length(sets),size=length(sets), replace=T))

  mysum <- list()
  F_boot <- list()
  for(k in 1:nBootstrap){
    mysum[[k]] <- matrix(0,nrow = nrow(data),ncol = ncomps)
    for(i in boot[,k]){
      mysum[[k]] <- mysum[[k]] + Partial_fs[[i]]
    }
    F_boot[[k]] <- mysum[[k]] / length(sets)
  }

  # Calculating the mean value for the bootstrap samples
  sum_boot <- matrix(0,nrow = nrow(data),ncol = ncomps)
  for (k in 1:nBootstrap){
    sum_boot <- sum_boot + F_boot[[k]]
  }
  mean_boot <- sum_boot/nBootstrap

  sub_mean_boot <- list()
  for (k in 1:nBootstrap){
    sub_mean_boot[[k]] <- F_boot[[k]] - mean_boot
  }

  # Calculating the variance and standard deviation for the bootstrap samples
  var_boot <- matrix(0,nrow = nrow(data),ncol = ncomps)
  for(k in 1:nBootstrap){
    var_boot <- var_boot + sub_mean_boot[[k]]^2
  }
  var_boot <- var_boot/nBootstrap
  sd_boot <- var_boot^(0.5)

  # Calculating the boostrap ratios

  ratio_boot <- mean_boot/sd_boot

  # return
  res <- list("Eigenvalues" = Eigen,
              "Compromise factor scores" = Fscores,
              "Partial factor scores by assessor" = Partial_fs,
              "Matrix of loadings" = Q.n,
              "Compromise contributions" = cont)
  class(res) <- append(class(res), "mfaClass")
  return(res)
}

# This is the definition of the print function for the mfaClass class.
print.mfaClass <- function(mfa_out){
}

mfa_out <- mfa(data = data, sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54),
               supplData <- c(55:58), ncomps = 2, center = TRUE, scale = TRUE)


# Optional: GSVD of X not using GSVD function
# notation follows Abdi 2007: tildaU is P; tildaV is Q in our problem

#tildaX <- sqrt(M) %*% X %*% sqrt(A)
#gSVD <- svd(tildaX) # = GSVD(X, PLin = rep(1/12, 12), PCol = a)
#
#P <- gSVD$u
#Q <- gSVD$v
#Del <- diag(gSVD$d)
#
#t(Q) %*% Q # I
#t(P) %*% P # I
#P %*% Del %*% t(Q) # Xtilda
#
#tildaU <- sqrt(solve(M)) %*% P
#tildaV <- sqrt(solve(A)) %*% Q
#tildaDel <- Del
#
#t(tildaU) %*% M %*% tildaU # I
#t(tildaV) %*% A %*% tildaV # I
#tildaU %*% tildaDel %*% t(tildaV) # X
