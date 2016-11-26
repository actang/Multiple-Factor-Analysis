#install.packages("MFAg")
library("MFAg")
url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
data <- read.csv(url)

mfa <- function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE){

  ## Check that all the arguments are correct
  checkScaleOrCenter(scale, ncol(data))
  checkScaleOrCenter(center,ncol(data))
  
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

  Eigen <- (gSVD$d)^2
  

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

  # Calculating the contributions
  #TODO: populate the variables
  conts <- matrix(nrow = length(sets),ncol = ncomps)
  cont <- a * Q.n^2
  for (i in 1:length(sets)){
    for (j in 1:ncol(Q.n)){
    conts[i,j] <- sum(cont[sets[[i]]-1,j])
    }
  }

  # Calculating the partial inertia
  partial_inertia <- matrix(nrow = length(sets),ncol = ncomps)
  for(i in 1:ncomps){
    partial_inertia[,i] <- conts[,i]*Eigen[i]
  }


  # return
  res <- list("EigenValues" = Eigen,
              "CompromiseFactorScores" = Fscores,
              "PartialFactorScores" = Partial_fs,
              "MatrixLoadings" = Q.n,
              #next 3 arguments needs to be fixed
              "CtrObserToDimension" = "x",
              "CtrVarToDimension" = "y",
              "CtrTableToDimension" = "z")
  
  class(res) <- "mfa"
  return(res)
}

mfa_out <- mfa(data = data, sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54),
               ncomps = 2, center = TRUE, scale = TRUE)

