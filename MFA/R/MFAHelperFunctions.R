url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
data <- read.csv(url)

mfa <- function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE){
  
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
  
  # return
  res <- list("Eigenvalues" = Eigen, 
              "Compromise factor scores" = Fscores, 
              "Partial factor scores by assessor" = Partial_fs, 
              "Matrix of loadings" = Q.n)
  return(res)
}

mfa(data = data, sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54),
    ncomps = 2, center = TRUE, scale = TRUE)


# Optional: GSVD of X not using GSVD function
# notation follows Abdi 2007: tildaU is P; tildaV is Q in our problem

tildaX <- sqrt(M) %*% X %*% sqrt(A)
gSVD <- svd(tildaX) # = GSVD(X, PLin = rep(1/12, 12), PCol = a)

P <- gSVD$u
Q <- gSVD$v 
Del <- diag(gSVD$d)

t(Q) %*% Q # I
t(P) %*% P # I
P %*% Del %*% t(Q) # Xtilda

tildaU <- sqrt(solve(M)) %*% P
tildaV <- sqrt(solve(A)) %*% Q
tildaDel <- Del

t(tildaU) %*% M %*% tildaU # I
t(tildaV) %*% A %*% tildaV # I
tildaU %*% tildaDel %*% t(tildaV) # X
