#' @title RV
#' @description Computes the RV coefficients to study the Between-Table Structure
#' @param Xi represents the first table
#' @param Xj represents the second table
#' @return RV coefficient for Xi and Xj
#' @export
#' @examples
#' # default
#' rv_coef <- RV(table1, table2)
#'
RV <- function(Xi,Xj){
  numerator <- (tr((Xi%*%t(Xi) * Xj%*%t(Xj))))
  denominator <- (sqrt((tr((Xi%*%t(Xi) * Xi%*%t(Xi))))*(tr((Xj%*%t(Xj) * Xj%*%t(Xj))))))
  Rv <- numerator/denominator
  return(Rv)
}

#' @title Rv_table
#' @description Computes the RV coefficients to study the Between-Table Structure
#' @param data represents the data-set that contains all the tables
#' @param sets represents the list of vector used to identify the tables
#' @return RV coefficient for the data-set
#' @export
#' @examples
#' # default
#' rv_coef <- RV_table(dataset, sets = list(1:3, 4:5, 6:10))
#'
Rv_table <- function(data, sets){
  Rv <- matrix(0,nrow = length(sets),ncol = length(sets))
  tempK <- as.matrix(data[ ,sets[[K]]])
  X[[K]] <- tempK
  for(i in 1:length(sets)){
    for(j in 1:length(sets)){
      numerator <- (tr((X[[i]]%*%t(X[[i]]) * X[[j]]%*%t(X[[j]]))))
      denominator <- (sqrt((tr((X[[i]]%*%t(X[[i]]) * X[[i]]%*%t(X[[i]]))))*(tr((X[[j]]%*%t(X[[j]]) * X[[j]]%*%t(X[[j]]))))))
      Rv_table[i,j] <- numerator/denominator
    }
  }
  return(Rv_table)
}


# Computing Lg table coefficient
#' @title Lg
#' @description Computes the LG coefficients to study the Between-Table Structure
#' @param Xi represents the first table
#' @param Xj represents the second table
#' @return Lg coefficient for Xi and Xj
#' @export
#' @examples
#' # default
#' lg_coef <- Lg(table1, table2)
#'

Lg <- function(Xi,Xj){
  alpha1 <- NA
  SVDi <- svd(Xi)
  SVDj <- svd(Xj)
  alpha1_i <- (SVDi$d[1])^(-2)
  alpha1_j <- (SVDj$d[1])^(-2)
  Lg <- (tr((Xi%*%t(Xi) * Xj%*%t(Xj)))) * alpha1_i * alpha1_j
  return(Lg)
}


# Computing Lg table coefficient
#' @title Lg_table
#' @description Computes the LG coefficients to study the Between-Table Structure
#' @param data represents the data-set that contains all the tables
#' @param sets represents the list of vector used to identify the tables
#' @return LG coefficient for the data-set
#' @export
#' @examples
#' # default
#' lg_coef <- Lg(dataset, sets = list(1:3, 4:5, 6:10))
#'

Lg_table <- function(data, sets){
  X <- list()
  alpha1 <- NA
  tempK <- as.matrix(data[ ,sets[[K]]])
  X[[K]] <- tempK
  SVD <- svd(X[[K]])
  alpha1[K] <- (SVD$d[1])^(-2)
  Lg <- matrix(0,nrow = length(sets),ncol = length(sets))
  for(i in 1:length(sets)){
    for(j in 1:length(sets)){
      Lg[i,j] <- (tr((X[[i]]%*%t(X[[i]]) * X[[j]]%*%t(X[[j]])))) * alpha1[i] * alpha1[j]
    }
  }
  return(Lg)
}


##########################################################
# Helper function for printing and plotting the mfa object
#########################################################

print.mfa <- function(p)
{
  cat('Object of type mfa', "\n")
  print('The number of assessors is',length(p$Sets))
  print('The number of components is',length(p$Eigen))
}

plot.mfa <- function(p) {
  plot_eigenvalues(p)
  plot_factor_scores(p)
  plot_partial_factor_scores(p)
  for (i in 1:length(obj$"Partial factor scores by assessor")){
    plot_variable_loadings(p, i)
  }
}

Eigenvalues <- function(p) {
  #make sure that the parameter p is of class type mfa
  if(class(p) != "mfa")
  {
    stop("Eigenvalues expects the argument to be of class mfaClass")
  }
  inertia <- p$EigenValues/sum(p$EigenValues) * 100
  returnValue <- data.frame()
  ### TODO: The last row needs to be fixed and converted into an integer.
  returnValue = rbind(sqrt(p$EigenValues), p$EigenValues, cumsum(p$EigenValues), inertia, cumsum(inertia))
  rownames(returnValue) <- c("Singular value", "Eigenvalue", "cumulative", "Inertia", "cumulative")
  colnames(returnValue) <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11")
  return(returnValue)
}

CtrObserToDimension <- function(p) {
  #make sure that the parameter p is of class type mfa
  if(class(p) != "mfa")
  {
    stop("CtrObserToDimension expects the argument to be of class mfaClass")
  }
  return(p$CtrObserToDimension)
}

CtrVarToDimension <- function(p) {
  #make sure that the parameter p is of class type mfa
  if(class(p) != "mfa")
  {
    stop("CtrVarToDimension expects the argument to be of class mfaClass")
  }
  return(p$CtrVarToDimension)
}

CtrTableToDimension <- function(p) {
  #make sure that the parameter p is of class type mfa
  if(class(p) != "mfa")
  {
    stop("CtrTableToDimension expects the argument to be of class mfaClass")
  }
  return(p$CtrTableToDimension)
}


##################################################################
## Plot functions
##################################################################

plot_eigenvalues <- function(obj){
  ylim <- c(0, 1.1 * max(obj$EigenValues))
  plot <- barplot(obj$EigenValues, main = "Eigenvalues", ylim = ylim)
  text(x = plot, y = obj$EigenValues,
       label = round(obj$EigenValues, digit=2), col = "red", cex = 0.8)
}

plot_factor_scores <- function(obj){
  ncomps = ncol(obj$CompromiseFactorScores)
  if (ncomps >= 2) {
    F_score = obj$CompromiseFactorScores
    plot(F_score,
         xlab = "First Principal Component",
         ylab = "Second Principal Component",
         main = "Factor Scores",
         xlim=c(min(F_score[, 1]) - 0.1, max(F_score[, 1]) + 0.1),
         ylim=c(min(F_score[, 2]) - 0.1, max(F_score[, 2]) + 0.1)
    )
    abline(h = 0, v = 0, lty=2)
    text(F_score, as.vector(obj$"Labels"), pos=3, col = "red")
  }
}

plot_partial_factor_scores <- function(obj, accessor_number=0, wine_number=0){
  plot_factor_scores(obj)
  partial_scores = obj$PartialFactorScores
  factor_scores = obj$CompromiseFactorScores
  if (accessor_number != 0) {
    i = accessor_number
    for (j in 1:nrow(factor_scores)) {
      points(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
             pch = 24, cex = 0.2)
      segments(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
               factor_scores[j, 1], factor_scores[j, 2],
               lty = 3, lwd = 0.8, col = 'blue')
    }
  }
  if (wine_number != 0) {
    j = wine_number
    for (i in 1:length(partial_scores)) {
      points(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
             pch = 24, cex = 0.2)
      segments(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
               factor_scores[j, 1], factor_scores[j, 2],
               lty = 3, lwd = 0.8, col = 'blue')
    }
  }
  if (accessor_number != 0 && wine_number == 0){
    title(sub=paste("with Partial Factor Scores of accessor", accessor_number))
  }
  else if(accessor_number == 0 && wine_number != 0){
    title(sub=paste("with Partial Factor Scores of item", wine_number))
  }
  else if(accessor_number != 0 && wine_number != 0){
    title(sub=paste("with Partial Factor Scores of accessor", accessor_number, "and item", wine_number))
  }
  else{
    for (j in 1:nrow(factor_scores)) {
      for (i in 1:length(partial_scores)) {
        points(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
               pch = 24, cex = 0.2)
        segments(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
                 factor_scores[j, 1], factor_scores[j, 2],
                 lty = 3, lwd = 0.8, col = 'blue')
      }
    }
    title(sub="with Partial Factor Scores")
  }
}

plot_variable_loadings <- function(obj, accessor_number=0){
  partial_scores = obj$PartialFactorScores
  loadings = obj$MatrixLoadings
  if (accessor_number == 0) {
    accessor_number = 1
  }
  ncomps = ncol(obj$CompromiseFactorScores)
  if (ncomps >= 2) {
    plot(partial_scores[[accessor_number]],
         xlab = "First Principal Component",
         ylab = "Second Principal Component",
         main = "Partial Factor Scores with Variable Loadings",
         xlim=c(min(partial_scores[[accessor_number]][, 1]) - 0.1,
                max(partial_scores[[accessor_number]][, 1]) + 0.1),
         ylim=c(min(partial_scores[[accessor_number]][, 2]) - 0.1,
                max(partial_scores[[accessor_number]][, 2]) + 0.1)
    )
    abline(h = 0, v = 0, lty=2)
    text(partial_scores[[accessor_number]], as.vector(obj$"Labels"), pos=3, col = "red")
    points(loadings[obj$Sets[[accessor_number]]-1, ], pch = 24)
    text(loadings[obj$Sets[[accessor_number]]-1, ], as.vector((obj$"ColumnNames")[obj$Sets[[accessor_number]]-1]),
         pos=3, col = "blue", cex=0.5)
    title(sub=paste("with Partial Factor Scores of accessor ", accessor_number))
  }
}

plot_boot_ratio <- function(obj){
  for(i in 1:obj$ncomps){
    ylim <- c(-1.1 * max(obj$BootstrapRatio[,i]), 1.1 * max(obj$BootstrapRatio[,i]))
    plot <- barplot(obj$BootstrapRatio[,i], main = "Bootstrap Ratios", ylim = ylim, col = ifelse(abs(obj$BootstrapRatio[,i]) < 3, 'gray',ifelse(obj$BootstrapRatio[,i]< 0,'green','blue')))
    text(x = plot, y = obj$BootstrapRatio[,i],
       label = round(obj$BootstrapRatio[,i], digit=2), col = ifelse(abs(obj$BootstrapRatio[,i]) < 3, 'gray',ifelse(obj$BootstrapRatio[,i]< 0,'green','blue')), cex = 0.8, pos = ifelse(obj$BootstrapRatio[,i]< 0, 1, 3), offset = 0.2)
  }
}

##################################################################
## Helper functions to check that the arguments passed are correct
##################################################################

checkScaleOrCenter <- function(param, maxCount) {
  if(is.vector(param, mode = "numeric") || is.logical(param))
  {
    if(is.vector(param, mode = "numeric") && length(param) > maxCount)
    {
      stop("Size of the scales or center vector must be less than number of columns in data")
    }
    else
    {
      return(TRUE)
    }
  }
  else
  {
    stop("Scale or center parameter must either be a numeric vector or a boolean value (TRUE or FALSE)")
  }
}

checkNComponents <- function(ncomps) {
  if(ncomps%%1 == 0)
  {
    return(TRUE)
  }
  else
  {
    print(ncomps)
    stop("Number of Components parameter must be integer")
  }
}

checkData <- function(data)
{

    if(!(is.data.frame(data) || is.matrix(data)))
    {
      stop("Data parameter must be matrix or data frame")
    }
    else
    {
      return(TRUE)
    }

}

checkSets <- function(sets) {
    if(!is.list(obj$Sets))
    {
      stop("Sets input must be a list")
    }
    else
    {
      return(TRUE)
    }
}

