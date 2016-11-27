##########################################################
# Helper function for printing and plotting the mfa object
#########################################################

print.mfa <- function(p, ...)
{
  cat('Object of type mfa', "\n")
  cat('add some more information here')
  invisible(p)
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
  ylim <- c(0, 1.1 * max(obj$Eigenvalues))
  plot <- barplot(obj$Eigenvalues, main = "Eigenvalues", ylim = ylim)
  text(x = plot, y = obj$Eigenvalues,
       label = round(obj$Eigenvalues, digit=2), col = "red", cex = 0.8)
}

plot_factor_scores <- function(obj){
  ncomps = ncol(obj$"Compromise factor scores")
  if (ncomps >= 2) {
    F_score = obj$"Compromise factor scores"
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
  partial_scores = obj$"Partial factor scores by assessor"
  factor_scores = obj$"Compromise factor scores"
  if (accessor_number != 0) {
    i = accessor_number
    for (j in 1:nrow(factor_scores)) {
      points(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
             pch = 24, cex = 0.2)
      segments(partial_scores[[i]][j, 1], partial_scores[[i]][j, 2],
               factor_scores[j, 1], factor_scores[j, 2],
               lty = 3, lwd = 0.8, col = 'blue')
    }
    title(sub=paste("with Partial Factor Scores of accessor ", accessor_number))
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
    title(sub=paste("with Partial Factor Scores of item ", wine_number))
  }
  if (wine_number == 0 && accessor_number == 0) {
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
  partial_scores = obj$"Partial factor scores by assessor"
  loadings = obj$"Matrix of loadings"
  if (accessor_number == 0) {
    accessor_number = 1
  }
  ncomps = ncol(obj$"Compromise factor scores")
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
    points(loadings[sets[[accessor_number]]-1, ], pch = 24)
    text(loadings[sets[[accessor_number]]-1, ], as.vector((obj$"Column Names")[sets[[accessor_number]]-1]),
         pos=3, col = "blue", cex=0.5)
    title(sub=paste("with Partial Factor Scores of accessor ", accessor_number))
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
    if(!is.list(sets))
    {
      stop("Sets input must be a list")
    }
    else
    {
      return(TRUE)
    }
}

