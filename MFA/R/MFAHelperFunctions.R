source("mfa.R")
##########################################################
# Helper function for printing and plotting the mfa object
#########################################################

print.mfa <- function(p, ...)
{
  cat('Object of type mfa', "\n")
  cat('add some more information here')
  invisible(p)
}

print(mfa_out)

plot.mfa <- function(p) {
  
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
    stop("Eigenvalues expects the argument to be of class mfaClass")
  }
  return(p$CtrObserToDimension)
}

CtrVarToDimension <- function(p) {
  #make sure that the parameter p is of class type mfa
  if(class(p) != "mfa")
  {
    stop("Eigenvalues expects the argument to be of class mfaClass")
  }
  return(p$CtrVarToDimension)
}

CtrTableToDimension <- function(p) {
  #make sure that the parameter p is of class type mfa
  if(class(p) != "mfa")
  {
    stop("Eigenvalues expects the argument to be of class mfaClass")
  }
  return(p$CtrTableToDimension)
}
