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
  #TODO: Plot the plot functions implemented by Allen
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

