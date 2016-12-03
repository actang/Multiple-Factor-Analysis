## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(mfa)

## ---- eval=FALSE---------------------------------------------------------
#  mfa_out <- function(data, sets, supplData, ncomps = NULL, center = TRUE, scale = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  ncomps_2 <- mfa(..., ncomps = 2)
#  ncomps_null <- mfa(..., ncomps = NULL)  # extract all components

## ---- eval = FALSE-------------------------------------------------------
#  not_centered <- mfa(..., center = FALSE)
#  not_normalized <- mfa(..., scale = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  print(mfa_out)

## ---- eval = FALSE-------------------------------------------------------
#  Eigenvalues(mfa_out)

## ---- eval=FALSE---------------------------------------------------------
#  CtrObserToDimension(mfa_out)
#  CtrVarToDimension(mfa_out)
#  CtrTableToDimension(mfa_out)

## ---- eval = FALSE-------------------------------------------------------
#  # RV
#  rv_coef <- RV(table1, table2)
#  rv_table <- Rv_table(dataset, sets = list(1:3, 4:5, 6:10))
#  
#  # Lg
#  lg_coef <- Lg(table1, table2)
#  lg_table <- Lg_table(dataset, sets = list(1:3, 4:5, 6:10))

## ---- eval = FALSE-------------------------------------------------------
#  boot_ratio <- mfa_out$BootstrapRatio

## ---- eval = FALSE, fig.show='hold'--------------------------------------
#  plot_eigenvalues(mfa_out)
#  plot_factor_scores(mfa_out)
#  plot_partial_factor_scores(mfa_out)
#  plot_variable_loadings(mfa_out)
#  plot_boot_ratio(mfa_out, 2) # number of components = 2

