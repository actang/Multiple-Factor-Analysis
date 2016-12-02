## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(mfa)

## ------------------------------------------------------------------------
mymfa <- function(data, sets, supplData, ncomps = NULL, center = TRUE, scale = TRUE)
mymfa

## ---- eval = FALSE-------------------------------------------------------
#  ncomps_2 <- mfa(..., ncomps = 2)
#  ncomps_null <- mfa(..., ncomps = NULL)  # extract all components

## ---- eval = FALSE-------------------------------------------------------
#  not_centered <- mfa(..., center = FALSE)
#  not_normalized <- mfa(..., scale = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  Eigenvalues(mymfa)

## ---- eval=FALSE---------------------------------------------------------
#  CtrObserToDimension(mymfa)
#  CtrVarToDimension(mymfa)
#  CtrTableToDimension(mymfa)

## ---- eval = FALSE-------------------------------------------------------
#  # RV
#  rv_coef <- RV(table1, table2)
#  rv_table <- RV_table(dataset, sets = list(1:3, 4:5, 6:10))
#  
#  # Lg
#  lg_coef <- Lg(table1, table2)
#  lg_table <- Lg_table(dataset, sets = list(1:3, 4:5, 6:10))

## ---- eval = FALSE-------------------------------------------------------
#  boot_ratio <- boot_ratio(XX)

## ---- eval = FALSE, fig.show='hold'--------------------------------------
#  plot_eigenvalues(mymfa)
#  plot_factor_scores(mymfa)
#  plot_partial_factor_scores(mymfa)
#  plot_variable_loadings(mymfa)
#  plot_boot_ratio(mymfa)

