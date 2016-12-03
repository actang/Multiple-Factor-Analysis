Multiple Factor Analysis Using R
========================================================
author:Saurabh Belsare, Ankush Desai, Jieun Kim and Allen Tang
date: 2016-12-02
autosize: true

Introduction
========================================================

**MFA Package**
- The package `"mfa"` implements a statistical multivariate technique called Multiple Factor Analysis (MFA)

- In addition to the main `"mfa"` function, it provides a set of complementary functions and methods to compute summaries of eigenvalues, contributions, coefficients to study the between-table structure (both $R_V$ and $L_g$), and bootstrapping to estimate the stability of the compromise factor scores.



- For visualization, the package includes functions for five types of plots:
  + a bar-chart for the eigenvalues,
  + a scatterplot of the common factor scores,
  + a scatterplot of the partial factor scores by block and by observation,
  + a scatterplot of the loadings of given dimensions (i.e., components), and
  + a bar-chart for bootstrap ratios. A Shiny App is available for an interactive visualization.

Main Function: Creating a `mfa` object
========================================================
The first step is to create a `"mfa"` object with the function `mfa()`:


```r
mymfa <- function(data, sets, supplData, ncomps = NULL, center = TRUE, scale = TRUE)
```

This is the main mfa function that implements all the calculations for performing the Multiple Factor Analysis. The function takes six arguments:

- `data`: the input data, which contains only the assessor inputs,
- `sets`: the column number list demarcating columns for each assessor,
- `supplData`: the list of supplementary data for an additional factor analysis,
- `ncomps`: the number of components for the MFA,
- `center`: a boolean flag which decides whether to center the data,
- `scale`: a boolean flag which decides whether to scale the data.

Main Function: return value of mfa
========================================================
The function returns an object of type `mfa` that contains a list containing all the output information computed by the MFA analysis. It includes:

- **Main Output**: the eigenvalues, compromise factor scroes, matrix loadings, and contributions (by observation, variable, and table);
- **Additional information**: labels, column names of the data, sets, bootstrap ratios, and the number of components;
- **Supplementary Output**: eigenvalues, compromise factor scores, matrix loadings, and the number of components.


Complementary Functions
=======================================================
## Print basic information about mfa object
The package provides a print method to display basic information about mfa object: the numbers of blocks and components.

```r
source("MFAHelperFunctions.R")
source("mfa.R")
print(mfa_out)
```

```
Object of type mfa 
The number of assessors is 10 
The number of components is 11
```

Complementary Functions
=======================================================
## Summaries of eigenvalues
This package provides the summaries of eigenvalues that include each component's singular values, eigenvalues, cumulative eigenvalues, \% inertia, and cumulative \% inertia.

```r
Eigenvalues(mfa_out)
```

```
                       C1         C2          C3          C4          C5
Singular value  0.8776418  0.3506073  0.30118188  0.27570882  0.24413253
Eigenvalue      0.7702551  0.1229254  0.09071052  0.07601535  0.05960069
cumulative      0.7702551  0.8931806  0.98389110  1.05990645  1.11950714
Inertia        61.2296749  9.7716778  7.21082608  6.04266702  4.73782113
cumulative     61.2296749 71.0013527 78.21217882 84.25484584 88.99266698
                        C6          C7          C8          C9         C10
Singular value  0.19799790  0.17581135  0.15798257  0.13660609  0.11592003
Eigenvalue      0.03920317  0.03090963  0.02495849  0.01866122  0.01343745
cumulative      1.15871031  1.18961994  1.21457843  1.23323966  1.24667711
Inertia         3.11636657  2.45709049  1.98401852  1.48343143  1.06817972
cumulative     92.10903355 94.56612404 96.55014256 98.03357399 99.10175371
                        C11
Singular value   0.10630019
Eigenvalue       0.01129973
cumulative       1.25797684
Inertia          0.89824629
cumulative     100.00000000
```

Complementary Functions
=======================================================
## Contributions
This package provides three complementary functions to print (1) contribution of an observation to a dimension, (2) contribution of a variable to a dimension, and (3) contribution of a table to a dimension.

```r
# Note that only 2 roles are printed for the contributions below
head(CtrObserToDimension(mfa_out), 2)
```

```
           [,1]        [,2]
[1,] 0.10394860 0.018067938
[2,] 0.07078421 0.000721502
```

```r
head(CtrVarToDimension(mfa_out), 2)
```

```
           [,1]       [,2]
[1,] 0.02088509 0.02441275
[2,] 0.01712210 0.01484237
```

```r
head(CtrTableToDimension(mfa_out), 2)
```

```
          [,1]       [,2]
[1,] 0.1011327 0.09540216
[2,] 0.1000578 0.06849423
```

Complementary Functions
======================================================
## Between-table structure
This package provides functions for computing $R_V$ and $L_g$ coefficients to evaluate the similarity between two tables.
For $R_V$ coefficient, `RV()` takes two tables as the arguments and computes the cofficient for two tables.
Additionally, `Rv_table()` takes a data set and a list with sets of variables and computes a matrix of $R_V$ coefficients. For $L_g$ coefficient, this package provides `Lg()` and `Lg_table()`; their usage parallels that of $R_V$ coefficient.


```r
# RV
rv_coef <- RV(table1, table2)
rv_table <- Rv_table(dataset, sets = list(1:3, 4:5, 6:10))

# Lg
lg_coef <- Lg(table1, table2)
lg_table <- Lg_table(dataset, sets = list(1:3, 4:5, 6:10))
```

Complementary Functions
======================================================
## Bootstrap
This package allows users to perform bootstrapping in order to estimate the stability of the compromise factor scores.  While it does not provide a separate function for computing bootstrap ratios, these outputs are available from the main function `mfa()`. It generates 1,000 bootstrap samples that give 1,000 estimated bootstrapped factor scores. Like t-statistics, bootstrap ratios can be used to find the observations that reliably contribute to a given component.



```r
boot_ratio <- mfa_out$BootstrapRatio
boot_ratio
```

```
             [,1]       [,2]
 [1,] -25.5514734  1.2250542
 [2,] -10.1243783  0.3570051
 [3,]  -8.2369220 -5.2252653
 [4,] -16.2387589 -1.0899167
 [5,]  20.0580986 -1.0399369
 [6,]  15.6926495 -0.9261838
 [7,]   8.5033049  2.0533183
 [8,]  16.0250162  3.5714549
 [9,]  -6.7191098  2.4796290
[10,]   0.9944295 -5.3189617
[11,]  -6.0276074  6.6070450
[12,]   5.2655096 -0.6154428
```

Visualization
======================================================

This package provides five types of plots.
- a bar-chart for the eigenvalues,
- a scatterplot of the common factor scores,
- a scatterplot of the partial factor scores by block and by observation,
- a scatterplot of the loadings of given dimensions (i.e., components),
- a bar-chart for bootstrap ratios. You can use following functions to create graphics:



```r
# for plotting all the graphs
plot(mfa_out)
```

A bar-chart for the eigenvalues
=================================================

```r
plot_eigenvalues(mfa_out)
```

![plot of chunk unnamed-chunk-8](mfa-presentation-figure/unnamed-chunk-8-1.png)

A scatterplot of the common factor scores
================================================


```r
plot_factor_scores(mfa_out)
```

![plot of chunk unnamed-chunk-9](mfa-presentation-figure/unnamed-chunk-9-1.png)

A scatterplot of the partial factor scores by block and by observation
================================================

```r
plot_partial_factor_scores(mfa_out)
```

![plot of chunk unnamed-chunk-10](mfa-presentation-figure/unnamed-chunk-10-1.png)

A scatterplot of the loadings of given dimensions (i.e., components)
================================================

```r
plot_variable_loadings(mfa_out)
```

![plot of chunk unnamed-chunk-11](mfa-presentation-figure/unnamed-chunk-11-1.png)

A bar-chart for bootstrap ratios. You can use following functions to create graphics
================================================

```r
plot_boot_ratio(mfa_out, 2)
```

![plot of chunk unnamed-chunk-12](mfa-presentation-figure/unnamed-chunk-12-1.png)![plot of chunk unnamed-chunk-12](mfa-presentation-figure/unnamed-chunk-12-2.png)


========================================

**Thank You**
