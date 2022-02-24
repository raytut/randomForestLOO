# randomForestLOO
R-Package with additions to the randomforest package in R that enables different kinds of  Leave-One-Out analyses and the estimation of the true error.Contains three main functions for the estimation of leave-one-out random forest classifications, their true error via parametric bootstrapping, and significance testing of classificaiton models.


Currently only implemented for classification and not regression, as random forests are not ideal for extending regression models beyond the training/testing sets to new values. Package is given as is with no guarantees. 

# Installation
```
install.packages("remotes")
remotes::install_github("raytut/randomForestLOO")
```
