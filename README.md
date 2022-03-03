# randomForestLOO
R-Package with additions to the randomforest package that enables different kinds of Leave-One-Out analyses cross-validation and the estimation of the true error via bootstrap permutation testing. Contains three main functions for 1) the estimation of random forests classification with leave-one-out cross validation, 2) estimation of chance level errors via parametric bootstrapping, 3) and significance testing of classificaiton models.


Currently only implemented for classification and not regression, as random forests are not ideal for extending regression models beyond the training/testing sets to new values. Package is given as is with no guarantees. 

# Installation
```
install.packages("remotes")
remotes::install_github("raytut/randomForestLOO")
```
