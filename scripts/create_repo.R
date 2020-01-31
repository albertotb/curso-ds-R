library("miniCRAN")

revolution <- c(CRAN = "http://cran.microsoft.com")
pkgs <- c("glmnet","MASS","nnet","mgcv","gam","ridge","tidyverse","randomForest",
          "glmnetUtils","mlr","mlbench","rBayesianOptimization","ModelMetrics",
          "rpart", "xgboost","e1071","gbm","kernlab","caret")

pkgList <- pkgDep(pkgs, repos = revolution, type = "source", suggests = FALSE)

dir.create(pth <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "miniCRAN"))
makeRepo(pkgList, path = pth, repos = revolution, type = c("source", "win.binary"))

#addPackage("caret", path=pth, repos=revolution, type=c("source", "win.binary"))

pkgAvail(repos = pth, type = "win.binary")[, c(1:3, 5)]
