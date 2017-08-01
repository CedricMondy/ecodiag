#' @importFrom dplyr "%>%"
calibrate_DT <- function(trainingData, calibrationData,
                         selMetrics, params, p, nCores) {

  paramCombinations <-  expand.grid(params) %>%
    data.frame()

  test_randomForest <- function(combination,
                                trainingData,
                                calibrationData,
                                selMetrics) {

    nToSample <-
      table(trainingData$pressure) %>%
      min()                        %>%
      '*'(combination$sampsize)    %>%
      round()                      %>%
      rep(dplyr::n_distinct(trainingData$pressure))

    rf <-
      try(randomForest::randomForest(x = trainingData[, selMetrics],
                                     y = trainingData$pressure,
                                     mtry     = combination$mtry,
                                     ntree    = 150,
                                     replace  = FALSE,
                                     nodesize = combination$nodesize,
                                     maxnodes = combination$maxnodes,
                                     strata   = trainingData$pressure,
                                     sampsize = nToSample),
          silent = TRUE)

    if ("try-error" %in% class(rf)) {
      perf <- NA
    } else {
      perf <- pROC::roc(calibrationData$pressure,
                        predict(rf,
                                newdata = calibrationData,
                                type    = "prob")[, "impaired"])$auc
    }

    return(perf)
  }

  if (min(table(trainingData$pressure)) >= 100) {
    cl <- parallel::makeCluster(nCores)

    parallel::clusterExport(cl,
                            c("test_randomForest", "trainingData",
                              "calibrationData", "selMetrics", "%>%"),
                            envir = environment())

    paramCombinations$perf <-
      pbapply::pbsapply(split(paramCombinations,
                              rownames(paramCombinations)),
                        FUN             = test_randomForest,
                        trainingData    = trainingData,
                        calibrationData = calibrationData,
                        selMetrics      = selMetrics,
                        cl              = cl)

    parallel::stopCluster(cl)

    return(paramCombinations)
  } else {
    warning("Not enough training data to build a model for ", p,
            "\n    not impaired: ",
            length(trainingData$pressure[trainingData$pressure == "low"]),
            " / impaired: ",
            length(trainingData$pressure[trainingData$pressure == "impaired"]))
    return(NULL)
  }
}
