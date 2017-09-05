#' @importFrom dplyr "%>%"
calibrate_DT <- function(trainingData, calibrationData,
                         selMetrics, params, p, nCores) {

  paramCombinations <-  expand.grid(params) %>%
    data.frame()

  test_randomForest <- function(combination,
                                trainingData,
                                calibrationData,
                                selMetrics) {

    rf <-
      try(ranger::ranger(data            = trainingData[, c("pressure", selMetrics)],
                         formula         = pressure ~ .,
                         replace         = FALSE,
                         mtry            = combination$mtry,
                         num.trees       = 250,
                         min.node.size   = combination$min.node.size,
                         sample.fraction = combination$sample.fraction,
                         write.forest    = TRUE,
                         probability     = TRUE,
                         case.weights    = (1 - table(trainingData$pressure) /
                                              nrow(trainingData))[trainingData$pressure]),
          silent = TRUE)

    if ("try-error" %in% class(rf)) {
      perf <- NA
    } else {
      perf <- pROC::roc(calibrationData$pressure,
                        predict(rf,
                                data = calibrationData)$predictions[, "impaired"])$auc
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
