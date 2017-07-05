#' @importFrom dplyr '%>%'
calibrate_DT <- function(trainingData, calibrationData,
                         selMetrics, params, type, p, nCores) {

  paramCombinations <-  expand.grid(params) %>%
    data.frame()

  test_randomForest <- function(combination,
                                trainingData,
                                calibrationData,
                                selMetrics,
                                type) {

    if (type == "class") {
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
                                       ntree    = 50,
                                       replace  = FALSE,
                                       nodesize = combination$nodesize,
                                       maxnodes = combination$maxnodes,
                                       strata   = trainingData$pressure,
                                       sampsize = nToSample),
            silent = TRUE)

      if ("try-error" %in% class(rf)) {
        return(NA)
      } else {
        perf <- pROC::roc(calibrationData$pressure,
                          predict(rf,
                                  newdata = calibrationData,
                                  type    = "prob")[, "impaired"])$auc
      }
    } else {
      if (type == "gradient") {
        nToSample <- (combination$sampsize * nrow(trainingData)) %>%
          round()

        rf <-
          try(randomForest::randomForest(x = trainingData[, selMetrics],
                                         y = trainingData$pressure,
                                         xtest    = calibrationData[, selMetrics],
                                         ytest    = calibrationData$pressure,
                                         mtry     = combination$mtry,
                                         ntree    = 50,
                                         replace  = FALSE,
                                         nodesize = combination$nodesize,
                                         maxnodes = combination$maxnodes,
                                         strata   = trainingData$pressure,
                                         sampsize = nToSample),
              silent = TRUE)

        if ("try-error" %in% class(rf)) {
          return(NA)
        } else {
          perf <- 1/rf$test$mse
        }
      } else {
        stop("type should be either class or gradient")
      }
    }
    return(perf)
  }

  if (type == "gradient" |
      (type == "class" & min(table(trainingData$pressure)) >= 100)) {
    cl <- parallel::makeCluster(nCores)

    parallel::clusterExport(cl,
                            c("test_randomForest", "trainingData",
                              "calibrationData", "selMetrics", "type", "%>%"),
                            envir = environment())

    paramCombinations$perf <-
      pbapply::pbsapply(split(paramCombinations,
                              rownames(paramCombinations)),
                        FUN             = test_randomForest,
                        trainingData    = trainingData,
                        calibrationData = calibrationData,
                        selMetrics      = selMetrics,
                        type            = type,
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
