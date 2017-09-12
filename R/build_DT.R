#' Build ecological Diagnostic Tool
#'
#' This function allows to build an ecological Diagnostic Tool (DT) that
#' predicts an Impairment Probability for one or several anthropogenic
#' pressures.
#'
#' The function takes as input two tables: one with a binary description (low vs
#' impaired) of samples by one or several anthropogenic `pressures` and the
#' second with the values of biological `metrics` calculated from the community
#' data from the same samples.
#'
#' For each pressure (i.e. column in the `pressures` table), a model is built
#' and saved in the directory given by the `pathDT` argument. The whole set of
#' models (DT units) saved in this directory constitute the DT.
#'
#' Each DT unit is a probability random forest model built using the
#' [ranger::ranger()] function to predict the probability of a community being
#' impaired by the pressure considered based on the biological metrics exhibited
#' by the communities. The hyper-parameters of the [ranger::ranger()] model are
#' given in the params argument that could accpt one or several values per
#' parameter. If several parameter values are given and `calibrate = TRUE`, then
#' a grid search is performed to identify the parameter set exhibiting the best
#' performance when the model is built using training data and performance
#' assessed with the independent calibration data set. If `calibrate = TRUE`,
#' then the metrics exhibiting a negative importance (i.e. that degrade the
#' model performances) are discarded.
#'
#' @param pressures a data frame with samples in rows and pressure information
#'   in columns (one per pressure category). The table is filled with quality
#'   classes (i.e. low or impaired)
#'
#' @param metrics a data frame with the rows as pressures and the investigated
#'   biological metrics in columns
#'
#' @param pathDT character string, the path where the built models will be saved
#'
#' @param calibrate logical, should the model be calibrated to limit overfitting
#'
#' @param set_prop numeric vector giving the proportion of the data allocated to
#'   the training, calibration and test data sets. Only used if calibrate = TRUE
#'
#' @param params a named list with the values of the following ranger random
#'   forest parameters:
#'     - num.trees: Number of trees to grow;
#'     - mtry: Number of variables randomly sampled as candidates at each split;
#'     - sample.fraction: Proportion of samples to draw;
#'     - min.node.size: Minimum size of terminal nodes
#'
#' @param nCores an integer indicating the number of CPU cores available to
#'   parallelize the calibration step
#'
#' @return nothing, the models and used data are saved as .rda objects in the
#'   directory corresponding to the pathDT argument.
#'
#' @seealso [ranger]
#'
#' @importFrom dplyr "%>%"
#' @export
build_DT <- function(metrics,
                     pressures,
                     pathDT,
                     params    = list(num.trees       = 500,
                                      mtry            = 25,
                                      sample.fraction = 0.632,
                                      min.node.size   = 25),
                     CVfolds      = 5,
                     low          = "low",
                     impaired     = "impaired",
                     nCores       = 3L) {

  dir.create(path = pathDT, recursive = TRUE)


  if (nrow(pressures) != nrow(metrics)) {
    stop("pressures and metrics tables should have the same lines")
  }


   for (p in colnames(pressures)) {
    cat("\n", p, ":\n", sep = "")

    trainingData <- data.frame(pressure = pressures[[p]],
                               metrics) %>%
      (function(df) {
        colnames(df) <- c("pressure", colnames(metrics))

        df$pressure <- as.character(df$pressure) %>%
          gsub(pattern = paste(low, collapse = "|"),
               replacement = "low")              %>%
          gsub(pattern = paste(impaired, collapse = "|"),
               replacement = "impaired")         %>%
          factor(levels = c("low", "impaired"))

        df
      })                                         %>%
      dplyr::filter(!is.na(pressure))            %>%
      dplyr::as.tbl()


    selMetrics    <- colnames(trainingData)[-1]

    cat("\n    calibration...\n")

    bestParams <- calibrate_DT(trainingData    = trainingData,
                               CVfolds         = CVfolds,
                               selMetrics      = selMetrics,
                               params          = params,
                               p               = p,
                               nCores          = nCores)

    calibratedRF <-
      ranger::ranger(data            = trainingData,
                     formula         = pressure ~ .,
                     replace         = FALSE,
                     mtry            = bestParams$x$mtry,
                     num.trees       = bestParams$x$num.trees,
                     min.node.size   = bestParams$x$min.node.size,
                     sample.fraction = bestParams$x$sample.fraction,
                     importance      = 'impurity',
                     write.forest    = TRUE,
                     probability     = TRUE,
                     case.weights    = (1 - table(trainingData$pressure) /
                                          nrow(trainingData))[trainingData$pressure]
      )

    cat("\n    metric selection...\n")

    selMetrics <-
      names(calibratedRF$variable.importance)[calibratedRF$variable.importance > 0]

    DTunit <- list()

    DTunit$rf <-
      ranger::ranger(data            = trainingData[, c("pressure", selMetrics)],
                     formula         = pressure ~ .,
                     replace         = FALSE,
                     mtry            = bestParams$x$mtry,
                     num.trees       = bestParams$x$num.trees,
                     min.node.size   = bestParams$x$min.node.size,
                     sample.fraction = bestParams$x$sample.fraction,
                     importance      = 'impurity',
                     write.forest    = TRUE,
                     probability     = TRUE,
                     case.weights    = (1 - table(trainingData$pressure) /
                                          nrow(trainingData))[trainingData$pressure])


    DTunit$auc <- bestParams$y

        save(DTunit, trainingData,
             file = file.path(pathDT, paste0("model_", p, ".rda")))
   }
}


#' @export
predict_DT <- function(object,
                       newdata,
                       uncertainty = FALSE,
                       IC          = 90,
                       nSim        = 100) {

  estimate_IC <- function(x, nSim, lower, upper, modelAUC) {
    meanSample <- sapply(1:nSim,
                         function(i) {
                           sample(x       = x,
                                  size    = length(x),
                                  replace = TRUE) %>%
                             mean()
                         })

    mu    <- mean(meanSample)
    stdev <- sd(meanSample) / modelAUC^2

    return(c(lower = qnorm(p = lower, mean = mu, sd = stdev),
             mean   = mu,
             upper = qnorm(p = upper, mean = mu, sd = stdev)))
  }

  if (!uncertainty) {
    IP <- stats::predict(object      = object$rf,
                         data        = newdata,
                         predict.all = FALSE)$predictions[, "impaired"]
  } else {
    lower <- 0 + (1 - IC/100) / 2
    upper <- 1 - (1 - IC/100) / 2

    IP <- stats::predict(object      = object$rf,
                         data        = newdata,
                         predict.all = TRUE)$predictions

    IP <- lapply(1:object$rf$num.trees,
                 function(i) {
                   IP[, 2, i]
                 })         %>%
      do.call(what = cbind) %>%
      apply(MARGIN = 1,
            estimate_IC,
            nSim = nSim,
            lower = lower, upper = upper,
            modelAUC = object$auc) %>%
      t()
  }

  return(IP)
}
