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
build_DT <- function(training,
                     pathDT,
                     calibrate = TRUE,
                     params    = list(num.trees    = 500,
                                      mtry     = seq(5, 50, by = 2.5),
                                      sample.fraction = seq(0.1, 0.7, by = 0.1),
                                      min.node.size = 25),
                     calibration = NULL,
                     low         = "low",
                     impaired    = "impaired",
                     nCores      = 3L) {

  dir.create(path = pathDT, recursive = TRUE)

  if (!is.list(training) | all(!(names(training) %in% c("pressures", "metrics")))) {
    stop("training should be a named list with tables pressures and metrics")
  } else {
    if (nrow(training$pressures) != nrow(training$metrics)) {
      stop("in the training list, pressures and metrics tables should have the same lines")
    }
  }

  if (!is.null(calibration) & calibrate) {
    if (!is.list(calibration) | all(!(names(calibration) %in% c("pressures", "metrics")))) {
      stop("calibration should be a named list with tables pressures and metrics")
    } else {
      if (nrow(calibration$pressures) != nrow(calibration$metrics)) {
        stop("in the calibration list, pressures and metrics tables should have the same lines")
      } else {
        if (any(colnames(training$pressures) != colnames(calibration$pressures)) |
            any(colnames(training$metrics) != colnames(calibration$metrics))) {
          stop("if model calibration pressures and metrics tables should have the same columns than training pressures and metrics tables")
        }
      }
    }
  }

   for (p in colnames(training$pressures)) {
    cat("\n", p, ":\n", sep = "")

    trainingData <- data.frame(pressure = training$pressures[[p]],
                               training$metrics) %>%
      (function(df) {
        colnames(df) <- c("pressure", colnames(training$metrics))

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

    if (calibrate) {
      calibrationData <- data.frame(pressure = calibration$pressures[[p]],
                                    calibration$metrics) %>%
        (function(df) {
          colnames(df) <- c("pressure", colnames(calibration$metrics))

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


      cat("\n    calibration...\n")

      paramCombinations <- calibrate_DT(trainingData    = trainingData,
                                        calibrationData = calibrationData,
                                        selMetrics      = selMetrics,
                                        params          = params,
                                        p               = p,
                                        nCores          = nCores)

      if (!is.null(paramCombinations)) {
        bestParams <- dplyr::filter(paramCombinations,
                                    perf == max(perf, na.rm = TRUE))

        calibratedRF <-
          ranger::ranger(data            = trainingData,
                         formula         = pressure ~ .,
                         replace         = FALSE,
                         mtry            = bestParams$mtry,
                         num.trees       = bestParams$num.trees,
                         min.node.size   = bestParams$min.node.size,
                         sample.fraction = bestParams$sample.fraction,
                         importance      = 'impurity',
                         write.forest    = TRUE,
                         probability     = TRUE,
                         case.weights    = (1 - table(trainingData$pressure) /
                                              nrow(trainingData))[trainingData$pressure]
                         )
        cat("\n    forest prunning...\n")

        ntrees <- seq(from = 50, to = calibratedRF$num.trees, by = 50)

        aucsCalibration <- data.frame(
          ntree = ntrees,
          AUC   = pbapply::pbsapply(
            ntrees,
            function(i) {
              pred.i <- stats::predict(calibratedRF,
                                       data      = calibrationData,
                                       num.trees = i)$predictions

              pROC::roc(response  = calibrationData$pressure,
                        predictor = pred.i[, "impaired"],
                        smooth    = TRUE)$auc
            }
            )
          )


        aucsCalibration$smoothAUC <- stats::loess(data    = aucsCalibration,
                                                  formula = AUC ~ ntree) %>%
          stats::predict()

        ntreeOptim <- dplyr::filter(aucsCalibration,
                                    smoothAUC == max(smoothAUC))$ntree

        cat("\n    metric selection...\n")

        selMetrics <-
          names(calibratedRF$variable.importance)[calibratedRF$variable.importance > 0]

        mod <- ranger::ranger(data            = trainingData[, c("pressure", selMetrics)],
                              formula         = pressure ~ .,
                              replace         = FALSE,
                              mtry            = bestParams$mtry,
                              num.trees       = ntreeOptim,
                              min.node.size   = bestParams$min.node.size,
                              sample.fraction = bestParams$sample.fraction,
                              importance      = 'impurity',
                              write.forest    = TRUE,
                              probability     = TRUE,
                              case.weights    = (1 - table(trainingData$pressure) /
                                                   nrow(trainingData))[trainingData$pressure])

        DTunit        <- mod

        save(DTunit, paramCombinations,
             file = file.path(pathDT, paste0("model_", p, ".rda")))
      }

    } else {

      DTunit <- ranger::ranger(data            = trainingData[, c("pressure", selMetrics)],
                               formula         = pressure ~ .,
                               replace         = FALSE,
                               mtry            = params$mtry[1],
                               num.trees       = params$num.trees[1],
                               min.node.size   = params$min.node.size[1],
                               sample.fraction = params$sample.fraction[1],
                               importance      = 'impurity',
                               write.forest    = TRUE,
                               probability     = TRUE,
                               case.weights    = (1 - table(modelData$pressure) /
                                                    nrow(modelData))[modelData$pressure])


      save(DTunit,
           file = file.path(pathDT, paste0("model_", p, ".rda")))
    }
   }
}

#' @export
predict_DT <- function(object, newdata, uncertainty = FALSE, IC = 90, nSim = 100) {

  calc_beta_parameters <- function(mu, variance) {
    alpha <- ((1 - mu) / variance - 1/mu) * mu^2
    beta  <- ((1 - mu) / variance - 1/mu) * mu * (1 - mu)

    return(c(alpha = alpha, beta = beta))
  }

  estimate_beta <- function(nSim, alpha, beta, lower, upper) {
    meanSample <- sapply(1:nSim,
                         function(i) {
                           rbeta(n = 1000,
                                 shape1 = alpha,
                                 shape2 = beta) %>%
                             mean()
                         })

    mu    <- mean(meanSample)
    stdev <- sd(meanSample)

    return(c(lower = qnorm(p = lower, mean = mu, sd = stdev),
             mean   = mu,
             upper = qnorm(p = upper, mean = mu, sd = stdev)))
  }

  if (!uncertainty) {
    IP <- stats::predict(object      = object,
                         data        = newdata,
                         predict.all = FALSE)$predictions[, "impaired"]
  } else {
    lower <- 0 + (1 - IC/100) / 2
    upper <- 1 - (1 - IC/100) / 2

    IP <- stats::predict(object      = object,
                         data        = newdata,
                         predict.all = TRUE)$predictions

    IP <- lapply(1:object$num.trees,
                 function(i) {
                   IP[, 2, i]
                 })         %>%
      do.call(what = cbind) %>%
      apply(MARGIN = 1,
            function(x) {
              c(mu       = mean(x),
                variance = var(x))
              })            %>%
      t()                   %>%
    apply(MARGIN = 1,
          function(x) {
            calc_beta_parameters(mu       = x[[1]],
                                 variance = x[[2]])
            })              %>%
      t()                   %>%
      apply(MARGIN = 1,
            function(x) {
              estimate_beta(nSim  = nSim,
                            alpha = x[[1]],
                            beta  = x[[2]],
                            lower = lower,
                            upper = upper)
            })              %>%
      t()
  }

  return(IP)
}
