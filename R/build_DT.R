#' Build ecological Diagnostic Tool
#'
#' @param pressures a data frame with samples in rows and pressure information
#'   in columns (one per pressure category). The table is filled either with
#'   standardized intensities (`type = "gradient"`) or with quality classes
#'   (i.e. low or impaired; `type = "class"`)
#'
#' @param metrics a data frame with the rows as pressures and the investigated
#'   biological metrics in columns
#'
#' @param type character, either class or gradient
#'
#' @param path character string, the path where the built models will be saved
#'
#' @param calibrate logical, should the model be calibrated to limit overfitting
#'
#' @param set_prop numeric vector giving the proportion of the data allocated to
#'   the training, calibration and test data sets. Only used if calibrate = TRUE
#'
#' @param params a named list with the values of the following randomForest parameters:
#'     - ntree: Number of trees to grow;
#'     - mtry: Number of variables randomly sampled as candidates at each split;
#'     - sampsize: Proportion of samples to draw. For classification, the same
#'     number of samples is drawn for each class. If the data are unbalanced,
#'     the number of samples to draw is obtained by multiplying sampsize by the
#'     number of samples in the less represented class;
#'     - nodesize: Minimum size of terminal nodes;
#'     - maxnodes: Maximum number of terminal nodes trees in the forest can have.
#'
#' @return nothing, the models and used data are saved as .rda objects in the
#'   directory corresponding to the path argument.
#'
#' @seealso [randomForest]
#'
#' @importFrom dplyr '%>%'
#' @export
build_DT <- function(pressures,
                     metrics,
                     type = "gradient",
                     path,
                     calibrate = TRUE,
                     set_prop = c(train     = 0.7,
                                  calibrate = 0.15,
                                  test      = 0.15),
                     params    = list(ntree    = 500,
                                      mtry     = seq(5, 50, by = 2.5),
                                      sampsize = seq(0.1, 0.7, by = 0.1),
                                      nodesize = 25,
                                      maxnodes = seq(2, 50, by = 2)),
                     nCores    = 3L) {

  if (nrow(pressures) != nrow(metrics)) {
    stop("\nThe tables pressures and metrics should have the same rows.")
  }
  if (calibrate & length(set_prop) < 2) {
    stop("\nThe calibration can be done only if at least two sets of data are specified: training and calibration.")
  }

  if (calibrate & length(set_prop) > 3) {
    warning("\nOnly the first three elements of set_prop will be considered to allocate samples to training, calibration and test data sets.")
  }

  if (!is.list(params) |
      any(!(c("ntree", "mtry", "sampsize", "nodesize", "maxnodes") %in%
            names(params)))) {
    stop("\nparams should be a named list with the following elements:\n    ntree, mtry, sampsize, nodesize and maxnodes")
  }

  dir.create(path = path, recursive = TRUE)

   for (p in colnames(pressures)) {
    cat("\n", p, ":\n", sep = "")

    pressures[[p]][pressures[[p]] == Inf | pressures[[p]] == -Inf] <- NA

    modelData <- data.frame(pressure = pressures[[p]],
                                  metrics) %>%
      dplyr::filter(!is.na(pressure))      %>%
      (function(df) {
        df           <- dplyr::as.tbl(df)
        colnames(df) <- c("pressure", colnames(metrics))
        df
      })

    selMetrics    <- colnames(modelData)[-match(c("cd_opecont", "pressure"),
                                                colnames(modelData))]

    if (calibrate) {
      modelData$set <- sample_sets(x    = modelData$pressure,
                                   prop = set_prop)$sample

      trainingData    <- dplyr::filter(.data = modelData,
                                       set == "train")
      calibrationData <- dplyr::filter(.data = modelData,
                                       set == "calibrate")
      testData        <- dplyr::filter(.data = modelData,
                                       set == "test")

      cat("    calibration...\n")

      paramCombinations <- calibrate_DT(trainingData    = trainingData,
                                        calibrationData = calibrationData,
                                        selMetrics      = selMetrics,
                                        params          = params,
                                        type            = type,
                                        p               = p,
                                        nCores          = nCores)

      if (!is.null(paramCombinations)) {
        bestParams <- dplyr::filter(paramCombinations,
                                    perf == max(perf, na.rm = TRUE))

        if (type == "gradient") {
          nToSample <- (bestParams$sampsize *
                          nrow(trainingData)) %>%
            round()
        } else {
          if (type == "class") {
            nToSample <- table(trainingData$pressure) %>%
              min()                                   %>%
              '*'(bestParams$sampsize)                %>%
              round()                                 %>%
              rep(dplyr::n_distinct(trainingData$pressure))
          }
        }

        calibratedRF <-
          randomForest::randomForest(x          = trainingData[, selMetrics],
                                     y          = trainingData$pressure,
                                     replace    = FALSE,
                                     mtry       = bestParams$mtry,
                                     ntree      = bestParams$ntree,
                                     nodesize   = bestParams$nodesize,
                                     sampsize   = nToSample,
                                     maxnodes   = bestParams$maxnodes,
                                     importance = TRUE)

        cat("    metric selection...\n")

        if (type == "gradient") {
          selMetrics <- rownames(calibratedRF$importance)[
            (calibratedRF$importance[,1] -
               calibratedRF$importanceSD) > 0]
        } else {
          if (type == "class") {
            selMetrics <- rownames(calibratedRF$importance)[
              (calibratedRF$importance[, "MeanDecreaseAccuracy"] -
                 calibratedRF$importanceSD[, "MeanDecreaseAccuracy"]) > 0]
          }
        }


        mod <- randomForest::randomForest(x          = trainingData[, selMetrics],
                                          y          = trainingData$pressure,
                                          replace    = FALSE,
                                          mtry       = bestParams$mtry,
                                          ntree      = bestParams$ntree,
                                          nodesize   = bestParams$nodesize,
                                          sampsize   = nToSample,
                                          maxnodes   = bestParams$maxnodes,
                                          importance = TRUE)

        save(mod, modelData, paramCombinations,
             file = file.path(path, paste0("model_", p, ".rda")))
      }

    } else {
      if (type == "gradient") {
        nToSample <- (params$sampsize[1] *
                        nrow(modelData)) %>%
          round()
      } else {
        if (type == "class") {
          nToSample <- table(modelData$pressure) %>%
            min()                                %>%
            '*'(params$sampsize[1])              %>%
            round()                              %>%
            rep(dplyr::n_distinct(modelData$pressure))
        }
      }

      mod <- randomForest::randomForest(x          = modelData[, selMetrics],
                                        y          = modelData$pressure,
                                        replace    = FALSE,
                                        mtry       = params$mtry[1],
                                        ntree      = params$ntree[1],
                                        nodesize   = params$nodesize[1],
                                        sampsize   = nToSample,
                                        maxnodes   = params$maxnodes[1],
                                        importance = TRUE)

      save(mod, modelData,
           file = file.path(path, paste0("model_", p, ".rda")))
    }
   }
}
