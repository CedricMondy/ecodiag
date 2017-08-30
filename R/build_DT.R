#' Build ecological Diagnostic Tool
#'
#' @param pressures a data frame with samples in rows and pressure information
#'   in columns (one per pressure category). The table is filled with quality
#'   classes (i.e. low or impaired)
#'
#' @param metrics a data frame with the rows as pressures and the investigated
#'   biological metrics in columns
#'
#' @param path character string, the path where the built models will be saved
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
#'     - min.node.size: Minimum size of terminal nodes;
#'
#' @return nothing, the models and used data are saved as .rda objects in the
#'   directory corresponding to the path argument.
#'
#' @seealso [ranger]
#'
#' @importFrom dplyr "%>%"
#' @export
build_DT <- function(pressures,
                     metrics,
                     path,
                     calibrate = TRUE,
                     set_prop = c(train     = 0.7,
                                  calibrate = 0.15,
                                  test      = 0.15),
                     params    = list(num.trees    = 500,
                                      mtry     = seq(5, 50, by = 2.5),
                                      sample.fraction = seq(0.1, 0.7, by = 0.1),
                                      min.node.size = 25),
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
      any(!(c("num.trees", "mtry", "sample.fraction", "min.node.size") %in%
            names(params)))) {
    stop("\nparams should be a named list with the following elements:\n    num.trees, mtry, sample.fraction and min.node.size")
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
          ranger::ranger(data            = trainingData[, c("pressure", selMetrics)],
                         formula         = pressure ~ .,
                         replace         = FALSE,
                         mtry            = bestParams$mtry,
                         num.trees       = 250,
                         min.node.size   = bestParams$min.node.size,
                         sample.fraction = bestParams$sample.fraction,
                         importance      = 'impurity',
                         write.forest    = TRUE,
                         probability     = TRUE,
                         case.weights    = (1 - table(trainingData$pressure) /
                                              nrow(trainingData))[trainingData$pressure])

        cat("\n    metric selection...\n")

        selMetrics <-
          names(calibratedRF$variable.importance)[calibratedRF$variable.importance > 0]

        mod <- ranger::ranger(data            = trainingData[, c("pressure", selMetrics)],
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
                                                   nrow(trainingData))[trainingData$pressure])

        # dataErrors <- data.frame(IR = predict(object  = mod,
        #                                       data    = trainingData)$predictions[, "impaired"],
        #                          pressure = trainingData$pressure)
        #
        # IPthreshold <- optimize(f    = calc_errors,
        #                         data = dataErrors,
        #                         adjust   = 5,
        #                         interval = c(0,1)) %>%
        #   '$'("minimum")                           %>%
        #   round(digits = 2)
        #
        # DTunit        <- list(rf        = mod,
        #                       threshold = IPthreshold)
        DTunit        <- mod
        # class(DTunit) <- "DTmodel"

        save(DTunit, modelData, paramCombinations,
             file = file.path(path, paste0("model_", p, ".rda")))
      }

    } else {

      mod <- ranger::ranger(data            = modelData[, c("pressure", selMetrics)],
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

      # dataErrors <- data.frame(IR = predict(object  = mod,
      #                                       data    = modelData)$preditions[, "impaired"],
      #                          pressure = modelData$pressure)
      #
      # IPthreshold <- optimize(f    = calc_errors,
      #                         data = dataErrors,
      #                         adjust   = 5,
      #                         interval = c(0,1)) %>%
      #   '$'("minimum")                           %>%
      #   round(digits = 2)
      #
      # DTunit        <- list(rf        = mod,
      #                       threshold = IPthreshold)
      DTunit        <- mod
      # class(DTunit) <- "DTmodel"

      save(DTunit, modelData,
           file = file.path(path, paste0("model_", p, ".rda")))
    }
   }
}

#' @export
predict_DT <- function(object, newdata) {
  IR <- stats::predict(object  = object,
                       data    = newdata)$predictions[, "impaired"]

  # IP <- approx(x = c(0, object$threshold, 1),
  #              y = c(0, 0.5, 1),
  #              xout = IR)$y
  #
  # return(IP)
  return(IR)
}
