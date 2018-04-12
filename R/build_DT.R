#' Build ecological Diagnostic Tool
#'
#' This function allows to build an ecological Diagnostic Tool (DT) that
#' predicts an Impairment Probability for one or several anthropogenic
#' pressures.
#'
#' The function takes as input two tables: one with a categorical description
#' (quality classes) of samples by one or several anthropogenic `pressures` and
#' the second with the values of biological `metrics` calculated from the
#' community data from the same samples.
#'
#' For each pressure (i.e. column in the `pressures` table), a model is built
#' and saved in the directory given by the `pathDT` argument. The whole set of
#' models (DT units) saved in this directory constitute the DT.
#'
#' Each DT unit is a probability random forest model built using the
#' [ranger][ranger::ranger()] function to predict the probability of a community
#' being impaired by the pressure considered based on the biological metrics
#' exhibited by the communities. The hyper-parameters of the
#' [ranger][ranger::ranger()] model are given in the params argument that could
#' accpt one or several values per parameter. If several parameter values are
#' given, then a grid search using [tuneParams][mlr::tuneParams] is performed to
#' identify the parameter set exhibiting the best trade-off between performance
#' and execution time when the model is built using training data and
#' performance assessed with the independent calibration data set. Then the
#' metrics exhibiting a negative importance (i.e. that degrade the model
#' performances) are discarded.
#'
#' @param metrics a data frame with samples in rows and biological metrics in
#'   columns
#'
#' @param pressures a data frame with samples in rows and pressure information
#'   in columns (one per pressure category). The table is filled with quality
#'   classes (i.e. low or impaired)
#'
#' @param pathDT character string, the path where the built models will be saved
#'
#' @param params a named list with the values of the following ranger random
#'   forest parameters:
#'       - num.trees: Number of trees to grow;
#'       - mtry: Number of variables randomly sampled as candidates at each split;
#'       - sample.fraction: Proportion of samples to draw;
#'       - min.node.size: Minimum size of terminal nodes.
#'
#' @param CVfolds an integer indicating the number of parts made from the
#'   training data set and used to calibrate the model hyper-parameters.
#'
#' @param low,impaired character vectors with the labels of the pressure
#'   intensities (in `pressures`) corresponding to low impact and impaired
#'   situations, respectively.
#'
#' @param nCores an integer indicating the number of CPU cores available to
#'   parallelize the calibration step
#'
#' @return nothing, the models and used data are saved as .rda objects in the
#'   directory corresponding to the pathDT argument.
#'
#' @seealso [ranger][ranger::ranger] [tuneParams][mlr::tuneParams]
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

  set.seed(2017)

  dir.create(path = pathDT, recursive = TRUE)


  if (nrow(pressures) != nrow(metrics)) {
    stop("pressures and metrics tables should have the same lines")
  }


   for (p in colnames(pressures)) {
    cat("\n", p, ":\n", sep = "")

     pressure <- NULL

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
      dplyr::filter(!is.na(pressure))


    selMetrics    <- colnames(trainingData)[-1]

    if (any(sapply(params, length) > 1)) {
      cat("\n    calibration...\n")

      tunedModel <- calibrate_DT(trainingData    = trainingData,
                                 CVfolds        = CVfolds,
                                 selMetrics     = selMetrics,
                                 params         = params,
                                 p              = p,
                                 nCores         = nCores)
      bestParams <- tunedModel$opt.path %>%
        as.data.frame()

      print(bestParams[, c("num.trees", "mtry",
                           "sample.fraction", "min.node.size",
                           "auc.test.mean", "timeboth.test.mean")])

      auc.test.mean <- timeboth.test.mean <-
        auc.diff <- time.diff <- weight <- NULL

      bestParams <- dplyr::mutate(bestParams,
                                  auc = auc.test.mean,
                                  time = timeboth.test.mean) %>%
        dplyr::mutate(auc_diff = (max(auc) - auc) / (1 - min(auc)),
                      time_diff = (time - min(time)) / (max(time) - min(time))) %>%
        dplyr::mutate(weight = 2/3 * auc_diff + 1/3 * time_diff) %>%
        dplyr::filter(weight == min(weight)) %>%
        dplyr::mutate(num.trees = as.character(num.trees) %>%
                        as.numeric(),
                      mtry = as.character(mtry) %>%
                        as.numeric(),
                      sample.fraction = as.character(sample.fraction) %>%
                        as.numeric(),
                      min.node.size = as.character(min.node.size) %>%
                        as.numeric()
        )

    } else {
      bestParams <- do.call(what = "cbind", args = params) %>%
        data.frame()
    }

    cat("\n    parameter values:\n")

    print(bestParams[, c("num.trees", "mtry",
                         "sample.fraction", "min.node.size")])

    learner <- mlr::makeLearner(cl           = "classif.ranger",
                                predict.type = "prob",
                                num.threads  = nCores,
                                replace      = FALSE)

    task <- mlr::makeClassifTask(data     = trainingData[, c("pressure",
                                                             selMetrics)],
                                 target   = "pressure",
                                 positive = "impaired")

    DTunit <- mlr::bootstrapB632(learner         = learner,
                                 num.trees       = bestParams$num.trees,
                                 mtry            = bestParams$mtry,
                                 sample.fraction = bestParams$sample.fraction,
                                 min.node.size   = bestParams$min.node.size,
                                 task      = task,
                                 iters     = 30,
                                 stratify  = TRUE,
                                 models    = TRUE,
                                 keep.pred = TRUE,
                                 measures  = list(mlr::auc, mlr::timeboth),
                                 show.info = FALSE)

        save(DTunit, trainingData,
             file = file.path(pathDT, paste0("model_", p, ".rda")))
   }
}


#' @import mlr
#' @export
predict_DT <- function(object,
                       newdata,
                       pred.all = TRUE) {

  IP_all <- pbapply::pblapply(object$models,
                              predict,
                              newdata = newdata) %>%
    lapply(function(pred) {
      pred$data$prob.impaired
    })                                           %>%
    do.call(what = cbind)                        %>%
    data.frame()                                 %>%
    dplyr::as.tbl()
  colnames(IP_all) <- paste("iter", 1:length(object$models), sep = "_")

  IP_summ <- apply(IP_all, 1, mean) %>%
    data.frame()                    %>%
    dplyr::as.tbl()
  colnames(IP_summ) <- "average"

  if (pred.all) {
    return(list(IP_all = IP_all, IP_summ = IP_summ))
  } else {
    return(IP_summ)
  }
}
