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
#' [ranger][ranger::ranger] function to predict the probability of a community
#' being impaired by the pressure considered based on the biological metrics
#' exhibited by the communities.
#'
#' For each DT unit, the given metrics and pressures tables are splitted in
#' training and test data sets. This is performed using the trainingFrac
#' argument that specify the proportion of the data (once observations with
#' missing pressure are removed) from each pressure level (low or impaired) that
#' are used to constitute the training data (stratified sampling). By default,
#' trainingFrac refers to the observations (rows of metrics and pressures) but
#' if a grouping vector (e.g. site ID) is given to the argument samplingUnit,
#' then this the training data set is built by sampling among samplingUnit and
#' not among the rows. If a site has observations with different pressure levels
#' (low or impaired), then the level occuring with highest frequency is
#' allocated to the site.
#'
#' The hyper-parameters of the [ranger][ranger::ranger] model are given in the
#' params argument that could accpt one or a range of values (minimum and
#' maximum) per parameter. If a range of parameter values is given, then an
#' optimization procedure using [tuneParamsMultiCrit][mlr::tuneParamsMultiCrit]
#' is performed to identify the parameter set exhibiting the best trade-off
#' between performance (AUC) and execution time.
#'
#' @param metrics a data frame with samples in rows and biological metrics in
#'   columns
#'
#' @param pressures a data frame with samples in rows and pressure information
#'   in columns (one per pressure category). The table is filled with quality
#'   classes (e.g. low or impaired)
#'
#' @param pathDT character string, the path where the built models will be saved
#'
#' @param params a named list with the values, one or two (minimum, maximum) of
#'   the following parameters:
#'       - num.trees: Number of trees to grow;
#'       - mtry: Number of variables randomly sampled as candidates at each split;
#'       - sample.fraction: Proportion of samples to draw;
#'       - min.node.size: Minimum size of terminal nodes.
#'
#' @param CVfolds an integer indicating the number of parts made from the
#'   training data set and used to calibrate the model hyper-parameters.
#'
#' @param low,impaired character vectors with the labels of the pressure classes
#'   (in `pressures`) corresponding to low impact and impaired situations,
#'   respectively.
#'
#' @param nIter an integer indicating the number of ranger RF models created for
#'   each pressure type. nIter larger than 1 allow to estimate prediction
#'   uncertainty and improve model robustness.
#'
#' @param nCores an integer indicating the number of CPU cores available to
#'   parallelize the calibration step
#'
#' @param trainingFrac a number between 0 and 1 indicating which propotion of
#'   the data set will be used to train the model
#'
#' @param samplingUnit a vector with a length equal to the number of rows of
#'   metrics and pressures indicating to which group each observation belongs
#'   to. The training and test data sets will be obtained by sampling these
#'   groups and not the observations (except if samplingUnit = NULL, the
#'   default).
#'
#' @param calibPopSize numeric. The size of the population used by the genetic
#'   algorithm used to calibrate the parameters.
#'
#' @param calibGenNb numeric. The number of generations used by the genetic
#'   algorithm used to calibrate the parameters. (calibGenNb + 1) * calibPopSize
#'   gives the total number of iterations performed by the calibration
#'   algorithm.
#'
#' @param seed numeric. The seed used for the random number generator
#'
#' @return nothing, the models and used data are saved as .rda objects in the
#'   directory corresponding to the pathDT argument.
#'
#' @seealso [ranger][ranger::ranger]
#'   [tuneParamsMultiCrit][mlr::tuneParamsMultiCrit] [nsga2][mco::nsga2]
#'
#' @importFrom dplyr "%>%"
#' @export
build_DT <- function(metrics,
                     pressures,
                     low          = "low",
                     impaired     = "impaired",
                     pathDT,
                     params    = list(num.trees       = 500,
                                      mtry            = 25,
                                      sample.fraction = 0.632,
                                      min.node.size   = 25),
                     CVfolds      = 5,
                     nIter        = 1L,
                     nCores       = 1L,
                     trainingFrac = 1,
                     samplingUnit = NULL,
                     calibPopSize = 10,
                     calibGenNb   = 10,
                     seed         = 20181025) {

  set.seed(seed)

  num.trees <- mtry <- sample.fraction <- min.node.size <- ellapsedTime <-
    AUC <- auc_diff <- time_diff <- NULL

  dir.create(path = pathDT, recursive = TRUE)

  if (nrow(pressures) != nrow(metrics)) {
    stop("pressures and metrics tables should have the same lines")
  }

  cat("Building DT", file = paste0(pathDT, "log.csv"))

   for (p in colnames(pressures)) {
    cat("\n", p, "\n", sep = "")
     cat("\n", p, "\n", sep = "",
         file = paste0(pathDT, "log.csv"), append = TRUE)

     pressure <- NULL

    allData <- data.frame(pressure = pressures[[p]],
                               metrics)                      %>%
      dplyr::mutate(
        pressure = dplyr::case_when(pressure %in% low      ~ "low",
                                    pressure %in% impaired ~ "impaired",
                                    TRUE                   ~ NA_character_) %>%
               factor(x = ., levels = c("low", "impaired"))) %>%
      split_training_test(data = ., frac = trainingFrac,
                          group = samplingUnit)

    trainingData <- allData$training
    testData     <- allData$test

    rm(allData)

    selMetrics    <- colnames(trainingData)[-1]

    if (any(sapply(params, length) > 1)) {
      cat("\n    calibration...\n")
      cat("\n    calibration...\n",
          file = paste0(pathDT, "log.csv"), append = TRUE)

      tunedModel <- calibrate_DT(trainingData   = trainingData,
                                 CVfolds        = CVfolds,
                                 selMetrics     = selMetrics,
                                 params         = params,
                                 p              = p,
                                 nCores         = nCores,
                                 calibPopSize   = calibPopSize,
                                 calibGenNb     = calibGenNb)

      testedParams <- tunedModel$opt.path %>%
        as.data.frame() %>%
        dplyr::select(num.trees, mtry, sample.fraction, min.node.size,
                      AUC = auc.test.mean,
                      ellapsedTime = timeboth.test.mean)

      suppressWarnings(utils::write.table(testedParams,
                                          file = paste0(pathDT, "log.csv"),
                                          sep = ";", append = TRUE,
                                          row.names = FALSE))

      bestParams <- tunedModel$x[[1]] %>%
        as.data.frame() %>%
        dplyr::bind_cols(tunedModel$y %>%
                           as.data.frame() %>%
                           dplyr::slice(1) %>%
                           dplyr::rename(AUC = auc.test.mean,
                                         ellapsedTime = timeboth.test.mean))

    } else {
      bestParams <- do.call(what = "cbind", args = params) %>%
        data.frame() %>%
        dplyr::mutate(AUC = NA, ellapsedTime = NA)
    }

    cat("\n    best parameter values\n",
        file = paste0(pathDT, "log.csv"), append = TRUE)

    suppressWarnings(utils::write.table(bestParams,
                                        file = paste0(pathDT, "log.csv"),
          sep = ";", append = TRUE, row.names = FALSE))

    learner <- mlr::makeLearner(cl           = "classif.ranger",
                                predict.type = "prob",
                                num.threads  = nCores,
                                replace      = FALSE) %>%
      setHyperPars(learner = .,
                   par.vals = bestParams %>%
                     dplyr::select(-AUC, -ellapsedTime) %>%
                     as.list())

    task <- mlr::makeClassifTask(data     = trainingData[, c("pressure",
                                                             selMetrics)],
                                 target   = "pressure",
                                 positive = "impaired")

    DTunit <- mlr::bootstrapB632(learner   = learner,
                                 task      = task,
                                 iters     = nIter,
                                 stratify  = TRUE,
                                 models    = TRUE,
                                 keep.pred = TRUE,
                                 measures  = list(mlr::auc, mlr::timeboth),
                                 show.info = FALSE)

        save(DTunit, trainingData, testData, task,
             file = file.path(pathDT, paste0("model_", p, ".rda")))

        cat("    DONE")
   }
}
