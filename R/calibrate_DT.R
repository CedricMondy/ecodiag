#' @importFrom dplyr "%>%"
calibrate_DT <- function(trainingData, CVfolds = 5,
                         selMetrics,
                         params,
                         p, nCores, calibPopSize, calibGenNb) {

  learner <- mlr::makeLearner(cl           = "classif.ranger",
                              predict.type = "prob",
                              num.threads  = nCores,
                              replace      = FALSE)

  task <- mlr::makeClassifTask(data     = trainingData[, c("pressure", selMetrics)],
                               target   = "pressure",
                               positive = "impaired")

  control <- mlr::makeTuneMultiCritControlNSGA2(popsize     = calibPopSize,
                                                generations = calibGenNb)

  sampler <- mlr::makeResampleDesc(method        = "CV",
                                   predict       = "test",
                                   iters         = CVfolds,
                                   stratify      = TRUE)

  set_param <- function(param, params) {
      if (param == "sample.fraction") {
        ParamHelpers::makeNumericParam(param,
                                       lower = min(params[[param]]) - 1e-6,
                                       upper = max(params[[param]]) + 1e-6,
                                       tunable = min(params[[param]]) !=
                                         max(params[[param]]))
      } else {
        ParamHelpers::makeIntegerParam(param,
                                       lower = min(params[[param]]) - 1e-6,
                                       upper = max(params[[param]]) + 1e-6,
                                       tunable = min(params[[param]]) !=
                                         max(params[[param]]))
      }
  }

  paramSet <- ParamHelpers::makeParamSet(
    set_param("num.trees", params),
    set_param("mtry", params),
    set_param("sample.fraction", params),
    set_param("min.node.size", params)
  )

  if (min(table(trainingData$pressure)) >= 100) {

    mlr::tuneParamsMultiCrit(learner    = learner,
                             task       = task,
                             resampling = sampler,
                             measures   = list(mlr::auc, mlr::timeboth),
                             par.set    = paramSet,
                             control    = control,
                             show.info  = TRUE)
  } else {
    warning("Not enough training data to build a model for ", p,
            "\n    not impaired: ",
            length(trainingData$pressure[trainingData$pressure == "low"]),
            " / impaired: ",
            length(trainingData$pressure[trainingData$pressure == "impaired"]))
    return(NULL)
  }
}
