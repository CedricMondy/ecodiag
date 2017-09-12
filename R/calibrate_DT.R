#' @importFrom dplyr "%>%"
calibrate_DT <- function(trainingData, CVfolds = 5,
                         selMetrics, sampleBlocks = NULL,
                         params,
                         p, nCores) {

  learner <- mlr::makeLearner(cl           = "classif.ranger",
                              predict.type = "prob",
                              num.threads  = nCores,
                              replace      = FALSE)

  task <- mlr::makeClassifTask(data     = trainingData[, c("pressure", selMetrics)],
                               target   = "pressure",
                               positive = "impaired",
                               blocking = sampleBlocks)

  control <- mlr::makeTuneControlGrid()

  sampler <- mlr::makeResampleDesc(method        = "CV",
                                   predict       = "test",
                                   iters         = CVfolds,
                                   stratify      = TRUE)

  paramSet <- ParamHelpers::makeParamSet(
    ParamHelpers::makeDiscreteParam("num.trees",
                                    values = params$num.trees),
    ParamHelpers::makeDiscreteParam("mtry",
                                    values = params$mtry),
    ParamHelpers::makeDiscreteParam("sample.fraction",
                                    values = params$sample.fraction),
    ParamHelpers::makeDiscreteParam("min.node.size",
                                    values = params$min.node.size)
  )

  if (min(table(trainingData$pressure)) >= 100) {

    mlr::tuneParams(learner    = learner,
                    task       = task,
                    resampling = sampler,
                    measures   = list(mlr::auc),
                    par.set    = paramSet,
                    control    = control)
  } else {
    warning("Not enough training data to build a model for ", p,
            "\n    not impaired: ",
            length(trainingData$pressure[trainingData$pressure == "low"]),
            " / impaired: ",
            length(trainingData$pressure[trainingData$pressure == "impaired"]))
    return(NULL)
  }
}
