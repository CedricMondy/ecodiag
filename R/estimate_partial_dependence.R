#' Estimate partial dependence of model predictions to variables
#'
#' This functions evaluates and represents the partial dependence of a model
#' prediction to changes in individual variable values.
#'
#' The function estimates (using [generatePartialDependenceData][mlr]) how the
#' model stored in `modelPath` is affected by changes in one of several
#' `variables` values. The partial dependence of the model for a given variable
#' is calculated by setting all the other variables to their mean values and by
#' exploring the changes in model prediction by setting the values of the
#' variable of interest over a range. If, during the construction of the DT unit
#' using [build_DT], several models were trained (i.e. `nIter` larger than 1),
#' the partial dependence is calculated and represented for each of the
#' individual model.
#'
#' Once the partial dependence values calculated, a plot is produced
#' representing the changes in model predictions over the range of the variable
#' values. A [loess][stats] smoothed curve is added to represent the tendencies.
#' The distribution of the values of the variable of interest in the training
#' data set is also represented.
#'
#' @inheritParams estimate_variable_importance
#' @param variables character vector. The names of the variables for which the
#'   partial independence should be evaluated.
#'
#' @return a list with two elements: `data` the estimated partial dependence
#'   values and `plot` a ggplot object representing the trends of the
#'   relationships between model predictions and the values of the variable of
#'   interest.
#'
#' @seealso [generatePartialDependenceData][mlr]
#'
#' @import mmpf
#' @export
#'
estimate_partial_dependence <- function(modelPath, variables) {
  load(modelPath)

  partialDep <- pbapply::pblapply(seq(length(DTunit$models)),
                         function(i) {
                           mlr::generatePartialDependenceData(
                             obj = DTunit$models[[i]],
                             task,
                             features = variables) %>%
                             '[['("data")          %>%
                             tidyr::gather(key   = "trait",
                                           value = "value",
                                           -impaired)
                           })  %>%
    dplyr::bind_rows()         %>%
    dplyr::filter(!is.na(value))

  partialPlot <- ggplot2::ggplot()                                             +
    ggplot2::geom_point(data = partialDep,
                        ggplot2::aes(x = value, y = impaired))  +
    ggplot2::geom_smooth(data = partialDep,
                         ggplot2::aes(x = value, y = impaired),
                         method = "loess") +
    ggplot2::geom_rug(data = dplyr::select(trainingData, !!variables) %>%
               tidyr::gather(key = trait, value = value),
               ggplot2::aes(x = value),
             colour = "red")                                    +
    ggplot2::facet_wrap(~trait, scales = "free_x")

  list(data = partialDep, plot = partialPlot)
}



