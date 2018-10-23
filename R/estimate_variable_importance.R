#' Estimate variable importance
#'
#' This function allows to estimate the importance of individual variables in a
#' model unit of a diagnostic tool.
#'
#' This functions estimates the variable importance of all the variables
#' included in the investigated model using the importance metric(s) specified
#' in the `method` argument. In this regard, the function is a wrapper around
#' the function [generateFilterValuesData][mlr] from the `mlr` package with the
#' possibility to run multiple iterations for the metrics `ranger.impurity` and
#' `ranger.permutation` potentially in parallel (using `nCores` larger than 1).
#'
#' The second step performed by this function corresponds to the production of a
#' plot representing the importance of the most important variables. The
#' selection of the metrics is performed by ranking the importance metric values
#' and the number of variables to be represented is controlled by the argument
#' `nVarToPlot`. If several importance metrics are used, the selection is made
#' on the average rank of the variables over the different metrics.
#'
#' @param modelPath the path of the RData file where the model is saved
#' @param nVarToPlot numeric. The number of most important variables to
#'   graphically represent
#' @param methods character vector. The metric(s) used to estimate variable
#'   importance. The available choices are: `anova.test`, `auc`, `chi.squared`
#'   (package [FSelector]), `gain.ratio` ([FSelector]), `information.gain`
#'   ([FSelector]), `kruskal.test`, `ranger.impurity` ([ranger]) and/or
#'   `ranger.permutation` ([ranger])
#' @param nIter integer. If `ranger.impurity` or `ranger.permutation` is used as
#'   importance metrics, the number of times the estimate is repeated.
#' @param nCores integer.If `ranger.impurity` or `ranger.permutation` is used as
#'   importance metrics and `nIter` larger than 1, the number of CPUs used to
#'   perform the computations.
#'
#' @return a list with two elements: `varImp`: the table with the importance
#'   measure(s) for all variables and `varImpPlot` a ggplot object representing
#'   the importance of the most important variables.
#'
#' @seealso [generateFilterValuesData][mlr]
#'
#' @importFrom dplyr "%>%"
#' @export
#'
estimate_variable_importance <-
  function(modelPath,
           methods    = c("anova.test", "auc", "chi.squared", "gain.ratio",
                          "information.gain", "kruskal.test", "ranger.impurity",
                          "ranger.permutation"),
           nVarToPlot = 20,
           nIter      = 10,
           nCores     = 1L) {

    if (any(! methods %in% c("anova.test", "auc", "chi.squared", "gain.ratio",
                            "information.gain", "kruskal.test", "ranger.impurity",
                            "ranger.permutation")))
      stop("Variable importance criteria should be selected among: anova.test, auc, chi.squared, gain.ratio, information.gain, kruskal.test, ranger.impurity and/or ranger.permutation")

    load(modelPath)

    varImp <- data.frame(name = "x", type = "x",
                         importance_metric = "x", value = 0,
                         stringsAsFactors  = FALSE) %>%
      slice(0)

    if (length(methods[! grepl(pattern = "ranger.",
                              x = methods, fixed = TRUE)]) > 0) {
      varImp <-
        mlr::generateFilterValuesData(
          task   = task,
          method = methods[! grepl(pattern = "ranger.",
                                   x       = methods,
                                   fixed   = TRUE)]) %>%
        '[['("data")                                 %>%
        tidyr::gather(key = "importance_metric",
                      value = "value", -name, -type) %>%
        dplyr::bind_rows(varImp, .)
    }

    if (length(methods[grepl(pattern = "ranger.",
                            x = methods, fixed = TRUE)]) > 0) {
      cl <- parallel::makeCluster(nCores)
      parallel::clusterExport(cl, c("task"), envir = environment())
      parallel::clusterEvalQ(cl, expr = library("mlr"))

      varImp <-
        pbapply::pblapply(X      = seq(nIter),
                          FUN    = mlr::generateFilterValuesData,
                          task   = task,
                          method = methods[grepl(pattern = "ranger.", x = methods,
                                        fixed = TRUE)],
                 cl     = cl)                        %>%
        lapply(X = ., FUN = '[[', "data")            %>%
        dplyr::bind_rows()                           %>%
        tidyr::gather(key = "importance_metric",
                      value = "value", -name, -type) %>%
        dplyr::bind_rows(varImp, .)

      parallel::stopCluster(cl)
    }

    top_n <- varImp                           %>%
      dplyr::group_by(name, importance_metric)       %>%
      dplyr::summarise(median_value = median(value)) %>%
      dplyr::group_by(importance_metric)             %>%
      dplyr::mutate(rank = rank(median_value))       %>%
      dplyr::group_by(name)                          %>%
      dplyr::summarise(mean_rank = mean(rank))       %>%
      dplyr::arrange(desc(mean_rank))                %>%
      dplyr::slice(seq(nVarToPlot))

    varImpTop <- dplyr::filter(varImp, name %in% top_n$name)  %>%
      dplyr::mutate(name = factor(name, levels = top_n$name))

    varImpPlot <- ggplot2::ggplot(data = varImpTop,
                                  ggplot2::aes(x = name, y = value)) +
      ggplot2::geom_boxplot()                                        +
      ggplot2::coord_flip()                                          +
      ggplot2::facet_wrap(~importance_metric, scales = "free_x")

    list(varImp = dplyr::as.tbl(varImp), varImpPlot = varImpPlot)
}

