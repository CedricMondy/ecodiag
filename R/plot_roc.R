#' @export
plot_roc <- function(rocobj) {
  dataPlot <- NULL

  if (class(rocobj) %in% c("roc", "smooth.roc")) {
    dataPlot <- data.frame(specificity = rocobj$specificities,
                           sensitivity = rocobj$sensitivities,
                           AUC       = paste0("AUC = ", round(rocobj$auc, 3)))

  } else {
    if (is.list(rocobj)) {
      testRoc <- sapply(rocobj,
                        function(roc) {
                          class(roc) %in% c("roc", "smooth.roc")
                          })

      if (any(!testRoc)) {
        rocobj <- rocobj[testRoc]

        if (length(rocobj) == 0) {
          stop("No roc object found in rocobj")
        } else {
          warning("At least one element from rocobj is not a roc object and was not considered")
        }
      }

      if (is.null(names(rocobj))) {
        names(rocobj) <- paste("roc", 1:length(rocobj))
      }

      dataPlot <- lapply(rocobj,
                         function(roc) {
                           data.frame(specificity = roc$specificities,
                                      sensitivity = roc$sensitivities,
                                      AUC       = paste0(" (AUC = ",
                                                         round(roc$auc, 3),
                                                         ")"))
                         })

      for (i in names(dataPlot)) {
        dataPlot[[i]]$AUC <- paste0(i, dataPlot[[i]]$AUC)
      }

      dataPlot <- do.call(dataPlot, what = rbind)
      rownames(dataPlot) <- NULL
    }
  }

  if (is.null(dataPlot)) {
    stop("No roc object found in rocobj")
  }

  rocPlot <- ggplot2::ggplot(data = dataPlot,
                             ggplot2::aes(x = specificity,
                                          y = sensitivity,
                                          colour = AUC)) +
    ggplot2::geom_path()                                 +
    ggplot2::scale_x_reverse()                           +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  return(rocPlot)
}
