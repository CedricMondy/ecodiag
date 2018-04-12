plot_roc <- function(rocobj, rocobj2 = NULL,
                     rocNames = c("rocobj",
                               ifelse(is.null(rocobj2),
                                      NULL, "rocobj2"))) {
  extractRocData <- function(rocData) {
    dataPlot <- NULL

    if (class(rocData) %in% c("roc", "smooth.roc")) {
      dataPlot <- data.frame(specificity = rocData$specificities,
                             sensitivity = rocData$sensitivities,
                             AUC       = paste0("AUC = ", round(rocData$auc, 3)))

    } else {
      if (is.list(rocData)) {
        testRoc <- sapply(rocData,
                          function(roc) {
                            class(roc) %in% c("roc", "smooth.roc")
                          })

        if (any(!testRoc)) {
          rocData <- rocData[testRoc]

          if (length(rocData) == 0) {
            stop("No roc object found")
          } else {
            warning("At least one element is not a roc object and was not considered")
          }
        }

        if (is.null(names(rocData))) {
          names(rocData) <- paste("roc", 1:length(rocData))
        }

        format_rocData <- function(rocData) {
          sensitivities <- specificities <- AUC <- sens <- low <- high <- NULL

          lapply(1:length(rocData),
                 function(i) {
                   data.frame(iter          = i,
                              sensitivities = rocData[[i]]$sensitivities,
                              specificities = rocData[[i]]$specificities)
                 })  %>%
            do.call(what = rbind) %>%
            dplyr::mutate(specificities = round(specificities, 2)) %>%
            dplyr::group_by(specificities) %>%
            dplyr::summarise(low  = min(sensitivities),
                             sens = mean(sensitivities),
                             high = max(sensitivities)) %>%
            dplyr::transmute(specificities = specificities,
                             sensitivities = sens,
                             low           = low,
                             high          = high) %>%
            dplyr::mutate(AUC = sapply(rocData,
                                       (function(obj) {
                                         obj$auc
                                         }))               %>%
                            mean()                         %>%
                            format(digits = 3, nsmall = 3) %>%
                            paste0("AUC = ", .))
        }

        dataPlot <- format_rocData(rocData)
      }
    }

    return(dataPlot)

  }

  dataPlot <- extractRocData(rocobj)
  if (!is.null(rocobj2)) {
    dataPlot <- data.frame(set = rocNames[1],
                           dataPlot,
                           stringsAsFactors = FALSE) %>%
      rbind(data.frame(set = rocNames[2],
                       extractRocData(rocobj2),
                       stringsAsFactors = FALSE)) %>%
      dplyr::as.tbl()
  }

  set <- AUC <- NULL

  dataPlot <- dplyr::mutate(dataPlot,
                            legend = paste0(set, " (", AUC, ")"))

  rocPlot <- ggplot2::ggplot(data = dataPlot,
                             ggplot2::aes(x = specificities,
                                          y = sensitivities,
                                          colour = legend,
                                          fill   = legend)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low,
                                      ymax = high),
                         alpha = 0.25)                   +
    ggplot2::geom_path()                                 +
    ggplot2::scale_x_reverse()                           +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::theme_bw()

  return(rocPlot)
}
