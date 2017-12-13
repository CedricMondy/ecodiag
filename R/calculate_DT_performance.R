#' @importFrom dplyr "%>%"
#' @export
calculate_DT_performance <- function(modelPath,
                                     data,
                                     low       = "low",
                                     impaired  = "impaired",
                                     smoothROC = TRUE) {

  modelList <- list.files(path       = modelPath,
                          pattern    = "model_*",
                          full.names = TRUE)

  pressureList <- gsub(modelList,
                       pattern     = paste0(modelPath,
                                            "model_"),
                       replacement = "") %>%
    gsub(pattern     = ".rda",
         replacement = "")

    calc_model_performance <- function(i, low, impaired) {
      DTunit <- NULL
      load(modelList[i])

      calc_roc <- function(data.s, p, low, impaired, smoothROC) {
        pressure <- NULL

        subData <- data.frame(pressure = data.s$pressures[[p]],
                              data.s$metrics)        %>%
          (function(df) {
            colnames(df) <- c("pressure", colnames(data.s$metrics))

            df$pressure <- as.character(df$pressure) %>%
              gsub(pattern = paste(low, collapse = "|"),
                   replacement = "low")              %>%
              gsub(pattern = paste(impaired, collapse = "|"),
                   replacement = "impaired")         %>%
              factor(levels = c("low", "impaired"))

            df
          })                                         %>%
          dplyr::filter(!is.na(pressure))


        pROC::roc(response  = subData$pressure,
                  predictor = predict_DT(object      = DTunit,
                                         newdata     = subData),
                  ci        = TRUE,
                  smooth    = smoothROC)
        }

      if (is.null(names(data))) {
        names(data) <- paste("data", 1:length(data))
      }

      rocs <- lapply(data,
                     calc_roc,
                     p         = pressureList[i],
                     low       = low,
                     impaired  = impaired,
                     smoothROC = smoothROC)

      aucs <- sapply(rocs, '[[', "ci") %>%
        t()                            %>%
        round(3)                       %>%
        data.frame(pressureList[i],
                   round(DTunit$auc, 3),
                   names(data),
                   .)
      colnames(aucs) <- c("pressure", "AUC training", "data",
                          "CI 5%", "AUC", "CI 95%")

      ROCurves <- plot_roc(rocobj = rocs)             +
        ggplot2::theme_bw()                           +
        ggplot2::scale_colour_discrete(name   = "Data sets") +
        ggplot2::ggtitle(pressureList[i])             +
        ggplot2::geom_label(x = -0.125, y = 0.06,
                            label = paste("AUC training =", round(DTunit$auc, 3)),
                            colour = "black")

      return(list(AUC = aucs, ROC = ROCurves))

    }

    modelPerformances <- pbapply::pblapply(1:length(modelList),
                                           calc_model_performance,
                                           low      = low,
                                           impaired = impaired)

    AUC <- lapply(modelPerformances,
                  "[[", "AUC") %>%
      dplyr::bind_rows()

    ROC <- lapply(modelPerformances,
                  "[[", "ROC")

    performances <- list(AUC = AUC, ROC = ROC)

    return(performances)
}
