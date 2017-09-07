#' @importFrom dplyr "%>%"
#' @export
calculate_DT_performance <- function(modelPath,
                                     sets = c("train", "calibrate", "test"),
                                     smoothROC = TRUE) {
  # modelPath <- "C:/Users/cedri/CloudStation/LIEC_ONEMA/R/Logiciels/ecodiag/temp/models/class/"

  modelList <- list.files(path       = modelPath,
                          pattern    = "model_*",
                          full.names = TRUE)

  pressureList <- gsub(modelList,
                       pattern     = paste0(modelPath,
                                            "model_"),
                       replacement = "") %>%
    gsub(pattern     = ".rda",
         replacement = "")

    calc_model_performance <- function(p) {
    load(modelList[p])

      calc_roc <- function(s) {
        subData <- dplyr::filter(modelData, set == s)

        pROC::roc(response  = subData$pressure,
                  predictor = predict_DT(DTunit,
                                         newdata    = subData,
                                         uncertainty = FALSE),
                  ci        = TRUE,
                  smooth    = smoothROC)
        }

      rocs <- lapply(sets,
                     calc_roc)
      names(rocs) <- sets

      aucs <- sapply(rocs, '[[', "ci") %>%
        t()                            %>%
        round(3)                       %>%
        data.table::data.table(pressureList[p],
                               sets,
                               .)
      colnames(aucs) <- c("pressure", "sets",
                          "AUC_2.5%", "AUC", "AUC_97.5%")

      ROCurves <- plot_roc(rocobj = rocs)             +
        ggplot2::theme_bw()                           +
        ggplot2::scale_colour_discrete(name   = "Data sets",
                                       breaks = sets) +
        ggplot2::ggtitle(pressureList[p])

      return(list(AUC = aucs, ROC = ROCurves))

    }

    modelPerformances <- pbapply::pblapply(1:length(modelList),
                                           calc_model_performance)

    AUC <- lapply(modelPerformances,
                  "[[", "AUC") %>%
      dplyr::bind_rows()

    ROC <- lapply(modelPerformances,
                  "[[", "ROC")

    performances <- list(AUC = AUC, ROC = ROC)

    return(performances)
}
