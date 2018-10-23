#' Calculate the performances of a DT
#'
#' This function allows to quantify the performances of a DT model built using
#' the  [build_DT][build_DT] function, ideally using a different data set than
#' the one used to train the DT.
#'
#' The function will calculate the ROC curves (smoothed or not depending on
#' argument `smoothROC`) and the corresponding Area Under the Curve (AUC) for
#' each of the DT models using (i) the training data set stored in the models
#' and (ii) a test data set (provided using `pressures` and `metrics`).
#'
#' @inheritParams build_DT
#'
#' @param smoothROC if TRUE, the ROC curve is passed to [smooth][pROC::smooth]
#'   to be smoothed.
#'
#' @return a list with two elements:
#'     - `AUC`: a data frame with the calculated AUC in both the training and
#'     test data sets;
#'     - `ROC`: a list of ggplot objects (one per DT model) containing the ROC
#'     curves
#'
#' @seealso [roc][pROC::roc] [build_DT][build_DT]
#'
#' @importFrom dplyr "%>%"
#' @export
calculate_DT_performance <- function(pathDT,
                                     metrics,
                                     pressures,
                                     low       = "low",
                                     impaired  = "impaired",
                                     smoothROC = TRUE) {

  . <- NULL

  modelList <- list.files(path       = pathDT,
                          pattern    = "model_*",
                          full.names = TRUE)

  pressureList <- list.files(path       = pathDT,
                             pattern    = "model_*",
                             full.names = FALSE) %>%
    gsub(., pattern     = "model_",
         replacement = "") %>%
    gsub(pattern     = ".rda",
         replacement = "")

    calc_model_performance <- function(i, low, impaired) {
      cat("\n", pressureList[i], "\n")

      DTunit <- NULL

      load(modelList[i])

      calc_roc <- function(pressures, metrics, p, low, impaired, smoothROC) {
        pressure <- NULL

        subData <- data.frame(pressure = pressures[[p]],
                              metrics)               %>%
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


        preds <- predict_DT(object      = DTunit,
                            newdata     = subData,
                            pred.all    = TRUE)$IP_all

        apply(preds,
              MARGIN = 2,
              function(j) {
                pROC::roc(response  = subData$pressure,
                          predictor = j,
                          smooth    = smoothROC)
              })
        }

      testRocs <- calc_roc(pressures = pressures,
                           metrics   = metrics,
                           p         = pressureList[i],
                           low       = low,
                           impaired  = impaired,
                           smoothROC = smoothROC)

      set <- NULL

      trainRocs <- dplyr::filter(DTunit$pred$data,
                                 set == "test") %>%
        split(x = ., f = as.factor(.$iter))     %>%
        lapply(function(df) {
          pROC::roc(response  = df$truth,
                    predictor = df$prob.impaired,
                    smooth    = smoothROC)
        })

      summarise_auc <- function(x) {
        paste0(round(mean(x), 3),
               " (+/- ",
               round(stats::sd(x), 3),
               ")")
      }

      aucs <- sapply(testRocs, '[[', "auc") %>%
        summarise_auc()                     %>%
        t()                                 %>%
        data.frame(pressureList[i],
              summarise_auc(DTunit$measures.test$auc) %>%
                t(),
              .,
              stringsAsFactors = FALSE)

      colnames(aucs) <- c("pressure",
                          "AUC training",
                          "AUC test")

      ROCurves <- plot_roc(rocobj = trainRocs, rocobj2 = testRocs,
                           rocNames = c("training", "test")) +
        ggplot2::scale_colour_discrete(name   = "Data sets") +
        ggplot2::scale_fill_discrete(name = "Data sets")     +
        ggplot2::ggtitle(pressureList[i])

      return(list(AUC = aucs, ROC = ROCurves))

    }

    modelPerformances <- lapply(1:length(modelList),
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
