#' Run a Diagnostic Tool
#'
#' This function run a diagnostic tool DT model built using the
#' [build_DT][build_DT] function on new data.
#'
#' The function takes the path where the DT model has been saved and the new
#' data as inputs. Then, the averaged predictions for each of the DT unit (unit
#' of a DT model corresponding to a given pressure) are computed and the
#' predictions of the different DT units are gathered.
#'
#' @inheritParams build_DT
#'
#' @param newdata a data frame with samples in rows and biological metrics in
#'   columns
#'
#' @return a data frame with the samples from `newdata` in rows and the
#'   corresponding predictions for the different pressures included in the DT
#'   model in columns.

#' @seealso [build_DT][build_DT]
#'
#' @importFrom dplyr "%>%"
#' @export
run_DT <- function(pathDT, newdata) {
  . <- NULL
  modelList <- list.files(path       = pathDT,
                          pattern    = "model_*",
                          full.names = TRUE)

  pressureList <- gsub(modelList,
                       pattern     = paste0(pathDT,
                                            "model_"),
                       replacement = "") %>%
    gsub(pattern     = ".rda",
         replacement = "")

  preds <- lapply(1:length(modelList),
                  function(i) {
                    cat("\n", pressureList[i], "\n")
                    DTunit <- NULL
                    load(modelList[i])
                    predict_DT(object      = DTunit,
                               newdata     = newdata,
                               pred.all    = FALSE)
                    })    %>%
    do.call(what = cbind) %>%
    data.frame(ID = rownames(newdata), .)

  colnames(preds) <- c("ID", pressureList)

  return(preds)
  }
