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
