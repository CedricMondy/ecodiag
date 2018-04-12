#' @importFrom dplyr "%>%"
#' @export
run_DT <- function(modelPath, data) {
  modelList <- list.files(path       = modelPath,
                          pattern    = "model_*",
                          full.names = TRUE)

  pressureList <- gsub(modelList,
                       pattern     = paste0(modelPath,
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
                               newdata     = data,
                               pred.all    = FALSE)
                    })    %>%
    do.call(what = cbind) %>%
    data.frame(ID = rownames(data), .)

  colnames(preds) <- c("ID", pressureList)

  return(preds)
  }
