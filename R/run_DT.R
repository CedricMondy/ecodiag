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

  preds <- pbapply::pblapply(modelList,
                  function(p) {
                    load(p)
                    predict_DT(object  = DTunit,
                               newdata = data)
                  })                       %>%
    do.call(what = cbind)                  %>%
    data.frame(row.names = rownames(data)) %>%
    data.table::data.table(keep.rownames = TRUE)
  colnames(preds) <- c("ID", pressureList)

  return(preds)
  }
