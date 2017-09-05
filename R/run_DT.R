#' @importFrom dplyr "%>%"
#' @export
run_DT <- function(modelPath, data, uncertainty = FALSE) {
  modelList <- list.files(path       = modelPath,
                          pattern    = "model_*",
                          full.names = TRUE)

  pressureList <- gsub(modelList,
                       pattern     = paste0(modelPath,
                                            "model_"),
                       replacement = "") %>%
    gsub(pattern     = ".rda",
         replacement = "")

  if (!uncertainty) {
    preds <- pbapply::pblapply(modelList,
                               function(p) {
                                 load(p)
                                 predict_DT(object      = DTunit,
                                            newdata     = data,
                                            uncertainty = FALSE)
                               })                       %>%
      do.call(what = cbind)                  %>%
      data.frame(row.names = rownames(data)) %>%
      data.table::data.table(keep.rownames = TRUE)
    colnames(preds) <- c("ID", pressureList)
  } else {
    preds <-  pbapply::pblapply(modelList,
                                function(p) {
                                  load(p)
                                  predict_DT(object      = DTunit,
                                             newdata     = data,
                                             uncertainty = TRUE)
                                })
    names(preds) <- pressureList

    extractPreds <- function(preds, colName) {
      preds %>%
        (function(predList) {
          newPreds <- lapply(preds,
                             function(df) {
                               df[, colName]
                             }) %>%
            do.call(what = cbind)                  %>%
            data.frame(row.names = rownames(data)) %>%
            data.table::data.table(keep.rownames = TRUE)
          colnames(newPreds) <- c("ID", pressureList)

          newPreds
    })
    }


    preds <- lapply(c("5%", "avg", "95%"),
                    extractPreds,
                    preds = preds)
    names(preds) <- c("5%", "avg", "95%")

  }

  return(preds)
  }
