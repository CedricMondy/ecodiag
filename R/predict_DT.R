#' @import mlr
predict_DT <- function(object,
                       newdata,
                       pred.all = TRUE) {

  IP_all <- pbapply::pblapply(object$models,
                              mlr:::predict.WrappedModel,
                              newdata = newdata) %>%
    lapply(function(pred) {
      pred$data$prob.impaired
    })                                           %>%
    do.call(what = cbind)                        %>%
    data.frame()                                 %>%
    dplyr::as.tbl()
  colnames(IP_all) <- paste("iter", 1:length(object$models), sep = "_")

  IP_summ <- apply(IP_all, 1, mean) %>%
    data.frame()                    %>%
    dplyr::as.tbl()
  colnames(IP_summ) <- "average"

  if (pred.all) {
    return(list(IP_all = IP_all, IP_summ = IP_summ))
  } else {
    return(IP_summ)
  }
}
