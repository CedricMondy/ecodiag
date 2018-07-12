#' @importFrom dplyr "%>%"
sample_sets <- function(x,
                        prop      = c(train     = 0.7,
                                      calibrate = 0.15,
                                      test      = 0.15)) {

  x_id <- 1:length(x)

  calibrate_id <- sample(x       = x_id,
                         size    = prop[2] * length(x),
                         replace = FALSE)

  if (length(prop) > 2) {
    test_id <- sample(x       = x_id[-calibrate_id],
                      size    = prop[3] * length(x),
                      replace = FALSE)
  } else {
    test_id <- NULL
  }


  train_id <- x_id[-c(calibrate_id, test_id)]

  x_summary <- data.frame(x                = x,
                          sample           = NA,
                          stringsAsFactors = FALSE)

  x_summary$sample[train_id]     <- "train"
  x_summary$sample[calibrate_id] <- "calibrate"
  x_summary$sample[test_id]      <- "test"

  return(x_summary)
}
