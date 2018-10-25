#' @importFrom dplyr "%>%"
split_training_test <- function(data, frac, group = NULL, seed = 20181025){

  set.seed(seed = seed)

  data <- dplyr::filter(data, ! is.na(pressure))

  if (!is.null(group)) {
    if (length(group) != nrow(data))
      stop("The length of the group vector should be equal to the number of data observations")

    data <- dplyr::mutate(data, grouping = group)

    selection <- data                         %>%
      dplyr::select(pressure, grouping)       %>%
      dplyr::count(x = ., grouping, pressure) %>%
      dplyr::group_by(grouping)               %>%
      dplyr::filter(n == max(n))              %>%
      dplyr::group_by(pressure)               %>%
      dplyr::sample_frac(tbl = ., size = frac, replace = FALSE)

    training <- dplyr::filter(.data = data,
                              grouping %in% selection$grouping)   %>%
      select(-grouping)
    test     <- dplyr::filter(.data = data,
                              ! grouping %in% selection$grouping) %>%
      select(-grouping)

  } else {
    training <- dplyr::sample_frac(tbl = data, size = frac, replace = FALSE)
    test     <- dplyr::anti_join(x = data, y = training, by = colnames(data))
  }

  list(training = training, test = test)
}
