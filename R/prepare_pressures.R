#' Transform pressure data in quality classes or standardized intensity
#'
#' This function takes raw pressure parameters, transform them (in quality
#' classes or standardized intensity values) and if several parameters
#' correspond to a single pressure category, aggregate them.
#'
#' The parameters present in the pressureVar table but with no information in
#' the pressureInfo table will be discarded and a waning produced.
#'
#' The function compares the parameter values to the quality thresholds (`low`
#' and `impaired`) and depending on the `type` argument either:
#'     - `type = "class"`: allocate a quality class (low or impaired);
#'     - `type = "gradient"`: calculate the standardized intensity (I =
#'     log10(value/low) for parameters those values increase with stress
#'     intensity and I = log10(low/value) for parameters those values decrease
#'     with stress intensity)
#'
#' When `type = "class"`, an optional `impaired` columns can be added in the
#' pressureInfo table if the low and impaired classes are not adjacent.
#'
#' @param pressureVar a data frame with the information about sample code
#'   (`cd_opecont`), the parameter code (`cd_param`) and the measured value
#'   (`value`).
#'
#' @param pressureInfo a data frame with, for each pressure parameter in rows,
#'   information about:
#'       - the parameter code `cd_param`
#'       - the pressure `category` it corresponds to;
#'       - the response `direction`, i.e. either increasing stress is associated
#'       with `increasing`or `decreasing` parameter values;
#'       - the `low` boundary, i.e. the threshold delineating `low` and
#'       `impaired` quality classes but also the parameter value that will be
#'       used to standardized the intensities (if `type = "gradient"`).
#'       - the `impaired` boundary (optional, if not given `impaired = low`),
#'       only used when `type = "class"`.
#'
#' @param type character either `class` or `gradient`
#'
#' @return a data frame with the information about samples (`cd_opecont`),
#'   pressure category (`category`) and the pressure `intensity` (quality class
#'   or standardized intensity)
#'
#' @importFrom dplyr '%>%'
#' @export
prepare_pressures <- function(pressureVar,
                              pressureInfo,
                              type      = "class") {

  if (any(!(unique(pressureInfo$direction) %in%
            c("decreasing", "increasing", NA)))) {
    stop("\nThe direction should contain only the following levels:\n    increasing, decreasing")
  }

  if (any(!(c("cd_opecont", "cd_param", "value") %in%
            colnames(pressureVar)))) {
    stop("\nThe pressureVar table should contain all the following fields:\n    cd_opecont, cd_param, value")
  }

  if (any(!(c("category", "direction", "low", "condition") %in%
            colnames(pressureInfo)))) {
    stop("\nThe pressureInfo table should contain all the following fields:\n    category, direction, low and condition")
  }

  if (any(!(unique(pressureVar$cd_param) %in% pressureInfo$cd_param))) {
    warning("\nThe following pressures were not in the pressureInfo table and thus were not considered:\n    ",
            unique(pressureVar$cd_param)[!(unique(pressureVar$cd_param) %in% pressureInfo$cd_param)])
    pressureVar <- dplyr::filter(pressureVar, cd_param %in% pressureInfo$cd_param)

  }

  intensity <- matrix(NA,
                      nrow = dplyr::n_distinct(na.omit(pressureVar$cd_opecont)),
                      ncol = dplyr::n_distinct(na.omit(pressureInfo$category)),
                      dimnames = list(unique(na.omit(pressureVar$cd_opecont)),
                                      unique(na.omit(pressureInfo$category)))) %>%
    data.frame()

    maxClass <- function(x) {
      if (length(x[x %in% "impaired"]) > 0) {
        return("impaired")
      } else {
        return("low")
      }
    }

    for (p in unique(na.omit(pressureInfo$category))) {
      if (p %in% c("Acidification", "Micropolluants_mineraux")) {
        subInfo <- dplyr::filter(pressureInfo,
                                 category %in% p) %>%
          data.frame()

        if (!("impaired" %in% colnames(subInfo))) {
          subInfo$impaired <- subInfo$low
        }

        subVal  <- dplyr::filter(pressureVar,
                                 cd_param %in% subInfo$cd_param) %>%
          dplyr::mutate(category = p,
                        direction = NA,
                        low       = NA,
                        impaired  = NA,
                        intensity = NA)

        for (i in as.character(subInfo$cd_param)) {
          if (all(is.na(subInfo$condition[which(subInfo$cd_param %in% i)]))) {
            subVal[subVal$cd_param %in% i,
                   c("direction", "low", "impaired")] <-
              subInfo[subInfo$cd_param %in% i,
                      c("direction", "low", "impaired")]
          } else {
            if (i %in% "1370") {
              if (p == "Acidification") {
                sel_opecont <- dplyr::filter(subVal,
                                             cd_param %in% "1302" &
                                               value <= 6.5)$cd_opecont %>%
                  unique()

                subVal[with(subVal,
                            cd_param %in% i &
                              cd_opecont %in% sel_opecont),
                       c("direction", "low", "impaired")] <-
                  subInfo[subInfo$cd_param %in% i,
                          c("direction", "low", "impaired")]
              } else {
                sel_opecont <- filter(subVal,
                                      cd_param %in% "1302" &
                                        value > 6.5)$cd_opecont %>%
                  unique()

                subVal[with(subVal,
                            cd_param %in% i &
                              cd_opecont %in% sel_opecont),
                       c("direction", "low", "impaired")] <-
                  subInfo[subInfo$cd_param %in% i,
                          c("direction", "low", "impaired")]
              }
            } else {
              sel_opecont1 <- dplyr::filter(pressureVar,
                                            cd_param %in% "1345" &
                                              value <= 5)$cd_opecont %>%
                unique()
              sel_opecont2 <- dplyr::filter(pressureVar,
                                            cd_param %in% "1345" &
                                              value > 5 &
                                              value <= 20)$cd_opecont %>%
                unique()
              sel_opecont3 <- dplyr::filter(pressureVar,
                                            cd_param %in% "1345" &
                                              value > 20)$cd_opecont %>%
                unique()

              subVal[with(subVal,
                          cd_param %in% i &
                            cd_opecont %in% sel_opecont1),
                     c("direction", "low", "impaired")] <-
                subInfo[with(subInfo,
                             cd_param %in% i &
                               condition %in% "cond_mtx1"),
                        c("direction", "low", "impaired")]

              subVal[with(subVal,
                          cd_param %in% i &
                            cd_opecont %in% sel_opecont2),
                     c("direction", "low", "impaired")] <-
                subInfo[with(subInfo,
                             cd_param %in% i &
                               condition %in% "cond_mtx2"),
                        c("direction", "low", "impaired")]

              subVal[with(subVal,
                          cd_param %in% i &
                            cd_opecont %in% sel_opecont3),
                     c("direction", "low", "impaired")] <-
                subInfo[with(subInfo,
                             cd_param %in% i &
                               condition %in% "cond_mtx3"),
                        c("direction", "low", "impaired")]
            }
          }
        }
      } else {
        subInfo <- dplyr::filter(pressureInfo,
                                 category %in% p) %>%
          data.frame()
        rownames(subInfo) <- subInfo$cd_param

        if (!("impaired" %in% colnames(subInfo))) {
          subInfo$impaired <- subInfo$low
        }

        subVal  <- dplyr::filter(pressureVar,
                                 cd_param %in% subInfo$cd_param)
        subVal <- data.frame(subVal,
                             subInfo[as.character(subVal$cd_param),
                                     c("category", "direction",
                                       "low", "impaired")],
                             intensity = NA)
      }
      subVal <- dplyr::filter(subVal, !is.na(low))

      if (type == "gradient") {
        subVal$intensity[subVal$direction %in% "increasing"] <-
          with(subVal[subVal$direction %in% "increasing",],
               log10(value/low))

        subVal$intensity[subVal$direction %in% "decreasing"] <-
          with(subVal[subVal$direction %in% "decreasing",],
               log10(low/value))

        subIntensity <- dplyr::group_by(subVal,
                                        cd_opecont,
                                        category) %>%
          dplyr::summarise(intensity = max(intensity, na.rm = TRUE)) %>%
          reshape2::dcast(cd_opecont ~ category,
                          value.var = "intensity", fill = NA)
      } else {
        if (type == "class") {
          subVal[with(subVal,
                      direction %in% "increasing" &
                        value <= low), "intensity"]     <- "low"
          subVal[with(subVal,
                      direction %in% "increasing" &
                        value > impaired), "intensity"] <- "impaired"
          subVal[with(subVal,
                      direction %in% "decreasing" &
                        value >= low), "intensity"]     <- "low"
          subVal[with(subVal,
                      direction %in% "decreasing" &
                        value < impaired), "intensity"] <- "impaired"

          subIntensity <- dplyr::group_by(subVal,
                                          cd_opecont,
                                          category) %>%
            dplyr::summarise(intensity = maxClass(intensity)) %>%
            reshape2::dcast(cd_opecont ~ category,
                            value.var = "intensity", fill = NA)
        } else {
          stop("\ntype should be either 'gradient' or 'class'")
        }
      }
      intensity[subIntensity$cd_opecont, p] <- subIntensity[, p]
    }
    intensity$cd_opecont <- rownames(intensity)
    intensity <- reshape2::melt(intensity,
                                id.vars       = "cd_opecont",
                                variable.name = "category",
                                value.name    = "intensity") %>%
      dplyr::filter(!is.na(intensity))
    return(intensity)
}
