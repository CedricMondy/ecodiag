## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ---- include=FALSE------------------------------------------------------
#  library(tidyverse)
#  library(ecodiag)

## ----import-packages, eval=FALSE-----------------------------------------
#  library(tidyverse)
#  library(ecodiag)

## ----import-data---------------------------------------------------------
#  # Import the data
#  data(metrics)
#  data(pressures)
#  
#  # Define a learning and a test data sets
#  learning_set <- sample(x       = seq(nrow(metrics)),
#                         size    = round(0.632 * nrow(metrics)),
#                         replace = FALSE)
#  
#  test_set <- setdiff(x = seq(nrow(metrics)), y = learning_set)
#  
#  learning_metrics   <- metrics[learning_set, ]
#  learning_pressures <- pressures[learning_set, ]
#  
#  test_metrics   <- metrics[test_set, ]
#  test_pressures <- pressures[test_set, ]

## ----no-calibration, eval=FALSE------------------------------------------
#  build_DT(metrics   = learning_metrics,
#           pressures = learning_pressures,
#           low       = c("high", "good"),
#           impaired  = c("moderate", "poor", "bad"),
#           pathDT    = "models/",
#           params    = list(num.trees       = 500,
#                            mtry            = 25,
#                            sample.fraction = 0.632,
#                            min.node.size   = 25),
#           nIter     = 4
#           )

## ----calibration, eval=FALSE---------------------------------------------
#  build_DT(metrics   = learning_metrics,
#           pressures = learning_pressures,
#           low       = c("high", "good"),
#           impaired  = c("moderate", "poor", "bad"),
#           pathDT    = "calibrated_models/",
#           params    = list(
#             num.trees       = c(500, 750, 1000, 1500),
#             mtry            = c(5, 10, 25, 50),
#             sample.fraction = 0.632,
#             min.node.size   = 25),
#           CVfolds   = 5
#           )

## ----iterations-no-calibration-------------------------------------------
#  build_DT(metrics   = learning_metrics,
#           pressures = learning_pressures,
#           low       = c("high", "good"),
#           impaired  = c("moderate", "poor", "bad"),
#           pathDT    = "models/",
#           params    = list(num.trees       = 500,
#                            mtry            = 25,
#                            sample.fraction = 0.632,
#                            min.node.size   = 25),
#           nIter     = 4,
#           nCores    = 4
#           )

## ----evaluation----------------------------------------------------------
#  modelPerformances <-
#    calculate_DT_performance(pathDT    = "models/",
#                             metrics   = test_metrics,
#                             pressures = test_pressures,
#                             low       = c("high", "good"),
#                             impaired  = c("moderate", "poor", "bad"),
#                             smoothROC = TRUE
#                             )

## ----AUC-table-----------------------------------------------------------
#  modelPerformances$AUC

## ----multiplot-----------------------------------------------------------
#  # Multiple plot function
#  #
#  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#  # - cols:   Number of columns in layout
#  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#  #
#  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#  # then plot 1 will go in the upper left, 2 will go in the upper right, and
#  # 3 will go all the way across the bottom.
#  #
#  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#    library(grid)
#  
#    # Make a list from the ... arguments and plotlist
#    plots <- c(list(...), plotlist)
#  
#    numPlots = length(plots)
#  
#    # If layout is NULL, then use 'cols' to determine layout
#    if (is.null(layout)) {
#      # Make the panel
#      # ncol: Number of columns of plots
#      # nrow: Number of rows needed, calculated from # of cols
#      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#    }
#  
#   if (numPlots==1) {
#      print(plots[[1]])
#  
#    } else {
#      # Set up the page
#      grid.newpage()
#      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#  
#      # Make each plot, in the correct location
#      for (i in 1:numPlots) {
#        # Get the i,j matrix positions of the regions that contain this subplot
#        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#  
#        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                        layout.pos.col = matchidx$col))
#      }
#    }
#  }

## ----ROC, fig.width=7, fig.height=4--------------------------------------
#  multiplot(plotlist = modelPerformances$ROC, cols = 2)

## ----run-DT--------------------------------------------------------------
#  predictions <- run_DT(pathDT  = "models/",
#                        metrics = as.data.frame(test_metrics))

## ------------------------------------------------------------------------
#  as.tbl(predictions)

## ----plot-DT, fig.width=7------------------------------------------------
#  plot_DT(plot.data        = head(predictions),
#          plot.title       = "test DT model",
#          axis.label.size  = 3,
#          grid.label.size  = 6,
#          legend.title     = "observations",
#          legend.text.size = 12,
#          )

## ---- fig.width=7--------------------------------------------------------
#  plot_DT(plot.data        = head(predictions),
#          axis.label.size  = 3,
#          grid.label.size  = 6,
#          byRow            = TRUE)
#  

