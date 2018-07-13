#' Plot the predictions of a DT model
#'
#' This function is a wrapper around the
#' [ggradar](https://github.com/ricardo-bion/ggradar/blob/master/R/ggradar.R)
#' function and produces radar plot for the DT model predictions.
#'
#' @param plot.data dataframe comprising samples in rows and DT model
#'   predictions in columns (as returned by [run_DT][run_DT])
#' @param axis.labels names of axis labels if other than column names supplied
#'   via plot.data
#' @param grid.line.width,gridline.min.colour,gridline.mid.colour,gridline.max.colour
#'   controls for the appearance of some aspects the 'minimum', 'average' and
#'   'maximum' grid lines.
#' @param axis.label.offset vertical displacement of axis labels from maximum
#'   grid line, measured relative to circle diameter
#' @param axis.line.colour colour of the axes
#' @param axis.label.size,grid.label.size,legend.text.size text size
#' @param group.line.width,group.point.size controls for the appearance of some
#'   aspects of the sample lines
#' @param background.circle.colour,background.circle.transparency controls for
#'   the appearance of background circle
#' @param plot.legend logical, does the legend be printed or not
#' @param legend.title,plot.title titles
#' @param byRow logical, should one radar plot be returned for each sample ID
#'
#'
#' @references script adapted from
#'   https://github.com/ricardo-bion/ggradar/blob/master/R/ggradar.R
#'
#' @importFrom dplyr "%>%"
#' @export
plot_DT <- function(plot.data,
                    axis.labels           = colnames(plot.data)[-1],
                    grid.line.width       = 0.5,
                    gridline.min.colour   = "grey",
                    gridline.mid.colour   = "#007A87",
                    gridline.max.colour   = "grey",
                    grid.label.size       = 7,
                    axis.label.offset     = 1.15,
                    axis.label.size       = 8,
                    axis.line.colour      = "grey",
                    group.line.width      = 1.5,
                    group.point.size      = 6,
                    background.circle.colour       = "#D7D6D1",
                    background.circle.transparency = 0.2,
                    plot.legend           = if (nrow(plot.data) > 1) TRUE else FALSE,
                    legend.title          = "",
                    plot.title            = "",
                    legend.text.size      = grid.label.size,
                    byRow                 = FALSE) {

  ID <- x <- y <- text <- axis.no <- NULL


  if (byRow) {
    IDs <- plot.data$ID

    plotList <- lapply(IDs,
           function(i) {
             subset(plot.data, ID == i)
           }) %>%
      lapply(function(x) {
        plot_DT(plot.data = x,
                axis.labels           = axis.labels,
                grid.line.width       = grid.line.width,
                gridline.min.colour   = gridline.min.colour,
                gridline.mid.colour   = gridline.mid.colour,
                gridline.max.colour   = gridline.max.colour,
                grid.label.size       = grid.label.size,
                axis.label.offset     = axis.label.offset,
                axis.label.size       = axis.label.size,
                axis.line.colour      = axis.line.colour,
                group.line.width      = group.line.width,
                group.point.size      = group.point.size,
                background.circle.colour       = background.circle.colour,
                background.circle.transparency = background.circle.transparency,
                plot.legend           = FALSE,
                legend.title          = "",
                plot.title            = "",
                legend.text.size      = legend.text.size,
                byRow                 = FALSE)})

    names(plotList) <- IDs

    return(plotList)
  } else {
    values.radar          <- c("0.0", "0.5", "1.0")
    grid.min              <- 0
    grid.mid              <- 0.5
    grid.max              <- 1
    centre.y              <- grid.min - ((1/9)*(grid.max - grid.min))
    plot.extent.x.sf      <- 1
    plot.extent.y.sf      <- 1.2
    x.centre.range        <- 0.02*(grid.max - centre.y)
    gridline.label.offset <- -0.1*(grid.max - centre.y)
    gridline.min.linetype <- "longdash"
    gridline.mid.linetype <- "longdash"
    gridline.max.linetype <- "longdash"

    plot.data     <- as.data.frame(plot.data)
    plot.data.min <- plot.data.max <- NULL

    plot.data[,1]       <- as.factor(as.character(plot.data[,1]))
    names(plot.data)[1] <- "group"

    var.names <- colnames(plot.data)[-1]  #'Short version of variable names
    #axis.labels [if supplied] is designed to hold 'long version' of variable
    #names with line-breaks indicated using \n

    #caclulate total plot extent as radius of outer circle x a user-specifiable
    #scaling factor
    plot.extent.x <- (grid.max + abs(centre.y))*plot.extent.x.sf
    plot.extent.y <- (grid.max + abs(centre.y))*plot.extent.y.sf

    #Check supplied data makes sense
    if (length(axis.labels) != ncol(plot.data) - 1)
      stop("'axis.labels' contains the wrong number of axis labels")
    if (min(plot.data[,-1]) < centre.y)
      stop("plot.data' contains value(s) < centre.y")
    if (max(plot.data[,-1]) > grid.max)
      stop("'plot.data' contains value(s) > grid.max")
    #Declare required internal functions

    CalculateGroupPath <- function(df) {
      #Converts variable values into a set of radial x-y coordinates
      #Code adapted from a solution posted by Tony M to
      #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
      #Args:
      #  df: Col 1 -  group ('unique' cluster / group ID of entity)
      #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or
      #      median) of variables v1 to v.n

      path <- as.factor(df[,1])

      ##find increment
      angles <- seq(from = 0, to = 2*pi, by = (2*pi)/(ncol(df) - 1))
      ##create graph data frame
      graphData <- data.frame(seg = "", x = 0, y = 0)
      graphData <- graphData[-1,]

      for (i in levels(path)) {
        pathData <- subset(df, df[,1] == i)
        for (j in c(2:ncol(df))) {
          #pathData[,j]= pathData[,j]


          graphData <- rbind(graphData,
                             data.frame(group = i,
                                        x     = pathData[,j]*sin(angles[j - 1]),
                                        y     = pathData[,j]*cos(angles[j - 1])))
        }
        ##complete the path by repeating first pair of coords in the path
        graphData <- rbind(graphData,
                           data.frame(group = i,
                                      x     = pathData[,2]*sin(angles[1]),
                                      y     = pathData[,2]*cos(angles[1])))

      }
      #Make sure that name of first column matches that of input data (in case
      #!="group")
      colnames(graphData)[1] <- colnames(df)[1]
      graphData #data frame returned by function
    }

    CaclulateAxisPath <- function(var.names,min,max) {
      #Caculates x-y coordinates for a set of radial axes (one per variable
      #being plotted in radar plot)
      #Args:
      #var.names - list of variables to be plotted on radar plot
      #min - MININUM value required for the plotted axes (same value will be
      #applied to all axes)
      #max - MAXIMUM value required for the plotted axes (same value will be
      #applied to all axes)
      #var.names <- c("v1","v2","v3","v4","v5")
      n.vars <- length(var.names) # number of vars (axes) required
      #Cacluate required number of angles (in radians)
      angles <- seq(from = 0, to = 2*pi, by = (2*pi)/n.vars)
      #calculate vectors of min and max x+y coords
      min.x <- min*sin(angles)
      min.y <- min*cos(angles)
      max.x <- max*sin(angles)
      max.y <- max*cos(angles)
      #Combine into a set of uniquely numbered paths (one per variable)
      axisData <- NULL
      for (i in 1:n.vars) {
        a <- c(i,min.x[i],min.y[i])
        b <- c(i,max.x[i],max.y[i])
        axisData <- rbind(axisData, a, b)
      }
      #Add column names + set row names = row no. to allow conversion into a
      #data frame
      colnames(axisData) <- c("axis.no","x","y")
      rownames(axisData) <- seq(1:nrow(axisData))
      #Return calculated axis paths
      as.data.frame(axisData)
    }

    funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
      #Adapted from Joran's response to
      #http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
      tt <- seq(0,2*pi,length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }

    ### Convert supplied data into plottable format
    #(a) add abs(centre.y) to supplied plot data [creates plot centroid of 0,0
    #for internal use, regardless of min. value of y in user-supplied data]
    plot.data.offset <- plot.data
    plot.data.offset[,2:ncol(plot.data.offset)] <-
      plot.data[,2:ncol(plot.data)] + abs(centre.y)

    #print(plot.data.offset)
    # (b) convert into radial coords
    group      <- NULL

    group$path <- CalculateGroupPath(plot.data.offset)

    #print(group$path)
    # (c) Calculate coordinates required to plot radial variable axes
    axis      <- NULL
    axis$path <- CaclulateAxisPath(var.names,
                                   grid.min + abs(centre.y),
                                   grid.max + abs(centre.y))
    #print(axis$path)
    # (d) Create file containing axis labels + associated plotting coordinates
    #Labels
    axis$label <- data.frame(
      text = axis.labels,
      x    = NA,
      y    = NA )
    #print(axis$label)
    #axis label coordinates
    n.vars <- length(var.names)
    angles <- seq(from = 0, to = 2*pi, by = (2*pi)/n.vars)
    axis$label$x <- sapply(1:n.vars,
                           function(i, x) {
                             ((grid.max + abs(centre.y))*axis.label.offset)*sin(angles[i])
                           })
    axis$label$y <- sapply(1:n.vars,
                           function(i, x) {
                             ((grid.max + abs(centre.y))*axis.label.offset)*cos(angles[i])
                           })
    #print(axis$label)
    # (e) Create Circular grid-lines + labels
    #caclulate the coordinates required to plot circular grid-lines for three
    #user-specified
    #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
    gridline <- NULL
    gridline$min$path <- funcCircleCoords(c(0,0),
                                          grid.min + abs(centre.y),
                                          npoints = 360)
    gridline$mid$path <- funcCircleCoords(c(0,0),
                                          grid.mid + abs(centre.y),
                                          npoints = 360)
    gridline$max$path <- funcCircleCoords(c(0,0),
                                          grid.max + abs(centre.y),
                                          npoints = 360)
    #print(head(gridline$max$path))
    #gridline labels
    gridline$min$label <- data.frame(x    = gridline.label.offset,
                                     y    = grid.min + abs(centre.y),
                                     text = as.character(grid.min))
    gridline$max$label <- data.frame(x    = gridline.label.offset,
                                     y    = grid.max + abs(centre.y),
                                     text = as.character(grid.max))
    gridline$mid$label <- data.frame(x    = gridline.label.offset,
                                     y    = grid.mid + abs(centre.y),
                                     text = as.character(grid.mid))
    #print(gridline$min$label)
    #print(gridline$max$label)
    #print(gridline$mid$label)
    ### Start building up the radar plot

    # Declare 'theme_clear', with or without a plot legend as required by user
    #[default = no legend if only 1 group [path] being plotted]
    theme_clear <- ggplot2::theme_bw(base_size = 20) +
      ggplot2::theme(axis.text.y      = ggplot2::element_blank(),
                     axis.text.x      = ggplot2::element_blank(),
                     axis.ticks       = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.border     = ggplot2::element_blank(),
                     legend.key       = ggplot2::element_rect(linetype = "blank"))



    #Base-layer = axis labels + plot extent
    # [need to declare plot extent as well, since the axis labels don't always
    # fit within the plot area automatically calculated by ggplot, even if all
    # included in first plot; and in any case the strategy followed here is to
    # first plot right-justified labels for axis labels to left of Y axis for x<
    # (-x.centre.range)], then centred labels for axis labels almost immediately
    # above/below x= 0 [abs(x) < x.centre.range]; then left-justified axis
    # labels to right of Y axis [x>0]. This building up the plot in layers
    # doesn't allow ggplot to correctly identify plot extent when plotting first
    # (base) layer]

    #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
    base <- ggplot2::ggplot(axis$label)                             +
      ggplot2::xlab(NULL)                                           +
      ggplot2::ylab(NULL)                                           +
      ggplot2::coord_equal()                                        +
      ggplot2::geom_text(data = subset(axis$label,axis$label$x < (-x.centre.range)),
                         ggplot2::aes(x = x, y = y, label = text),
                         size = axis.label.size, hjust = 1)         +
      ggplot2::scale_x_continuous(limits = c(-1.5*plot.extent.x,
                                             1.5*plot.extent.x))    +
      ggplot2::scale_y_continuous(limits = c(-plot.extent.y,
                                             plot.extent.y))        +
      ggplot2::geom_text(data = subset(axis$label,
                                       abs(axis$label$x) <= x.centre.range),
                         ggplot2::aes(x = x, y = y, label = text),
                         size = axis.label.size, hjust = 0.5)       +
      ggplot2::geom_text(data = subset(axis$label,
                                       axis$label$x > x.centre.range),
                         ggplot2::aes(x = x, y = y, label = text),
                         size = axis.label.size, hjust = 0)         +
      ggplot2::labs(colour = legend.title,
                    size   = legend.text.size)                      +
      ggplot2::geom_polygon(data  = gridline$max$path,
                            ggplot2::aes(x,y),
                            fill  = background.circle.colour,
                            alpha = background.circle.transparency) +
      ggplot2::geom_path(data = axis$path,
                         ggplot2::aes(x = x, y = y,
                                      group = axis.no),
                         colour = axis.line.colour)

    base <- base +
      ggplot2::geom_path(data = group$path,
                         ggplot2::aes(x = x, y = y,
                                      group = group, colour = group),
                         size = group.line.width)                   +
      ggplot2::geom_point(data = group$path,
                          ggplot2::aes(x = x, y = y,
                                       group = group, colour = group),
                          size = group.point.size)                  +
      ggplot2::geom_path(data   = gridline$min$path,
                         ggplot2::aes(x = x, y = y),
                         lty    = gridline.min.linetype,
                         colour = gridline.min.colour,
                         size   = grid.line.width)                  +
      ggplot2::geom_path(data   = gridline$mid$path,
                         ggplot2::aes(x = x, y = y),
                         lty    = gridline.mid.linetype,
                         colour = gridline.mid.colour,
                         size   = grid.line.width)                  +
      ggplot2::geom_path(data   = gridline$max$path,
                         ggplot2::aes(x = x, y = y),
                         lty    = gridline.max.linetype,
                         colour = gridline.max.colour,
                         size   = grid.line.width)                  +
      ggplot2::geom_text(data  = gridline$min$label,
                         ggplot2::aes(x = x, y = y,
                                      label = values.radar[1]),
                         size  = grid.label.size*0.8,
                         hjust = 1)                                 +
      ggplot2::geom_text(data  = gridline$mid$label,
                         ggplot2::aes(x = x, y = y,
                                      label = values.radar[2]),
                         size  = grid.label.size*0.8,
                         hjust = 1)                                 +
      ggplot2::geom_text(data  = gridline$max$label,
                         ggplot2::aes(x = x, y = y,
                                      label = values.radar[3]),
                         size  = grid.label.size*0.8,
                         hjust = 1)                                 +
      theme_clear                                                   +
      ggplot2::theme(legend.key.width = ggplot2::unit(3,"line"))    +
      ggplot2::theme(text = ggplot2::element_text(size = 20))       +
      ggplot2::theme(legend.text     = ggplot2::element_text(size = legend.text.size),
                     legend.position = "left")                      +
      ggplot2::theme(legend.key.height = ggplot2::unit(2,"line"))   +
      ggplot2::scale_colour_manual(values =
                                     rep(c("#FF5A5F", "#FFB400",
                                           "#007A87",  "#8CE071",
                                           "#7B0051", "#00D1C1",
                                           "#FFAA91", "#B4A76C",
                                           "#9CA299", "#565A5C",
                                           "#00A04B", "#E54C20"),
                                         100))

    if (!plot.legend) {
      base <- base +
        ggplot2::theme(legend.position = "none")
    }

    if (plot.title != "") {
      base <- base +
        ggplot2::ggtitle(plot.title)
    }

    return(base)
  }
}
