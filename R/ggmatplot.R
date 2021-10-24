#' ggmatplot
#'
#'
#' `ggmatplot` is a quick and easy way of plotting the columns of two matrices
#' against each other.
#'
#' @param x,y Vectors or matrices of data for plotting.
#'
#' * The number of rows of `x` and `y` should be the same.
#' * Missing values (NAs) are allowed.
#' * If either `x` or `y` is missing, the other is used as `y` and a vector of
#' `1:n` is used as `x`.
#'
#' @param color,fill Vectors of colors. Defining only one of them will update
#' both `color` and `fill` aesthetics of the plot by default, unless they are
#' both defined simultaneously.
#'
#' * The number of colors should match the higher number of columns of
#' matrices `x` or `y`, and will correspond to each of those columns.
#' * If only a single color is given, the same color will be used for all columns.
#'
#' @param shape,linetype A vector of shapes or line types respectively.
#'
#' * The number of shapes/line types should match the higher number of columns of
#' matrices `x` or `y`, and will correspond to each of those columns.
#' * If only a single shape/line type is given, the same shape/line type will be
#' used for all columns.
#'
#' @param xlim,ylim Ranges of x and y axes. Each of them should be a two element
#' vector specifying the lower and upper limits of the scale. If the larger
#' value is given first, the scale will be reversed. If one of the limits is
#' given as `NA`, the corresponding limit from the range of data will be used.
#'
#' @param log A string defining which axes to transform into a log scale.
#' (`x`, `y` or `xy`)
#'
#' @param main,xlab,ylab,legend_title Strings to update plot title, x axis label,
#'  y axis label and legend title respectively.
#'
#' @param legend_label A list of strings, to rename the legend labels.
#'
#' @param plot_type A string specifying the type of plot to draw. Possible plot
#' types are `point`, `line`, `both`(point + line), `density`, `histogram`,
#' `boxplot`, `dotplot`, `errorplot`, `violin`, and `ecdf`. Default plot_type is
#'  `point`.
#'
#' @param asp The y/x aspect ratio.
#'
#' @param ... Other arguments passed on to the plot. They can be used to set an
#' aesthetic to a fixed value, like `alpha = 0.4` or `size = 2`. They may also
#' be parameters passed on to the corresponding [geoms](https://ggplot2.tidyverse.org/reference/).
#'
#' @import ggplot2
#' @export
#' @md
#'
#' @examples
#'
#' # Define a data set
#' iris_sub <- subset(iris, Species == "setosa")
#' ggmatplot(iris_sub[, c(1, 3)], iris_sub[, c(2, 4)])
#' # Modify legend label and axis
#' ggmatplot(iris_sub[, c(1, 3)], iris_sub[, c(2, 4)], shape = c(4, 6), legend_label = c("Sepal", "Petal"), legend_title = "", xlab = "Length", ylab = "Width")
ggmatplot <- function(x, y, color = NULL, shape = NULL, linetype = NULL,
                      fill = NULL, xlim = c(NA, NA), ylim = c(NA, NA),
                      log = NULL, main = NULL, xlab = NULL, ylab = NULL,
                      legend_label = NULL, legend_title = NULL,
                      plot_type = "point", asp = NA, ...) {

  # valid plot types
  if (!plot_type %in% c(
    "point", "line", "both", "density", "histogram",
    "boxplot", "dotplot", "errorplot", "violin", "ecdf"
  )) {
    stop("plot_type can not take this value", call. = FALSE)
  }
  if (!missing(x) & !missing(y)) {
    # only single matrix input allowed for the following plot types
    if (plot_type %in% c("density", "histogram", "boxplot", "violin")) {
      stop("This plot type only uses a single matrix input", call. = FALSE)
    }
    data.list <- matclean(x = x, y = y)
  } else if (!missing(x)) {
    data.list <- matclean(x = x)
  } else {
    stop("x can not be missing", call. = FALSE)
  }

  params <- list(...)

  data <- data.list$data
  xname <- data.list$xname
  yname <- data.list$yname

  p <- ggplot(
    data = data,
    mapping = aes(
      group = Group,
      fill = Group,
      color = Group
    )
  )

  if (plot_type == "point") {
    p <- p +
      do.call(
        "geom_point",
        c(
          list(mapping = aes(
            x = .data[[xname]],
            y = .data[[yname]],
            shape = Group
          )),
          params
        )
      )
  } else if (plot_type == "line") {
    p <- p +
      do.call(
        "geom_line",
        c(
          list(mapping = aes(
            x = .data[[xname]],
            y = .data[[yname]],
            linetype = Group
          )),
          params
        )
      )
  } else if (plot_type == "both") {
    p <- p +
      do.call(
        "geom_point",
        c(
          list(mapping = aes(
            x = .data[[xname]],
            y = .data[[yname]],
            shape = Group
          )),
          params
        )
      ) +
      do.call(
        "geom_line",
        c(
          list(mapping = aes(
            x = .data[[xname]],
            y = .data[[yname]],
            linetype = Group
          )),
          params
        )
      )
  } else if (plot_type == "density") {
    params$alpha <- if (is.null(params$alpha)) 0.5 else params$alpha
    p <- p +
      do.call(
        "geom_density",
        c(
          list(mapping = aes(
            x = .data[[yname]]
          )),
          params
        )
      )
  } else if (plot_type == "histogram") {
    params$alpha <- if (is.null(params$alpha)) 0.5 else params$alpha
    p <- p +
      do.call(
        "geom_histogram",
        c(
          list(mapping = aes(
            x = .data[[yname]]
          )),
          params
        )
      )
  } else if (plot_type == "violin") {
    params$alpha <- if (is.null(params$alpha)) 0.5 else params$alpha
    p <- p +
      do.call(
        "geom_violin",
        c(
          list(mapping = aes(
            x = Group,
            y = .data[[yname]]
          )),
          params
        )
      )
  } else if (plot_type == "boxplot") {
    params$alpha <- if (is.null(params$alpha)) 0.5 else params$alpha
    p <- p +
      do.call(
        "geom_boxplot",
        c(
          list(mapping = aes(
            x = Group,
            y = .data[[yname]]
          )),
          params
        )
      )
  }

  # number of unique groups
  numGroups <- length(unique(data$Group))

  # default legend title
  if (is.null(legend_title)) legend_title <- "Group"

  if (!is.null(legend_label)) {
    # values > number of unique groups
    if (length(legend_label) > numGroups) {
      stop(paste0("Too many legend_label values. Only ", numGroups, " needed but " , length(legend_label), " provided."), call. = FALSE)
    }
    # values < number of unique groups
    else if (length(legend_label) < numGroups) {
      stop(paste0("Insufficient legend_label values. ", numGroups, " needed but only ", length(legend_label), " provided."), call. = FALSE)
    }
  } else {
    legend_label <- unique(data$Group)
  }

  if (!is.null(color)) {
    color <- validateNumParams(color, numGroups)
    p <- p + scale_color_manual(
      name = legend_title, labels = legend_label,
      values = color
    )
    if (is.null(fill)) {
      p <- p + scale_fill_manual(
        name = legend_title, labels = legend_label,
        values = color
      )
    }
  }

  if (!is.null(fill)) {
    fill <- validateNumParams(fill, numGroups)
    p <- p + scale_fill_manual(
      name = legend_title, labels = legend_label,
      values = fill
    )
    if (is.null(color)) {
      p <- p + scale_color_manual(
        name = legend_title, labels = legend_label,
        values = fill
      )
    }
  }

  if (is.null(color) & is.null(fill)) {
    p <- p +
      scale_fill_discrete(name = legend_title, labels = legend_label) +
      scale_color_discrete(name = legend_title, labels = legend_label)
  }

  if (!is.null(shape)) {
    # shape parameter is only valid for the following plot types
    if (plot_type %in% c("point", "both")) {
      shape <- validateNumParams(shape, numGroups)
    } else {
      warning(paste0("shape is an invalid parameter for plot type: ", plot_type), call. = FALSE)
    }
    p <- p + scale_shape_manual(
      name = legend_title, labels = legend_label,
      values = shape
    )
  } else {
    p <- p + scale_shape_discrete(name = legend_title, labels = legend_label)
  }

  if (!is.null(linetype)) {
    # linetype parameter is invalid for the following plot types
    if (!plot_type %in% c("point")) {
      linetype <- validateNumParams(linetype, numGroups)
    } else {
      warning(paste0("linetype is an invalid parameter for plot type: ", plot_type), call. = FALSE)
    }
    p <- p + scale_linetype_manual(
      name = legend_title, labels = legend_label,
      values = linetype
    )
  } else {
    p <- p + scale_linetype_discrete(name = legend_title, labels = legend_label)
  }

  if (!is.null(main)) p <- p + ggtitle(main)

  if (!is.null(log)) {
    # validating list of values the log parameter can take
    if (!log %in% c("x", "y", "xy")) {
      stop("invalid log value provided", call. = FALSE)
    } else {
      logv <- function(var) var %in% strsplit(log, "")[[1]]
      if (logv("x")) {
        p <- p + scale_x_log10() +
          xlab(paste0("log(", p$labels$x, ")"))
      }
      if (logv("y")) {
        p <- p + scale_y_log10() +
          ylab(paste0("log(", p$labels$y, ")"))
      }
    }
  }

  if (!is.null(xlab)) p <- p + xlab(xlab)
  if (!is.null(ylab)) p <- p + ylab(ylab)

  if (!missing(xlim)) p <- p + xlim(xlim)
  if (!missing(ylim)) p <- p + ylim(ylim)

  if (!is.na(asp)) p <- p + theme(aspect.ratio = asp)

  return(p)
}
