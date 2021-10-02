#' Quick matplot function
#'
#' `ggmatplot` is a quick way to make a simplier and similar plot as what `matplot` does but with a ggplot version.
#'
#' @param x,y vectors or matrices of data for plotting. The number of rows should match. If one of them are missing, the other is taken as y and an x vector of 1:n is used. Missing values (NAs) are allowed.
#' @param color vector of colors and they are used cyclically.
#' @param shape vector of shapes and they are used cyclically.
#' @param linetype vector of linetypes colors  and they are used cyclically.
#' @param xlim,ylim ranges of x and y axes, as in [plot()].
#' @param log Which variables to log transform ("x", "y", or "xy")
#' @param main,xlab,ylab Character vector giving plot title,
#'   x axis label, and y axis label respectively.
#' @param legend_label Character vector giving legend tables for different groups.
#' @param legend_title Character giving legend title
#' @param geom Character vector specifying geom(s) to draw. Defaults to
#'  "point". Other options are "line" or c("point","line").
#' @param asp The y/x aspect ratio
#' @import ggplot2
#' @export
#'
#' @examples
#' # Define a data set
#' iris_sub <- subset(iris, Species == "setosa")
#' ggmatplot(iris_sub[, c(1, 3)], iris_sub[, c(2, 4)])
#' # Modify legend label and axis
#' ggmatplot(iris_sub[, c(1, 3)], iris_sub[, c(2, 4)], shape = c("s", "S"), legend_label = c("Sepal", "Petal"), legend_title = "", xlab = "Length", ylab = "Width")
ggmatplot <- function(x, y, color = NULL, shape = NULL, linetype = NULL, fill = NULL,
                      xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                      main = NULL, xlab = NULL, ylab = NULL,
                      legend_label = NULL, legend_title = NULL,
                      plot.type = "point", asp = NA, ...) {
  if (!plot.type %in% c("point", "line", "both", "density", "histogram", "boxplot", "dotplot", "errorplot", "violin", "ecdf")) {
    stop("`plot.type` can not take this value", call. = FALSE)
  }
  if (!missing(x) & !missing(y)) {
    if (plot.type %in% c("density", "histogram", "boxplot", "violin")) {
      stop("This plot type only uses a single matrix input", call. = FALSE)
    }
    data.list <- matclean(x = x, y = y)
  } else if (!missing(x)) {
    data.list <- matclean(x = x)
  } else {
    stop("`x` can not be missing", call. = FALSE)
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


  if (plot.type == "point") {
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
  } else if (plot.type == "line") {
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
  } else if (plot.type == "both") {
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
  } else if (plot.type == "density") {
    params$alpha <- if(is.null(params$alpha)) 0.5 else params$alpha
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
  } else if (plot.type == "histogram") {
    params$alpha <- if(is.null(params$alpha)) 0.5 else params$alpha
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
  } else if (plot.type == "violin") {
    params$alpha <- if(is.null(params$alpha)) 0.5 else params$alpha
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
  } else if (plot.type == "boxplot") {
    params$alpha <- if(is.null(params$alpha)) 0.5 else params$alpha
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

  if (is.null(legend_title)) legend_title <- "Group"

  if (!is.null(legend_label)) {
    # values > number of unique groups
    if (length(legend_label) > numGroups) {
      stop(paste0("Too many legend_label values. Only ", numGroups, " needed but ", length(legend_label), " provided."), call. = FALSE)
    }
    # values < number of unique groups
    else if (length(legend_label) < numGroups) {
      stop(paste0("Insufficient legend_label values. ", numGroups, " needed but only ", length(legend_label), " provided."), call. = FALSE)
    }
  } else {
    legend_label <- unique(data$Group)
  }

  if (!is.null(color)) {
    color <- numParameterHandler(color, numGroups)
    p <- p + scale_color_manual(name = legend_title, labels = legend_label, values = color)
    if (is.null(fill)) {
      p <- p + scale_fill_manual(name = legend_title, labels = legend_label, values = color)
    }
  }

  if (!is.null(fill)) {
    fill <- numParameterHandler(fill, numGroups)
    p <- p + scale_fill_manual(name = legend_title, labels = legend_label, values = fill)
    if (is.null(color)) {
      p <- p + scale_color_manual(name = legend_title, labels = legend_label, values = fill)
    }
  }

  if (is.null(color) & is.null(fill)) {
    p <- p +
      scale_fill_discrete(name = legend_title, labels = legend_label) +
      scale_color_discrete(name = legend_title, labels = legend_label)
  }

  if (!is.null(shape)) {
    shape <- numParameterHandler(shape, numGroups)
    p <- p + scale_shape_manual(name = legend_title, labels = legend_label, values = shape)
  } else {
    p <- p + scale_shape_discrete(name = legend_title, labels = legend_label)
  }

  if (!is.null(linetype)) {
    linetype <- numParameterHandler(linetype, numGroups)
    p <- p + scale_linetype_manual(name = legend_title, labels = legend_label, values = linetype)
  } else {
    p <- p + scale_linetype_discrete(name = legend_title, labels = legend_label)
  }

  if (!is.null(main)) p <- p + ggtitle(main)

  logv <- function(var) var %in% strsplit(log, "")[[1]]
  if (logv("x")) p <- p + scale_x_log10() + xlab(paste0("log(", p$labels$x, ")"))
  if (logv("y")) p <- p + scale_y_log10() + ylab(paste0("log(", p$labels$y, ")"))

  if (!is.null(xlab)) p <- p + xlab(xlab)
  if (!is.null(ylab)) p <- p + ylab(ylab)

  if (!missing(xlim)) p <- p + xlim(xlim)
  if (!missing(ylim)) p <- p + ylim(ylim)

  if (!is.na(asp)) p <- p + theme(aspect.ratio = asp)

  return(p)
}
