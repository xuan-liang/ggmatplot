#' ggmatplot
#'
#'
#' `ggmatplot` is a quick and easy way of plotting the columns of two matrices
#' or data frames against each other using
#' [`ggplot2`](https://ggplot2.tidyverse.org/).
#'
#' @param x,y Vectors or matrices of data.
#'
#' * The number of rows of `x` and `y` should be the same.
#' * Either `x` or `y` should be a vector, unless the number of columns of `x`
#' and `y` are the same.
#' * Missing values (NAs) are allowed.
#' * If either `x` or `y` is missing, the other is used as `y` and a vector of
#' `1:n` is used as `x`.
#'
#' @param plot_type A string specifying the type of plot. Possible plot types
#' are `point`, `line`, `both`(point + line), `density`, `histogram`, `boxplot`,
#' `dotplot`, `errorplot`, `violin`, and `ecdf`. Default plot_type is `point`.
#'
#' @param color,fill Vectors of colors. Defining only one of them will update
#' both `color` and `fill` aesthetics of the plot by default, unless they are
#' both defined simultaneously.
#'
#' * The number of colors should match the higher number of columns of
#' matrices `x` or `y`, and will correspond to each of those columns.
#' * If only a single color is given, the same color will be used for all
#' columns.
#'
#' @param main,xlab,ylab,legend_title Strings to update plot title, x axis
#' label, y axis label and legend title respectively.
#'
#' @param legend_label A vector of strings, to rename the legend labels.
#'
#' @param shape,linetype A vector of shapes or line types respectively.
#'
#' * The number of shapes/line types should match the higher number of columns
#' of matrices `x` or `y`, and will correspond to each of those columns.
#' * If only a single shape/line type is given, the same shape/line type will
#' be used for all columns.
#'
#' @param xlim,ylim Ranges of x and y axes.
#'
#' * Each of them should be a two element vector specifying the lower and upper
#' limits of the scale.
#' * If the larger value is given first, the scale will be reversed. If one of
#' the limits is given as `NA`, the corresponding limit from the range of data
#' will be used.
#'
#' @param log A string defining which axes to transform into a log scale.
#' (`x`, `y` or `xy`)
#'
#' @param asp The y/x aspect ratio.
#'
#' @param desc_stat Descriptive statistics to be used for visualizing errors,
#' in `errorplot`. Possible values are `mean_se`, `mean_sd`, `mean_range`,
#'  `median_iqr` and `median_range`. Default desc_stat is `mean_se`.
#'
#' @param ... Other arguments passed on to the plot. Possible arguments are
#' those that can be passed on to the [underlying ggplot layers](#plot-types).
#'
#' @return A ggplot object. The columns of the input matrices will be plotted
#' against each other using the defined plot type.
#'
#' @import ggplot2
#' @export
#' @md
#'
#' @section Plot Types:
#'
#' `ggmatplot`plots are built upon `ggplot2 layers`. The following is a list of
#' `ggmatplot` plot types, along with their underlying
#' [`ggplot geoms`](https://ggplot2.tidyverse.org/reference/index.html#section-geoms)
#' or [`stats`](https://ggplot2.tidyverse.org/reference/index.html#section-stats).
#'
#' * \strong{point} \code{\link[ggplot2]{geom_point}}
#' * \strong{line} \code{\link[ggplot2]{geom_line}}
#' * \strong{both} \code{\link[ggplot2]{geom_point}} +
#' \code{\link[ggplot2]{geom_line}}
#' * \strong{density} \code{\link[ggplot2]{geom_density}}
#' * \strong{histogram} \code{\link[ggplot2]{geom_histogram}}
#' * \strong{boxplot} \code{\link[ggplot2]{geom_boxplot}}
#' * \strong{dotplot} \code{\link[ggplot2]{geom_dotplot}}
#' * \strong{errorplot} \code{\link[ggplot2]{geom_pointrange}}
#' * \strong{violin} \code{\link[ggplot2]{geom_violin}}
#' * \strong{ecdf} \code{\link[ggplot2]{stat_ecdf}}
#'
#' @examples
#'
#' # Define a data set
#' iris_sub <- subset(iris, Species == "setosa")
#' ggmatplot(iris_sub[, c(1, 3)], iris_sub[, c(2, 4)])
#' # Modify legend label and axis
#' ggmatplot(iris_sub[, c(1, 3)], iris_sub[, c(2, 4)],
#'   shape = c(4, 6),
#'   legend_label = c("Sepal", "Petal"), legend_title = "",
#'   xlab = "Length", ylab = "Width"
#' )
ggmatplot <- function(x = NULL, y = NULL,
                      plot_type = c("point", "line", "both", "density", "histogram",
                                    "boxplot", "dotplot", "errorplot", "violin", "ecdf"),
                      color = NULL, fill = NULL,
                      shape = NULL, linetype = NULL, xlim = c(NA, NA),
                      ylim = c(NA, NA), log = NULL, main = NULL, xlab = NULL,
                      ylab = NULL, legend_label = NULL, legend_title = NULL,
                      desc_stat = "mean_se", asp = NA, ...) {

  # binding global variables to objects
  Variable <- ymin <- ymax <- NULL
  params_input <- list(...)

  plot_type <- match.arg(plot_type)
  validate_input(x, y, plot_type, desc_stat,
                 color, fill, shape, linetype, xlim, ylim,
                 log, asp)

  data_list <- construct_data_list(x, y)
  data <- get_data_for_plot(data_list, plot_type, desc_stat)
  params <- setup_params(params_input, plot_type)

  xname <- data_list$xname
  yname <- data_list$yname

  plot_geom <- function(geom, aes) {
    do.call(geom, c(list(mapping = aes), params))
  }

  p <- ggplot(data = data, mapping = aes_string(group = GROUP_NAME))
  p <- p + switch(plot_type,
                  "point" = plot_geom(geom_point,
                                      aes_string(x = xname,
                                                 y = yname,
                                                 shape = GROUP_NAME,
                                                 color = GROUP_NAME)),
                  "line" = plot_geom(geom_line,
                                     aes_string(x = xname,
                                                y = yname,
                                                linetype = GROUP_NAME,
                                                color = GROUP_NAME)),
                  "both" = list(plot_geom(geom_point,
                                          aes_string(x = xname,
                                                     y = yname,
                                                     shape = GROUP_NAME,
                                                     color = GROUP_NAME)),
                                plot_geom(geom_line,
                                          aes_string(x = xname,
                                                     y = yname,
                                                     linetype = GROUP_NAME,
                                                     color = GROUP_NAME))),
                  "density" = plot_geom(geom_density,
                                        aes_string(x = yname,
                                                   color = GROUP_NAME,
                                                   fill = GROUP_NAME)),
                  "histogram" = plot_geom(geom_histogram,
                                          aes_string(x = yname,
                                                     color = GROUP_NAME,
                                                     fill = GROUP_NAME)),
                  "violin" = plot_geom(geom_violin,
                                       aes_string(x = GROUP_NAME,
                                                  y = yname,
                                                  color = GROUP_NAME,
                                                  fill = GROUP_NAME)),
                  "boxplot" = plot_geom(geom_boxplot,
                                        aes_string(x = GROUP_NAME,
                                                   y = yname,
                                                   color = GROUP_NAME,
                                                   fill = GROUP_NAME)),
                  "dotplot" = plot_geom(geom_dotplot,
                                        aes_string(x = GROUP_NAME,
                                                   y = yname,
                                                   color = GROUP_NAME,
                                                   fill = GROUP_NAME)),
                  "ecdf" = plot_geom(stat_ecdf,
                                     aes_string(x = yname,
                                                color = GROUP_NAME)),
                  "errorplot" = plot_geom(geom_pointrange,
                                          aes_string(x = GROUP_NAME,
                                                     y = "y",
                                                     ymin = "ymin",
                                                     ymax = "ymax",
                                                     color = GROUP_NAME,
                                                     fill = GROUP_NAME))
                  )


  nvar <- length(unique(data[[GROUP_NAME]]))
  legend_title <- legend_title %||% GROUP_NAME
  legend_label <- validate_nparam(legend_label, nvar, strict = TRUE) %||% levels(data[[GROUP_NAME]])
  color <- validate_nparam(color, nvar)
  fill <- validate_nparam(fill, nvar)
  shape <- validate_nparam(shape, nvar)
  linetype <- validate_nparam(linetype, nvar)

  p <- update_legend_aes(p, plot_type, color, fill, shape, linetype, legend_title, legend_label, nvar)

  # labels
  if (!is.null(main)) p <- p + ggtitle(main)
  if (!is.null(xlab)) p <- p + xlab(xlab)
  if (!is.null(ylab)) p <- p + ylab(ylab)
  # scales
  if (!is.null(log)) {
    if (grepl("x", log)) {
        p <- p + scale_x_log10()
    }
    if (grepl("y", log)) {
        p <- p + scale_y_log10()
    }
  }
  if(!missing(xlim)) p <- p + xlim(xlim)
  if(!missing(ylim)) p <- p + ylim(ylim)
  if (!is.na(asp)) p <- p + theme(aspect.ratio = asp)

  return(p)
}


