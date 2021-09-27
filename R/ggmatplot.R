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
                      plot.type = "point", asp = NA) {
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
      geom_point(mapping = aes(
        x = .data[[xname]],
        y = .data[[yname]],
        shape = Group
      ))
  } else if (plot.type == "line") {
    p <- p +
      geom_line(mapping = aes(
        x = .data[[xname]],
        y = .data[[yname]],
        linetype = Group
      ))
  } else if (plot.type == "both") {
    p <- p +
      geom_point(mapping = aes(
        x = .data[[xname]],
        y = .data[[yname]],
        shape = Group
      )) +
      geom_line(mapping = aes(
        x = .data[[xname]],
        y = .data[[yname]],
        linetype = Group
      ))
  } else if (plot.type == "density") {
    p <- p +
      geom_density(mapping = aes(x = .data[[yname]]))
  } else if (plot.type == "histogram") {
    p <- p +
      geom_histogram(mapping = aes(x = .data[[yname]]))
  } else if (plot.type == "violin") {
    p <- p +
      geom_violin(mapping = aes(
        x = Group,
        y = .data[[yname]]
      ))
  } else if (plot.type == "boxplot") {
    p <- p +
      geom_boxplot(mapping = aes(
        x = Group,
        y = .data[[yname]]
      ))
  }

  # number of unique groups
  maxGroups <<- length(unique(data$Group))

  if (!is.null(color)) {
    color <- checkValidNumValues(color)
    p <- p + scale_color_manual(values = color)
    if(is.null(fill)) {
      p <- p + scale_fill_manual(values = color)
    }
  }

  if (!is.null(shape)) {
    shape <- checkValidNumValues(shape)
    p <- p + scale_shape_manual(values = shape)
  }

  if (!is.null(linetype)) {
    linetype <- checkValidNumValues(linetype)
    p <- p + scale_linetype_manual(values = linetype)
  }

  if (!is.null(fill)) {
    fill <- checkValidNumValues(fill)
    p <- p + scale_fill_manual(values = fill)
    if(is.null(color)) {
      p <- p + scale_color_manual(values = fill)
    }
  }

  if (!is.null(xlab)) {
    p <- p + xlab(xlab)
  }

  if (!is.null(ylab)) {
    p <- p + ylab(ylab)
  }


  return(p)
}


checkValidNumValues <- function(parameterValues){
  # same for all groups
  if (!maxGroups == 1 & length(parameterValues) == 1){
    return(rep(parameterValues,maxGroups))
  }
  # values > number of unique groups
  else if (length(parameterValues) > maxGroups) {
    stop(paste0("Too many ", substitute(parameterValues), " values. Only ", maxGroups, " needed but ", length(parameterValues), " provided."), call. = FALSE)
  }
  # values < number of unique groups
  else if (length(parameterValues) < maxGroups) {
    stop(paste0("Insufficient ", substitute(parameterValues), " values. ", maxGroups, " needed but only ", length(parameterValues), " provided."), call. = FALSE)
  }
  else
    return(parameterValues)
}


# ggmatplot <- function (x, y, color = NULL, shape = NULL, linetype = NULL,
#                        xlim = c(NA, NA), ylim = c(NA, NA), log = "",
#                        main = NULL, xlab = NULL, ylab = NULL,
#                        legend_label =  NULL, legend_title = NULL,
#                        geom = "point", asp = NA){
#
#   caller_env <- parent.frame()
#
#   if (!is.character(geom)) stop("`geom` must be a character vector", call. = FALSE)
#
#   if (!missing(x)){
#     ncolx <- ncol(as.data.frame(x))
#   } else{
#     ncolx=1
#   }
#
#   if (!missing(y)){
#     ncoly <- ncol(as.data.frame(y))
#   }else{
#     ncoly=1
#   }
#
#   if (missing(y) & ncolx == 1) {
#     x <- data.frame(x)
#     data <- mutate(x, Observation_number = 1:n())
#     old_namex <- colnames(data)[1]
#     data <- rename(data, new_namex = old_namex)
#     p <- qplot(x = Observation_number, y = new_namex, data = data, geom = geom) + ylab(old_namex) + xlab("Observation Number")
#   } else if (missing(y) & ncolx > 1) {
#     data <- mutate(x, Observation_number = 1:n()) %>% gather(key = "Column", value = "Value", -(ncolx+1))
#     p <- qplot(x = Observation_number, y = Value, data = data, color = Column, shape = Column, linetype = Column, geom = geom)  + xlab("Observation Number")
#   } else if (missing(x) & ncoly == 1) {
#     y <- data.frame(y)
#     data <- mutate(y, Observation_number = 1:n())
#     old_namey <- colnames(data)[1]
#     data <- rename(data, new_namey = old_namey)
#     p <- qplot(x = Observation_number, y = new_namey, data = data, geom = geom) + ylab(old_namey) + xlab("Observation Number")
#   } else if (missing(x) & ncoly > 1) {
#     data <- mutate(y, Observation_number = 1:n()) %>% gather(key = "Column", value = "Value", -(ncoly+1))
#     p <- qplot(x = Observation_number, y = Value, data = data, color = Column, shape = Column, linetype = Column, geom = geom)  + xlab("Observation Number")
#   } else if (ncolx == 1 & ncoly == 1) {
#     data <- data.frame(x,y)
#     old_namex <- colnames(data)[1]
#     old_namey <- colnames(data)[2]
#     data <- rename(data, new_namex = old_namex, new_namey = old_namey)
#     p <- qplot(x = new_namex, y = new_namey, data = data) + xlab(old_namex) +  ylab(old_namey)
#   } else if (ncolx == 1& ncoly > 1) {
#     data <- data.frame(x,y)
#     ncol <- ncol(data)
#     old_namex <- colnames(data)[1]
#     data <- mutate(data, Observation_number = 1:n()) %>% gather(key = "Column", value = "Value", -c(1,(ncol+1)))  %>% rename(new_namex = old_namex)
#     p <- qplot(x = new_namex, y = Value, data = data, color = Column, shape = Column, linetype = Column, geom = geom) + xlab(old_namex)
#   } else if (ncolx > 1 & ncoly == 1) {
#     data <- data.frame(x,y)
#     ncol <- ncol(data)
#     old_namey <- colnames(data)[ncol]
#     data <- mutate(data, Observation_number = 1:n()) %>% gather(key = "Column", value = "Value", -c(ncol,(ncol+1)))  %>% rename(new_namey = old_namey)
#     p <- qplot(x = Value, y = new_namey, data = data, color = Column, shape = Column, linetype = Column, geom = geom) + ylab(old_namey)
#   } else if (ncolx > 1 & ncoly > 1 & ncolx == ncoly) {
#     colnames(x) = colnames(y) = paste0("Column ", 1:ncolx)
#     x <- gather(x, key = "Group", value = "x") %>% mutate(Observation_number = 1:n())
#     y <- gather(y, key = "Group", value = "y") %>% mutate(Observation_number = 1:n())
#     data <- full_join(x, y, by = "Observation_number") %>% rename(Column = Group.x)
#     p <- qplot(x = x, y = y, data = data, color = Column, shape = Column, linetype = Column, geom = geom)
#   } else{
#     stop("`x`` and `y` must have only 1 or the same number of columns", call. = FALSE)
#   }
#
#   maxcol= max(ncolx,ncoly)
#   maxshape = maxcol
#   maxlinetyoe = maxcol
#   if(!is.null(color) & length(color) != maxcol){
#     color=rep(color,maxcol)[1:maxcol]
#   }
#
#   if(!is.null(shape) & length(shape) != maxcol){
#     shape=rep(shape,maxcol)[1:maxcol]
#   }
#
#   if(!is.null(linetype) & length(linetype) != maxcol){
#     linetype=rep(shape,maxcol)[1:maxcol]
#   }
#
#   if(is.null(legend_title)){
#     legend_title = ""
#   }
#   if(is.null(legend_label)){
#     legend_label = unique(data$Column)
#   }
#
#   if (!is.null(color))
#     p <- p + scale_color_manual(name = legend_title, labels = legend_label, values = color)
#   if (is.null(color))
#     p <- p + scale_color_discrete(name = legend_title, labels = legend_label)
#
#   if (!is.null(shape))
#     p <- p + scale_shape_manual(name = legend_title, labels = legend_label, values = shape)
#   if (is.null(shape))
#     p <- p + scale_shape_discrete(name = legend_title, labels = legend_label)
#
#   if (!is.null(linetype))
#     p <- p + scale_linetype_manual(name = legend_title, labels = legend_label, values = linetype)
#   if (is.null(linetype))
#     p <- p + scale_linetype_discrete(name = legend_title, labels = legend_label)
#
#   if (!is.null(main)) p <- p + ggtitle(main)
#
#   if (!missing(xlab)) p <- p + xlab(xlab)
#   if (!missing(ylab)) p <- p + ylab(ylab)
#
#   if (!missing(xlim)) p <- p + xlim(xlim)
#   if (!missing(ylim)) p <- p + ylim(ylim)
#
#   logv <- function(var) var %in% strsplit(log, "")[[1]]
#
#   if (logv("x")) p <- p + scale_x_log10()
#   if (logv("y")) p <- p + scale_y_log10()
#
#   if (!is.na(asp)) p <- p + theme(aspect.ratio = asp)
#
#   p
# }
#
