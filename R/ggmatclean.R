#' Function to convert two matrices into a wide format dataframe
#'
#' @param x,y vector or matrices of data. The number of rows should match. If one of them are missing, the other is taken as y and an x vector of 1:n is used.
#'
#' @return a list containing the long format dataframe, number and names of the id columns, number and names of the pivoted columns (column with group names and values)
#' @NoRd
#'
#' @examples
#' # Define x and y matrices
#' iris_sub <- subset(iris, Species == "setosa")[1:4]
#' x <- iris_sub[, 1:2]
#' y <- iris_sub[, 3:4]
#' # Use the defined x and y matrices as parameters
#' ggmatclean(x, y)
ggmatclean <- function(x, y) {
  x <- data.frame(x)
  ncolx <- ncol(x)

  if (!missing(y)) {
    y <- data.frame(y)
    ncoly <- ncol(y)
  } else {
    ncoly <- 1
  }

  if (missing(y)) {
    xname <- "Observation_number"
    yname <- "y"
    group <- "Group"
    x$Observation_number <- 1:nrow(x)
    data <- ggpivotlonger(x, names_to = group, values_to = yname, ncolx + 1)
  } else if (ncolx > ncoly & ncoly == 1) {
    xname <- "x"
    yname <- colnames(y)
    group <- "Group"
    data <- data.frame(x, y)
    ncol <- ncol(data)
    yname <- colnames(data)[ncol]
    data$Observation_number <- 1:nrow(data)
    data <- ggpivotlonger(data, names_to = group, values_to = xname, c(ncol, (ncol + 1)))
  } else if (ncoly > ncolx & ncolx == 1) {
    xname <- colnames(x)
    yname <- "y"
    group <- "Group"
    data <- data.frame(x, y)
    ncol <- ncol(data)
    data$Observation_number <- 1:nrow(data)
    data <- ggpivotlonger(data, names_to = group, values_to = yname, c(1, (ncol + 1)))
  } else if (ncolx == ncoly) {
    xname <- "x"
    yname <- "y"
    group <- "Group"
    colnames(x) <- colnames(y) <- paste0("Column ", 1:ncolx)
    x <- ggpivotlonger(x, names_to = group, values_to = xname)
    x <- x[with(x, order(Group)), ]
    x$Observation_number <- 1:nrow(x)
    y <- ggpivotlonger(y, names_to = group, values_to = yname)
    y <- y[with(y, order(Group)), ]
    y$Observation_number <- 1:nrow(y)
    data <- merge(x, y, by = "Observation_number", all = TRUE)
    names(data)[names(data) == paste0(group, ".x")] <- group
    data <- subset(data, select = -c(get(paste0(group, ".y"))))
  } else {
    stop("`x`` and `y` must have only 1 or the same number of columns", call. = FALSE)
  }

  return(list(data = data, xname = xname, yname = yname, group = group, ncolx = ncolx, ncoly = ncoly))
}
