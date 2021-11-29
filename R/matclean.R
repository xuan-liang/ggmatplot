#' Function to convert two matrices into a wide format data frame
#'
#' @param x,y Vectors or matrices of data. The number of rows of x and y should
#' match. If one of them are missing, the other is taken as y and an x vector
#' of 1:n is used.
#'
#' @return A list containing:
#'  * data - the long format data frame.
#'  * xname - name(s) of the ID column(s)
#'  * yname - names of the pivoted columns(columns with group names and values)
#' @noRd
#'
#' @examples
#' # Define x and y matrices
#' iris_sub <- subset(iris, Species == "setosa")[1:4]
#' x <- iris_sub[, 1:2]
#' y <- iris_sub[, 3:4]
#' # Use the defined x and y matrices as parameters
#' matclean(x, y)
matclean <- function(x, y) {
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
    yname <- "x"
    x$Observation_number <- 1:nrow(x)
    data <- pivotlonger(x, names_to = "Group", values_to = yname, ncolx + 1)
  } else if (ncolx > ncoly & ncoly == 1) {
    xname <- "x"
    yname <- colnames(y)
    data <- data.frame(x, y)
    ncol <- ncol(data)
    yname <- colnames(data)[ncol]
    data$Observation_number <- 1:nrow(data)
    data <- pivotlonger(data,
      names_to = "Group", values_to = xname,
      c(ncol, (ncol + 1))
    )
  } else if (ncoly > ncolx & ncolx == 1) {
    xname <- colnames(x)
    yname <- "y"
    data <- data.frame(x, y)
    ncol <- ncol(data)
    data$Observation_number <- 1:nrow(data)
    data <- pivotlonger(data,
      names_to = "Group", values_to = yname,
      c(1, (ncol + 1))
    )
  } else if (ncolx == ncoly) {
    xname <- "x"
    yname <- "y"
    colnames(x) <- colnames(y) <- paste0("Column ", 1:ncolx)
    x <- pivotlonger(x, names_to = "Group", values_to = xname)
    x <- x[with(x, order(Group)), ]
    x$Observation_number <- 1:nrow(x)
    y <- pivotlonger(y, names_to = "Group", values_to = yname)
    y <- y[with(y, order(Group)), ]
    y$Observation_number <- 1:nrow(y)
    data <- merge(x, y, by = "Observation_number", all = TRUE)
    names(data)[names(data) == paste0("Group", ".x")] <- "Group"
    data <- subset(data, select = -c(get(paste0("Group", ".y"))))
  } else {
    stop(
    "Either x or y must have only 1 column, or both x and y must have the same number of columns",
    call. = FALSE)
  }

  return(list(data = data, xname = xname, yname = yname))
}
