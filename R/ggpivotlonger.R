#' Function to convert a wide dataframe into long format
#'
#' @param data dataframe to pivot.
#' @param names_to a string specifying the name of the column to create from the column names of those being pivoted.
#' @param values_to a string specifying the name of the column to create from the data stored in cell values of the columns being pivoted.
#' @param id_cols columns that uniquely identify observations. Columns to not pivot into longer format.
#'
#' @return a dataframe in long format
#' @NoRd
#'
#' @examples
#' # Define a data set
#' iris_sub <- subset(iris, Species == "setosa")[1:4]
#' # Use first two columns as ID columns and pivot the second two columns to long format
#' ggpivotlonger(iris_sub, names_to = "Measurement", values_to = "Value", c(1, 2))
ggpivotlonger <- function(data, names_to = "", values_to = "", id_cols = NULL) {
  long_df <- data.frame(matrix(ncol = 2 + length(id_cols), nrow = 0))
  colnames(long_df) <- c(colnames(data[id_cols]), names_to, values_to)
  n_widecols <- ncol(data) - length(id_cols)
  ifelse(!is.null(id_cols), widecols <- data[-1 * id_cols], widecols <- data)
  for (i in 0:nrow(data) - 1) {
    for (j in 1:n_widecols) {
      if (!is.null(id_cols)) {
        for (k in 1:length(id_cols)) {
          long_df[n_widecols * i + j, k] <- data[i + 1, id_cols[k]]
        }
      }
      long_df[n_widecols * i + j, names_to] <- colnames(widecols)[j]
      long_df[n_widecols * i + j, values_to] <- widecols[i + 1, j]
    }
  }
  return(long_df)
}
