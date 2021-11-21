#' Function to convert a wide dataframe into long format
#'
#' @param data A data frame to pivot.
#' @param names_to A string specifying the name of the column to create from the column names of those being pivoted.
#' @param values_to A string specifying the name of the column to create from the data stored in the columns being pivoted.
#' @param id_cols 	[<tidy-select>](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html) Columns that uniquely identify observations and not to pivot into longer format.
#'
#' @return A long format data frame.
#' @noRd
#'
#' @examples
#' # Define a data set
#' iris_sub <- subset(iris, Species == "setosa")[1:4]
#' # Use first two columns as ID columns and pivot the second two columns to
#' # long format
#' pivotlonger(data = iris_sub, names_to = "Measurement", values_to = "Value",
#' id_cols = c(1, 2))
#'
pivotlonger <- function(data, names_to = "", values_to = "", id_cols = NULL) {
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
