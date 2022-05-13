#' Function to compute descriptive statistics to be used for errorplots
#'
#' @param data Resultant data frame from matclean()
#' @param desc_stat Statistics to calculate. Either of `mean_se`, `mean_sd`,
#' `mean_range`, `median_iqr`, `median_range`
#'
#' @return A data frame with descriptive statistics for each group.
#'
#' @importFrom stats sd median quantile
#'
#' @noRd
#'
#' @examples
#' # Define a data set
#' iris_sub <- subset(iris, Species == "setosa")[1:4]
#' # Use ggmatplot::matclean to pivot the dataframe into long format
#' data <- matclean(iris_sub)$data
#' errorplotstats(data, desc_stat = "mean_se")
errorplotstats <- function(data, desc_stat = "") {
  if (desc_stat == "mean_se") {
    errorplot_data <- do.call(rbind, by(data, list(data[[GROUP_NAME]]), function(x) {
      c(
        y = mean(x$x, na.rm = TRUE),
        ymin = mean(x$x, na.rm = TRUE) -
          (sd(x$x, na.rm = TRUE) / sqrt(nrow(x))),
        ymax = mean(x$x, na.rm = TRUE) +
          (sd(x$x, na.rm = TRUE) / sqrt(nrow(x)))
      )
    }))
  } else if (desc_stat == "mean_sd") {
    errorplot_data <- do.call(rbind, by(data, list(data[[GROUP_NAME]]), function(x) {
      c(
        y = mean(x$x, na.rm = TRUE),
        ymin = mean(x$x, na.rm = TRUE) - sd(x$x, na.rm = TRUE),
        ymax = mean(x$x, na.rm = TRUE) + sd(x$x, na.rm = TRUE)
      )
    }))
  } else if (desc_stat == "mean_range") {
    errorplot_data <- do.call(rbind, by(data, list(data[[GROUP_NAME]]), function(x) {
      c(
        y = mean(x$x, na.rm = TRUE),
        ymin = min(x$x, na.rm = TRUE),
        ymax = max(x$x, na.rm = TRUE)
      )
    }))
  } else if (desc_stat == "median_iqr") {
    errorplot_data <- do.call(rbind, by(data, list(data[[GROUP_NAME]]), function(x) {
      c(
        y = median(x$x, na.rm = TRUE),
        ymin = quantile(x$x, probs = 0.25, names = FALSE),
        ymax = quantile(x$x, probs = 0.75, names = FALSE)
      )
    }))
  } else if (desc_stat == "median_range") {
    errorplot_data <- do.call(rbind, by(data, list(data[[GROUP_NAME]]), function(x) {
      c(
        y = median(x$x, na.rm = TRUE),
        ymin = min(x$x, na.rm = TRUE),
        ymax = max(x$x, na.rm = TRUE)
      )
    }))
  }

  group_df <- data.frame(x = factor(rownames(errorplot_data), levels = rownames(errorplot_data)))
  names(group_df) <- GROUP_NAME

  return(cbind(
    group_df,
    data.frame(errorplot_data, row.names = NULL)
  ))
}
