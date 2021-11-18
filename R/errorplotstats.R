#' Function to compute descriptive statistics to be used for errorplots
#'
#' @param data Resultant data frame from matclean()
#' @param desc_stat Statistics to calculate. Either of `mean_se`, `mean_sd`, `mean_range`, `median_iqr`, `median_range`
#'
#' @return A data frame with descriptive statistics for each group.

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
    errorplot_data <- do.call(rbind, by(data, list(data$Group), function(x) {
      c(
        y = mean(x$x, na.rm = TRUE),
        ymin = mean(x$x, na.rm = TRUE) - (standard_dev(x$x[!is.na(x$x)]) / sqrt(nrow(x))),
        ymax = mean(x$x, na.rm = TRUE) + (standard_dev(x$x[!is.na(x$x)]) / sqrt(nrow(x)))
      )
    }))
  } else if (desc_stat == "mean_sd") {
    errorplot_data <- do.call(rbind, by(data, list(data$Group), function(x) {
      c(
        y = mean(x$x, na.rm = TRUE),
        ymin = mean(x$x, na.rm = TRUE) - standard_dev(x$x[!is.na(x$x)]),
        ymax = mean(x$x, na.rm = TRUE) + standard_dev(x$x[!is.na(x$x)])
      )
    }))
  } else if (desc_stat == "mean_range") {
    errorplot_data <- do.call(rbind, by(data, list(data$Group), function(x) {
      c(
        y = mean(x$x, na.rm = TRUE),
        ymin = min(x$x, na.rm = TRUE),
        ymax = max(x$x, na.rm = TRUE)
      )
    }))
  } else if (desc_stat == "median_iqr") {
    errorplot_data <- do.call(rbind, by(data, list(data$Group), function(x) {
      c(
        y = get_median(x$x[!is.na(x$x)]),
        ymin = get_quantile(x$x[!is.na(x$x)], quantile = 0.25),
        ymax = get_quantile(x$x[!is.na(x$x)], quantile = 0.75)
      )
    }))
  } else if (desc_stat == "median_range") {
    errorplot_data <- do.call(rbind, by(data, list(data$Group), function(x) {
      c(
        y = get_median(x$x[!is.na(x$x)]),
        ymin = min(x$x, na.rm = TRUE),
        ymax = max(x$x, na.rm = TRUE)
      )
    }))
  }

  return(cbind(
    Group = rownames(errorplot_data),
    data.frame(errorplot_data, row.names = NULL)
  ))
}

# returns standard deviation of vector
standard_dev <- function(vector){
  return(sqrt(sum((vector - mean(vector))^2)/(length(vector)-1)))
}

# returns median of vector
get_median <- function(vector){
  vector <- sort(vector)
  if(length(vector)%%2 == 0){
    return((vector[length(vector)/2] + vector[(length(vector)/2)+1]) / 2)
  } else {
    return(vector[floor(length(vector)/2)])
  }
}

# returns value at defined quantile
get_quantile <- function(vector, quantile){
  vector <- sort(vector)
  return(vector[round(quantile*(length(vector)+ 1))])
}
