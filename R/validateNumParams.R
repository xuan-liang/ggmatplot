#' Function to validate the number of parameter values against the number of
#' groups
#'
#' @param parameterValues A list of values.
#' @param numGroups An integer denoting the number of unique values of the
#' grouping variable.
#'
#' @return If valid, a list of parameter values. If invalid, an error message.
#' @noRd
#'
validateNumParams <- function(parameterValues, numGroups) {
  # set same value for all groups
  if (!numGroups == 1 & length(parameterValues) == 1) {
    return(rep(parameterValues, numGroups))
  }
  # values > number of unique groups
  else if (length(parameterValues) > numGroups) {
    stop(paste0(
      "Too many ", substitute(parameterValues), " values. Only ",
      numGroups, " needed but ", length(parameterValues), " provided."
    ), call. = FALSE)
  }
  # values < number of unique groups
  else if (length(parameterValues) < numGroups) {
    stop(paste0(
      "Insufficient ", substitute(parameterValues), " values. ",
      numGroups, " needed but only ", length(parameterValues), " provided."
    ), call. = FALSE)
  }
  # values = number of unique groups
  else {
    return(parameterValues)
  }
}
