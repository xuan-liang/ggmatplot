#' Function to validate the number of parameter values against the number of groups
#'
#' @param parameterValues a list of values. Should be of length 1 or correspond to the number of groups given by the 'numGroups' parameter.
#' @param numGroups an integer denoting the number of unique values of the grouping variable
#'
#' @return a list of parameter values.
#' @NoRd
#'
numParameterHandler <- function(parameterValues, numGroups) {
  # same for all groups
  if (!numGroups == 1 & length(parameterValues) == 1) {
    return(rep(parameterValues, numGroups))
  }
  # values > number of unique groups
  else if (length(parameterValues) > numGroups) {
    stop(paste0("Too many ", substitute(parameterValues), " values. Only ", numGroups, " needed but ", length(parameterValues), " provided."), call. = FALSE)
  }
  # values < number of unique groups
  else if (length(parameterValues) < numGroups) {
    stop(paste0("Insufficient ", substitute(parameterValues), " values. ", numGroups, " needed but only ", length(parameterValues), " provided."), call. = FALSE)
  } else {
    return(parameterValues)
  }
}
