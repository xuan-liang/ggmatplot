getoption <- function(params, plot.type){
  
  option <- list()
  
  if(plot.type=="histogram")
    allowed_options <- c("data","x","color","fill","linetype","alpha", "bins", "binwidth", "add", "add.params", "rug","add_density","position")
  
  for (key in names(params)) {
    value <- params[[key]]
    if (key %in% allowed_options) {
      option[[key]] <- value
    }
  }
  return(option)
}