ggmatlist <- function (x){
  max.length <- max(sapply(x, length))
  x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
  x<- do.call(cbind, x)
}