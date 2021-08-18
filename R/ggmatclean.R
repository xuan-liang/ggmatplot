ggmatclean <- function (x, y){

  x <- data.frame(x)
  ncolx <- ncol(x)

  if (!missing(y)){
    ncoly <- ncol(as.data.frame(y))
    y <- data.frame(y)
  }else{
    ncoly<-1
  }

  if (missing(y)){
    x$Observation_number <- 1:nrow(x)
    data <- ggpivotlonger(x, names_to = "Group", values_to = "yy", ncolx+1)
    xname <- "Observation_number"
    yname <- "yy"
    group <- "Group"
  }else if(ncolx>ncoly&ncoly==1){
    data <- data.frame(x,y)
    ncol <- ncol(data)
    yname <- colnames(data)[ncol]
    data$Observation_number <- 1:nrow(data)
    data <- ggpivotlonger(data, names_to = "Group", values_to = "x", c(ncol,(ncol+1)))
    xname <- "x"
    yname <- colnames(y)
    group <- "Group"
  }else if(ncoly>ncolx&ncolx==1){
    data <- data.frame(x,y)
    ncol <- ncol(data)
    data$Observation_number <- 1:nrow(data)
    data <- ggpivotlonger(data, names_to = "Group", values_to = "yy", c(1,(ncol+1)))
    xname <- colnames(x)
    yname <- "yy"
    group <- "Group"
  }else if(ncolx==ncoly){
    colnames(x) = colnames(y) = paste0("Column ", 1:ncolx)
    x <- testfunc1(x, names_to = "Group", values_to = "x")
    x <- x[with(x, order(Group)),]
    x$Observation_number <- 1:nrow(x)
    y <- testfunc1(y, names_to = "Group", values_to = "yy")
    y <- y[with(y, order(Group)),]
    y$Observation_number <- 1:nrow(y)
    data <- full_join(x, y, by = "Observation_number") %>% rename(Group = Group.x) %>% select(-Group.y)
    xname <- "x"
    yname <- "yy"
    group <- "Group"
  }else{
    stop("`x`` and `y` must have only 1 or the same number of columns", call. = FALSE)
  }

  return(list(data=data, xname=xname, yname=yname, group=group, ncolx=ncolx, ncoly=ncoly))
}
