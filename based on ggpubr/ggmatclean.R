ggmatclean <- function (x, y){

  if(is.list(x)){
    x <- ggmatlist(x)
  }

  x <- data.frame(x)
  ncolx <- ncol(x)
  
  if (!missing(y)){
    ncoly <- ncol(as.data.frame(y))
    y <- data.frame(y)
  }else{
    ncoly<-1
  }
  
   if (missing(y)){
     data <- mutate(x, Observation_number = 1:n()) %>% gather(key = "Group", value = "yy", -(ncolx+1))
     xname <- "Observation_number"
     yname <- "yy"
     group <- "Group"
   }else if(ncolx>ncoly&ncoly==1){
     data <- data.frame(x,y)
     ncol <- ncol(data)
     yname <- colnames(data)[ncol]
     data <- mutate(data, Observation_number = 1:n()) %>% gather(key = "Group", value = "x", -c(ncol,(ncol+1)))
     xname <- "x"
     yname <- colnames(y)
     group <- "Group"
   }else if(ncoly>ncolx&ncolx==1){
     data <- data.frame(x,y)
     ncol <- ncol(data)
     data <- mutate(data, Observation_number = 1:n()) %>% gather(key = "Group", value = "yy", -c(1,(ncol+1)))
     xname <- colnames(x)
     yname <- "yy"
     group <- "Group"
   }else if(ncolx==ncoly){
     colnames(x) = colnames(y) = paste0("Column ", 1:ncolx)
     x <- gather(x, key = "Group", value = "x") %>% mutate(Observation_number = 1:n())
     y <- gather(y, key = "Group", value = "yy") %>% mutate(Observation_number = 1:n())
     data <- full_join(x, y, by = "Observation_number") %>% rename(Group = Group.x) %>% select(-Group.y)
     xname <- "x"
     yname <- "yy"
     group <- "Group"
   }else{
     stop("`x`` and `y` must have only 1 or the same number of columns", call. = FALSE)
   }
  
  return(list(data=data, xname=xname, yname=yname, group=group, ncolx=ncolx, ncoly=ncoly))
}
