ggmatplot1 <- function (x, y, color = NULL, shape = NULL, linetype = NULL, fill=NULL,
                       xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                       main = NULL, xlab = NULL, ylab = NULL,
                       legend_label =  NULL, legend_title = NULL,
                       plot.type = "point", asp = NA, ...){
  
  # caller_env <- parent.frame()
  
  if (! plot.type %in% c("point", "line", "both", "density", "histogram", "boxplot", "dotplot","errorplot",
                         "violin","ecdf") ) 
    stop("`plot.type` can not be this character", call. = FALSE)
  
  if (!missing(x)&!missing(y)){
    data.list <- ggmatclean (x=x, y=y)
  } else if (!missing(x)){
    data.list <- ggmatclean (x=x)
  } else{
    stop("No data have been put into the ggmatplot function", call. = FALSE)
  }
  
  data <- data.list$data %>% drop_na()
  xname <- data.list$xname
  yname <- data.list$yname
  ncolx <- data.list$ncolx
  ncoly <- data.list$ncoly
  group <- data.list$group 
  
  dcolor <- data.list$group 
  dfill <- data.list$group
  dshape <- data.list$group
  dlinetype <- data.list$group
  
  if(missing(y)& ncolx==1){
    dcolor = "black" 
    dfill = "lightgray"
    dshape = 19
    dlinetype = 1
  }
  
  params <- list(...)
  params$data <- data
  params$color <- dcolor
  params$fill<- dfill
  params$shape <- dshape
  params$dlinetype <- dlinetype
  
  if(plot.type=="point"){
    p <-ggscatter(data = data, x = xname, y = yname, color = dcolor, shape=dshape)
  }
  if(plot.type=="line"){
    p <-ggline(data = data, x = xname, y = yname, color= dcolor, linetype = dlinetype, plot_type = "l")
  }
  if(plot.type=="both"){
    p <-ggline(data = data, x = xname, y = yname, color= dcolor, linetype = dlinetype, shape=dshape, ...)
  }
  if(plot.type=="density"){
    p <-ggdensity(data = data, x = yname, color = dcolor, fill = dfill, ...)+ xlab("x")
  }
  if(plot.type=="histogram"){
    params$x <- yname
    option <- getoption(params, plot.type)
    p <- do.call(gghistogram, option)
    p <- p + xlab("x")
  }
  if(plot.type=="boxplot"){
    p <-ggboxplot(data = data, x = group, y = yname, color = dcolor) + xlab("")
  }
  if(plot.type=="dotplot"){
    p <-ggdotplot(data = data, x = group, y = yname, color = dcolor) + xlab("")
  }
  if(plot.type=="errorplot"){
    p <-ggerrorplot(data = data, x = group, y = yname, color = dcolor) + xlab("")
  }
  if(plot.type=="violin"){
    p <-ggviolin(data = data, x = group, y = yname, color = dcolor) + xlab("")
  }
  if(plot.type=="ecdf"){
    p <-ggecdf(data = data, x = yname, color = dcolor) + xlab("")
  }
  
  maxcol= max(ncolx,ncoly)
  maxshape = maxcol
  maxlinetyoe = maxcol
  if(!is.null(color) & length(color) != maxcol){
    color=rep(color,maxcol)[1:maxcol]
  }
  
  if(!is.null(shape) & length(shape) != maxcol){
    shape=rep(shape,maxcol)[1:maxcol]
  }
  
  if(!is.null(linetype) & length(linetype) != maxcol){
    linetype=rep(shape,maxcol)[1:maxcol]
  }
  
  if(is.null(legend_title)){
    legend_title = ""
  }
  if(is.null(legend_label)){
    legend_label = unique(data$Group)
  }
  
  if (!is.null(color))
    p <- p + scale_color_manual(name = legend_title, labels = legend_label, values = color)
  if (is.null(color))
    p <- p + scale_color_discrete(name = legend_title, labels = legend_label)
  
  if (!is.null(shape))
    p <- p + scale_shape_manual(name = legend_title, labels = legend_label, values = shape)
  if (is.null(shape))
    p <- p + scale_shape_discrete(name = legend_title, labels = legend_label)
  
  if (!is.null(linetype))
    p <- p + scale_linetype_manual(name = legend_title, labels = legend_label, values = linetype)
  if (is.null(linetype))
    p <- p + scale_linetype_discrete(name = legend_title, labels = legend_label)
  
  if (!is.null(fill))
    p <- p + scale_fill_manual(name = legend_title, labels = legend_label, values = fill)
  if (is.null(fill))
    p <- p + scale_fill_discrete(name = legend_title, labels = legend_label)
  
  if (!is.null(main)) p <- p + ggtitle(main) + theme(plot.title = element_text(hjust = 0.5))
  
  if (!missing(xlab)) p <- p + xlab(xlab)
  if (!missing(ylab)) p <- p + ylab(ylab)
  
  if (!missing(xlim)) p <- p + xlim(xlim)
  if (!missing(ylim)) p <- p + ylim(ylim)
  
  logv <- function(var) var %in% strsplit(log, "")[[1]]
  
  if (logv("x")) p <- p + scale_x_log10()
  if (logv("y")) p <- p + scale_y_log10()
  
  if (!is.na(asp)) p <- p + theme(aspect.ratio = asp)
  
  p <-p + theme(legend.position="right")
  
  if(missing(y)& ncolx==1){
    p<- p + theme(legend.position="none")
  }
  
  return(p)
}

