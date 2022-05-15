#' Function to validate the number of parameter values against the number of
#' groups
#'
#' @param param_values A list of values.
#' @param ngroup An integer denoting the number of unique values of the
#' grouping variable.
#' @param strict A logical value to indicate that the number of param_values has
#'  to match the number of groups. Default is FALSE.
#'
#' @return If valid, a list of parameter values. If invalid, an error message.
#' @noRd
#'
validate_nparam <- function(param_values, ngroup, strict = FALSE) {
  if(is.null(param_values)) {
    return(param_values)
  }
  # set same value for all groups
  if (!strict & !ngroup == 1 & length(param_values) == 1) {
    return(rep(param_values, ngroup))
  }
  # values > number of unique groups
  else if (length(param_values) > ngroup) {
    stop(paste0(
      "Too many ", substitute(param_values), " values. Only ",
      ngroup, " needed but ", length(param_values), " provided."
    ), call. = FALSE)
  }
  # values < number of unique groups
  else if (length(param_values) < ngroup) {
    stop(paste0(
      "Insufficient ", substitute(param_values), " values. ",
      ngroup, " needed but only ", length(param_values), " provided."
    ), call. = FALSE)
  }
  # values = number of unique groups
  else {
    return(param_values)
  }
}


validate_input <- function(x, y, plot_type, desc_stat,
                           color, fill, shape, linetype, xlim, ylim,
                           log, asp) {
  if(!is.null(x) & !is.null(y) & plot_type %in% SINGLE_INPUT_PLOT_TYPE) {
    stop("This plot type only uses a single matrix input", call. = FALSE)
  } else if (is.null(x)) {
    stop("x can not be missing", call. = FALSE)
  }

  if(desc_stat != "mean_se" & plot_type != "errorplot") {
    warning(paste0(
      "desc_stat is an invalid parameter for plot type: ", plot_type),
      call. = FALSE)
  }

  if(plot_type == "errorplot" && !(desc_stat %in% STATS_WITH_RANGE)) {
    stop("desc_stat can not take this value", call. = FALSE)
  }

  if(!is.null(log) && !log %in% c("x", "y", "xy")) {
    stop("invalid `log` value provided", call. = FALSE)
  }

  if(length(xlim) != 2) {
    stop("xlim must be a two element vector", call. = FALSE)
  }

  if(length(ylim) != 2) {
    stop("ylim must be a two element vector", call. = FALSE)
  }

  if (!is.null(shape) && !plot_type %in% c("point", "both")) {
    warning(paste0(
      "shape is an invalid parameter for plot type: ",
      plot_type
    ), call. = FALSE)
  }

  if (!is.null(linetype) && !plot_type %in% c("line", "both")) {
    warning(paste0(
      "linetype is an invalid parameter for plot type: ",
      plot_type
    ), call. = FALSE)
  }

}


construct_data_list <- function(x, y) {
  if(is.null(y)) {
    matclean(x = x)
  } else {
    matclean(x = x, y = y)
  }

}


OBSERVATION_NAME <- "Observation"
GROUP_NAME <- "Group"
SINGLE_INPUT_PLOT_TYPE <- c("density", "histogram", "boxplot", "dotplot",
                            "errorplot", "violin", "ecdf")
DISTRIBUTION_PLOT_TYPE <- c("histogram", "density", "violin", "boxplot", "dotplot")
STATS_WITH_RANGE <- c("mean_se", "mean_sd", "mean_range", "median_iqr", "median_range")



get_data_for_plot <- function(data_list, plot_type, desc_stat) {
  if(plot_type == "errorplot") {
    errorplotstats(data_list$data, desc_stat)
  } else {
    data_list$data
  }
}




setup_params <- function(params_input, plot_type) {
  params <- params_input
  if(plot_type %in% DISTRIBUTION_PLOT_TYPE) {
    params$alpha <- params$alpha %||% 0.5
  }
  if(plot_type == "dotplot") {
    params$binaxis <- params$binaxis %||% "y"
    params$stackdir <- params$stackdir %||% "center"
  }
  params
}


`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}


update_legend_aes <- function(plot, plot_type, color, fill, shape, linetype, legend_title, legend_label, nvar) {
  if (!is.null(color)) {
    plot <- plot + scale_color_manual(
      name = legend_title, labels = legend_label,
      values = color
    )
    ncolor <- length(unique(color))
    if(ncolor==1 || plot_type %in% c("violin", "boxplot", "errorplot", "dotplot")) {
      plot <- plot + guides(color = "none")
    }

    # if color is defined and fill isn't, update both using color values
    if (is.null(fill)) {
      plot <- plot + scale_fill_manual(
        name = legend_title, labels = legend_label,
        values = color
      )
      if(ncolor==1 || plot_type %in% c("violin", "boxplot", "errorplot", "dotplot")) {
        plot <- plot + guides(fill = "none")
      }
    }

  }

  if (!is.null(fill)) {
    plot <- plot + scale_fill_manual(
      name = legend_title, labels = legend_label,
      values = fill
    )
    nfill <- length(unique(fill))
    if(nfill==1 || plot_type %in% c("violin", "boxplot", "errorplot", "dotplot")) {
      plot <- plot + guides(fill = "none")
    }
    # if fill is defined and color isn't, update both using fill values
    if (is.null(color)) {
      plot <- plot + scale_color_manual(
        name = legend_title, labels = legend_label,
        values = fill
      )
      if(nfill==1 || plot_type %in% c("violin", "boxplot", "errorplot", "dotplot")) {
        plot <- plot + guides(color = "none")
      }
    }
  }

  # if both color and fill values are not defined, use default values
  if (is.null(color) & is.null(fill)) {

    # removing default coloring by groups for violin, boxplots and errorplot
    if(plot_type %in% c("violin","boxplot")) {
      plot <- plot +
        scale_fill_manual(name = legend_title, labels = legend_label, values = rep("white", length(legend_label))) +
        scale_color_manual(name = legend_title, labels = legend_label, values = rep("black", length(legend_label))) +
        guides(fill = "none", color = "none")
    } else if (plot_type %in% c("errorplot","dotplot") || nvar == 1) {
      plot <- plot +
        scale_fill_manual(name = legend_title, labels = legend_label, values = rep("black", length(legend_label))) +
        scale_color_manual(name = legend_title, labels = legend_label, values = rep("black", length(legend_label))) +
        guides(fill = "none", color = "none")
    }else {
      plot <- plot +
        scale_fill_discrete(name = legend_title, labels = legend_label) +
        scale_color_discrete(name = legend_title, labels = legend_label)
    }

  }

  if (!is.null(shape)) {
    plot <- plot + scale_shape_manual(
      name = legend_title, labels = legend_label,
      values = shape
    )
  } else {
    plot <- plot + scale_shape_discrete(name = legend_title, labels = legend_label)
  }

  if (!is.null(linetype)) {
    plot <- plot + scale_linetype_manual(
      name = legend_title, labels = legend_label, values = linetype)
  } else {
    plot <- plot + scale_linetype_discrete(name = legend_title, labels = legend_label)
  }

  if(nvar == 1) {
    plot <- plot + guides(fill = "none", color = "none", shape = "none", linetype = "none")
  }

  plot
}
