library(vdiffr)

x <- iris[, 1]
y <- iris[, 2:4]

test_that("plot colors by color values", {
  skip_on_cran()
  skip_on_os("windows")
  # single color value
  expect_doppelganger(
    "single color scatterplot",
    ggmatplot(x, y, plot_type = "point", color = "red")
  )
  # color values = number of groups
  expect_doppelganger(
    "three color violin plot",
    ggmatplot(y, plot_type = "violin", color = c("red", "blue", "#123456"))
  )
})

test_that("plot colors by fill values", {
  skip_on_cran()
  skip_on_os("windows")
  # single fill value
  expect_doppelganger(
    "single fill density plot",
    ggmatplot(y, plot_type = "density", fill = "red")
  )
  # fill values = number of groups
  expect_doppelganger(
    "three fill color violin plot",
    ggmatplot(y, plot_type = "violin", fill = c("red", "blue", "#123456"))
  )
})

test_that("plot colors by color and fill values simultaneously", {
  skip_on_cran()
  skip_on_os("windows")
  # single color value, fill values = number of groups
  expect_doppelganger(
    "color and fill density plot",
    ggmatplot(y,
      plot_type = "density", color = "red",
      fill = c("red", "blue", "#123456")
    )
  )
})

test_that("point shapes updated based on shape values", {
  skip_on_cran()
  skip_on_os("windows")
  # single shape value
  expect_doppelganger(
    "single shape scatterplot",
    ggmatplot(y, plot_type = "point", shape = "square")
  )
  # shape values = number of groups
  expect_doppelganger(
    "three shape scatterplot",
    ggmatplot(x, y, plot_type = "point", shape = c(15, 12, 3))
  )
})

test_that("line types updated based on linetype values", {
  skip_on_cran()
  skip_on_os("windows")
  # single linetype value
  expect_doppelganger(
    "single linetype line plot",
    ggmatplot(y, plot_type = "line", linetype = "dashed")
  )
  # linetype values = number of groups
  expect_doppelganger(
    "three linetype line plot",
    ggmatplot(x, y, plot_type = "line", linetype = c(15, 12, "dashed"))
  )
  # linetype values < number of groups
  expect_error(
    ggmatplot(x, y, plot_type = "both", linetype = c(15, 12)),
    "Insufficient linetype values. 3 needed but only 2 provided."
  )
  # linetype values > number of groups
  expect_error(
    ggmatplot(x, y, plot_type = "both", linetype = c(15, 12, 3, 5)),
    "Too many linetype values. Only 3 needed but 4 provided."
  )
})

test_that("plot axis limits are updated based on xlim and ylim parameters", {
  skip_on_cran()
  skip_on_os("windows")
  # x axis limits only
  suppressWarnings(expect_doppelganger(
    "density plot with x axis limits",
    ggmatplot(y, plot_type = "density", xlim = c(3, 4))
  ))
  # y axis limits only
  suppressWarnings(expect_doppelganger(
    "histogram with y axis limits",
    ggmatplot(y, plot_type = "histogram", ylim = c(0, 20))
  ))
  # x and y axis limits
  suppressWarnings(expect_doppelganger(
    "scatterplot with a and y axis limits",
    ggmatplot(x, y, plot_type = "point", xlim = c(5, 7), ylim = c(0, 3))
  ))
})

test_that("plot axes are transformed to a log scale based on log parameter
          values", {
  skip_on_cran()
  skip_on_os("windows")
  # log(x)
  expect_doppelganger(
    "line plot with log x",
    ggmatplot(x, y, plot_type = "line", log = "x")
  )
  # log(y)
  expect_doppelganger(
    "scatterplot with log y",
    ggmatplot(x, y, plot_type = "point", log = "y")
  )
  # log(x) and log(y)
  expect_doppelganger(
    "point + line plot with log x and y",
    ggmatplot(x, y, plot_type = "both", log = "xy")
  )
})

test_that("plot title is updated based on main paramter", {
  skip_on_cran()
  skip_on_os("windows")
  expect_equal(
    ggmatplot(x, y, plot_type = "line")$labels$title,
    NULL
  )
  expect_equal(
    ggmatplot(x, y,
      plot_type = "line",
      main = "title test"
    )$labels$title,
    "title test"
  )
})

test_that("plot axis labels are updated based on xlab and ylab paramters", {
  skip_on_cran()
  skip_on_os("windows")
  expect_equal(
    ggmatplot(x, y, plot_type = "line", xlab = "xlab test")$labels$x,
    "xlab test"
  )
  expect_equal(
    ggmatplot(y, plot_type = "point", ylab = "ylab test")$labels$y,
    "ylab test"
  )
})

test_that("invalid plot types are not allowed", {
  skip_on_cran()
  skip_on_os("windows")
  expect_error(
    ggmatplot(x, y, plot_type = "scatterplot"),
    "plot_type can not take this value"
  )
})

test_that("legend labels are updated", {
  skip_on_cran()
  skip_on_os("windows")
  # legend label values = number of groups
  expect_doppelganger(
    "plot with updated legend labels",
    ggmatplot(x, y, legend_label = c("lab1", "lab2", "lab3"))
  )
})

test_that("legend title is updated", {
  skip_on_cran()
  skip_on_os("windows")
  expect_doppelganger(
    "plot with updated legend title",
    ggmatplot(x, y, legend_title = "Legend Title")
  )
})

test_that("plot is resized by aspect ratio(asp)", {
  skip_on_cran()
  skip_on_os("windows")
  expect_doppelganger(
    "plot resized by aspect ratio",
    ggmatplot(x, y, asp = 0.5)
  )
})

test_that("errorplot updates using the defined desc_stat", {
  skip_on_cran()
  skip_on_os("windows")
  expect_doppelganger(
    "errorplot with medians and range errorbars",
    ggmatplot(y, plot_type = "errorplot", desc_stat = "median_range")
  )
})


test_that("invalid parameter values throw errors", {
  # color values < number of groups
  expect_error(
    ggmatplot(y, plot_type = "violin", color = c("red", "blue")),
    "Insufficient color values. 3 needed but only 2 provided."
  )
  # color values > number of groups
  expect_error(
    ggmatplot(y,
      plot_type = "violin",
      color = c("red", "blue", "yellow", "green")
    ),
    "Too many color values. Only 3 needed but 4 provided."
  )
  # fill values < number of groups
  expect_error(
    ggmatplot(y, plot_type = "violin", fill = c("red", "blue")),
    "Insufficient fill values. 3 needed but only 2 provided."
  )
  # fill values > number of groups
  expect_error(
    ggmatplot(y,
      plot_type = "violin",
      fill = c("red", "blue", "yellow", "green")
    ),
    "Too many fill values. Only 3 needed but 4 provided."
  )
  # single fill value, color values < number of groups
  expect_error(
    ggmatplot(y,
      plot_type = "violin", fill = "black",
      color = c("red", "blue")
    ),
    "Insufficient color values. 3 needed but only 2 provided."
  )
  # shape values < number of groups
  expect_error(
    ggmatplot(x, y, plot_type = "point", shape = c(15, 12)),
    "Insufficient shape values. 3 needed but only 2 provided."
  )
  # shape values > number of groups
  expect_error(
    ggmatplot(x, y, plot_type = "point", shape = c(15, 12, 3, 5)),
    "Too many shape values. Only 3 needed but 4 provided."
  )
  # legend label values < number of groups
  expect_error(
    ggmatplot(x, y, legend_label = c("lab1")),
    "Insufficient legend_label values. 3 needed but only 1 provided."
  )
  # legend label values > number of groups
  expect_error(
    ggmatplot(x, y, legend_label = c("lab1", "lab2", "lab3", "lab4")),
    "Too many legend_label values. Only 3 needed but 4 provided."
  )
  # invalid number of limits
  expect_error(
    ggmatplot(y, plot_type = "line", xlim = 5),
    "xlim must be a two element vector"
  )
  expect_error(
    ggmatplot(y, plot_type = "line", xlim = c(1, 2, 5)),
    "xlim must be a two element vector"
  )
  # invalid log value
  expect_error(
    ggmatplot(y, plot_type = "point", log = 1),
    "invalid log value provided"
  )
  # invalid des_stat value
  expect_error(
    ggmatplot(y, plot_type = "errorplot", desc_stat = "mean_sed"),
    "desc_stat can not take this value"
  )
})

test_that("unknown parameter values for plots throw errors", {
  # shapes for plot types that don't require shapes
  expect_warning(
    ggmatplot(y, plot_type = "density", shape = c(15, 12, 13)),
    "shape is an invalid parameter for plot type: density"
  )
  # linetypes for plot types that don't require linetypes
  expect_warning(
    ggmatplot(x, y, plot_type = "point", linetype = c(15, 12, 13)),
    "linetype is an invalid parameter for plot type: point"
  )
  # desc_stat for plot types that don't require descriptive statistics
  expect_warning(
    ggmatplot(x, plot_type = "density", desc_stat = "median_iqr"),
    "desc_stat is an invalid parameter for plot type: density"
  )
})
