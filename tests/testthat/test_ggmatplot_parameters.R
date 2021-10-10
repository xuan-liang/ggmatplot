library(vdiffr)

x <- iris[, 1]
y <- iris[, 2:4]

test_that("plot colors by color values", {
  # single color value
  expect_doppelganger("single color scatterplot", ggmatplot(x, y, plot.type = "point", color = "red"))
  # color values = number of groups
  expect_doppelganger("three color violin plot", ggmatplot(y, plot.type = "violin", color = c("red", "blue", "#123456")))
  # color values < number of groups
  expect_error(ggmatplot(y, plot.type = "violin", color = c("red", "blue")), "Insufficient color values. 3 needed but only 2 provided.")
  # color values > number of groups
  expect_error(ggmatplot(y, plot.type = "violin", color = c("red", "blue", "yellow", "green")), "Too many color values. Only 3 needed but 4 provided.")
})

test_that("plot colors by fill values", {
  # single fill value
  expect_doppelganger("single fill density plot", ggmatplot(y, plot.type = "density", fill = "red"))
  # fill values = number of groups
  expect_doppelganger("three fill color violin plot", ggmatplot(y, plot.type = "violin", fill = c("red", "blue", "#123456")))
  # fill values < number of groups
  expect_error(ggmatplot(y, plot.type = "violin", fill = c("red", "blue")), "Insufficient fill values. 3 needed but only 2 provided.")
  # fill values > number of groups
  expect_error(ggmatplot(y, plot.type = "violin", fill = c("red", "blue", "yellow", "green")), "Too many fill values. Only 3 needed but 4 provided.")
})

test_that("plot colors by color and fill values simultaneously", {
  # single color value, fill values = number of groups
  expect_doppelganger("color and fill density plot", ggmatplot(y, plot.type = "density", color = "red", fill = c("red", "blue", "#123456")))
  # single fill value, color values < number of groups
  expect_error(ggmatplot(y, plot.type = "violin", fill = "black", color = c("red", "blue")), "Insufficient fill values. 3 needed but only 2 provided.")
})

test_that("point shapes updated based on shape values", {
  # single shape value
  expect_doppelganger("single shape scatterplot", ggmatplot(y, plot.type = "point", shape = "square"))
  # shape values = number of groups
  expect_doppelganger("three shape scatterplot", ggmatplot(x, y, plot.type = "point", shape = c(15, 12, 3)))
  # shape values < number of groups
  expect_error(ggmatplot(x, y, plot.type = "point", shape = c(15, 12)), "Insufficient shape values. 3 needed but only 2 provided.")
  # shape values > number of groups
  expect_error(ggmatplot(x, y, plot.type = "point", shape = c(15, 12, 3, 5)), "Too many shape values. Only 3 needed but 4 provided.")
})

test_that("line types updated based on linetype values", {
  # single linetype value
  expect_doppelganger("single linetype line plot", ggmatplot(y, plot.type = "line", linetype = "dashed"))
  # linetype values = number of groups
  expect_doppelganger("three linetype line plot", ggmatplot(x, y, plot.type = "line", linetype = c(15, 12, "dashed")))
  # linetype values < number of groups
  expect_error(ggmatplot(x, y, plot.type = "both", linetype = c(15, 12)), "Insufficient linetype values. 3 needed but only 2 provided.")
  # linetype values > number of groups
  expect_error(ggmatplot(x, y, plot.type = "both", linetype = c(15, 12, 3, 5)), "Too many linetype values. Only 3 needed but 4 provided.")
})

test_that("plot axis limits are updated based on xlim and ylim parameters", {
  # x axis limits only
  expect_doppelganger("density plot with x axis limits", ggmatplot(y, plot.type = "density", xlim = c(3,4)))
  # y axis limits only
  expect_doppelganger("histogram with y axis limits", ggmatplot(y, plot.type = "histogram", ylim = c(0,20)))
  # x and y axis limits
  expect_doppelganger("scatterplot with a and y axis limits", ggmatplot(x,y, plot.type = "point", xlim = c(5,7),ylim = c(0,3)))
})

test_that("plot axes are transformed to a log scale based on log parameter values", {
  # log(x)
  expect_doppelganger("line plot with log x", ggmatplot(x,y, plot.type = "line", log = "x"))
  # log(y)
  expect_doppelganger("scatterplot with log y", ggmatplot(x,y, plot.type = "point", log = "y"))
  # log(x) and log(y)
  expect_doppelganger("point + line plot with log x and y", ggmatplot(x,y, plot.type = "both", log = "xy"))
})

test_that("plot title is updated based on main paramter", {
  expect_equal(ggmatplot(x,y, plot.type = "line")$labels$title, NULL)
  expect_equal(ggmatplot(x,y, plot.type = "line", main="title test")$labels$title, "title test")
})

test_that("plot axis labels are updated based on xlab and ylab paramters", {
  expect_equal(ggmatplot(x,y, plot.type = "line", xlab="xlab test")$labels$x, "xlab test")
  expect_equal(ggmatplot(y, plot.type = "point", ylab="ylab test")$labels$y, "ylab test")
})

test_that("invalid plot types are no allowed", {
  expect_error(ggmatplot(x,y, plot.type = "scatterplot"), "plot.type can not take this value")
})

test_that("legend labels are updated", {
  # legend label values = number of groups
  expect_doppelganger("plot with updated legend labels", ggmatplot(x,y, legend_label = c("lab1","lab2","lab3")))
  # legend label values < number of groups
  expect_error(ggmatplot(x,y, legend_label = c("lab1")), "Insufficient legend_label values. 3 needed but only 1 provided.")
  # legend label values > number of groups
  expect_error(ggmatplot(x,y, legend_label = c("lab1","lab2","lab3","lab4")), "Too many legend_label values. Only 3 needed but 4 provided.")
})

test_that("legend title is updated", {
  expect_doppelganger("plot with updated legend title", ggmatplot(x,y, legend_title = "Legend Title"))
})

test_that("plot is resized by aspect ratio(asp)", {
  expect_doppelganger("plot resized by aspect ratio", ggmatplot(x,y, asp = 0.5))
})

test_that("invalid parameter values throw errors", {
  # invalid shape value
  expect_error(ggmatplot(x, y, plot.type = "point", shape = "red"), "ERROR")
  # invalid linetype value
  expect_error(ggmatplot(x, y, plot.type = "line", linetype = "red"), "ERROR")
  # invalid limits
  expect_error(ggmatplot(y, plot.type = "line", xlim = c(-4,a)), "ERROR")
  expect_error(ggmatplot(y, plot.type = "line", xlim = 5), "ERROR")
  # invalid log value
  expect_error(ggmatplot(y, plot.type = "point", log = 1), "ERROR")
})

test_that("unknown parameter values for plots throw errors", {
  # shapes for plot types that don't require shapes
  expect_error(ggmatplot(y, plot.type = "density", shape = c(15, 12, 13)), "ERROR")
  # linetypes for plot types that don't require linetypes
  expect_error(ggmatplot(x, y, plot.type = "point", linetype = c(15, 12, 13)), "ERROR")
})