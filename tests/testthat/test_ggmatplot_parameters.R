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

test_that("invalid parameter values throw errors", {
  # invalid shape value
  expect_error(ggmatplot(x, y, plot.type = "point", shape = "red"), "ERROR")
  # invalid shape value
  expect_error(ggmatplot(x, y, plot.type = "line", linetype = "red"), "ERROR")
})

test_that("unknown parameter values for plots throw errors", {
  # shapes for plot types that don't require shapes
  expect_error(ggmatplot(y, plot.type = "density", shape = c(15, 12, 13)), "ERROR")
  # linetypes for plot types that don't require linetypes
  expect_error(ggmatplot(x, y, plot.type = "point", linetype = c(15, 12, 13)), "ERROR")
})
