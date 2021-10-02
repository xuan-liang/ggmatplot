library(vdiffr)

x <- iris[, 1]
y <- iris[, 3:4]

test_that("scatterplot draws correctly", {
  p <- ggmatplot(x, y, plot.type = "point")
  expect_doppelganger("basic scatterplot", p)
})

test_that("line plot draws correctly", {
  p <- ggmatplot(x, y, plot.type = "line")
  expect_doppelganger("basic line plot", p)
})

test_that("point + line plot draws correctly", {
  p <- ggmatplot(x, y, plot.type = "both")
  expect_doppelganger("basic point + line plot", p)
})

test_that("density plot draws correctly", {
  p <- ggmatplot(y, plot.type = "density")
  expect_doppelganger("basic density plot", p)
})

test_that("histogram draws correctly", {
  p <- ggmatplot(y, plot.type = "histogram")
  expect_doppelganger("basic histogram", p)
})

test_that("violin plot draws correctly", {
  p <- ggmatplot(y, plot.type = "violin")
  expect_doppelganger("basic violin plot", p)
})

test_that("boxplot draws correctly", {
  p <- ggmatplot(y, plot.type = "boxplot")
  expect_doppelganger("basic boxplot", p)
})

test_that("density, histogram, violin and boxplot do not accept multiple matrices", {
  expect_error(ggmatplot(x, y, plot.type = "density"), "This plot type only uses a single matrix input")
  expect_error(ggmatplot(x, y, plot.type = "histogram"), "This plot type only uses a single matrix input")
  expect_error(ggmatplot(x, y, plot.type = "violin"), "This plot type only uses a single matrix input")
  expect_error(ggmatplot(x, y, plot.type = "boxplot"), "This plot type only uses a single matrix input")
})
