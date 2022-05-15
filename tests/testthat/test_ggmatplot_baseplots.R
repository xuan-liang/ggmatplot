library(vdiffr)

x <- iris[, 1]
y <- iris[, 3:4]

test_that("scatterplot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic scatterplot",
    ggmatplot(x, y, plot_type = "point")
  )
})

test_that("line plot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic line plot",
    ggmatplot(x, y, plot_type = "line")
  )
})

test_that("point + line plot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic point + line plot",
    ggmatplot(x, y, plot_type = "both")
  )
})

test_that("density plot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic density plot",
    ggmatplot(y, plot_type = "density")
  )
})

test_that("histogram draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic histogram",
    ggmatplot(y, plot_type = "histogram")
  )
})

test_that("violin plot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic violin plot",
    ggmatplot(y, plot_type = "violin")
  )
})

test_that("boxplot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic boxplot",
    ggmatplot(y, plot_type = "boxplot")
  )
})

test_that("dotplot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic dotplot",
    ggmatplot(y, plot_type = "dotplot")
  )
})

test_that("ecdf draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic ecdf",
    ggmatplot(y, plot_type = "ecdf")
  )
})

test_that("errorplot draws correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  expect_doppelganger(
    "basic errorplot",
    ggmatplot(y, plot_type = "errorplot")
  )
})

test_that("density, histogram, ecdf, dotplot, violin, errorplot and boxplot do
          not accept multiple matrices", {
  expect_error(
    ggmatplot(x, y, plot_type = "density"),
    "This plot type only uses a single matrix input"
  )
  expect_error(
    ggmatplot(x, y, plot_type = "histogram"),
    "This plot type only uses a single matrix input"
  )
  expect_error(
    ggmatplot(x, y, plot_type = "ecdf"),
    "This plot type only uses a single matrix input"
  )
  expect_error(
    ggmatplot(x, y, plot_type = "dotplot"),
    "This plot type only uses a single matrix input"
  )
  expect_error(
    ggmatplot(x, y, plot_type = "violin"),
    "This plot type only uses a single matrix input"
  )
  expect_error(
    ggmatplot(x, y, plot_type = "errorplot"),
    "This plot type only uses a single matrix input"
  )
  expect_error(
    ggmatplot(x, y, plot_type = "boxplot"),
    "This plot type only uses a single matrix input"
  )
})
