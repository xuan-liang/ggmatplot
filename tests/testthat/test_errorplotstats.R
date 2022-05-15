library(stats)

iris_sub <- subset(iris, Species == "setosa")[1:4]
data <- matclean(iris_sub)$data


errorstats_mean_se <- errorplotstats(data, desc_stat = "mean_se")
errorstats_mean_sd <- errorplotstats(data, desc_stat = "mean_sd")
errorstats_median_iqr <- errorplotstats(data, desc_stat = "median_iqr")
errorstats_median_range <- errorplotstats(data, desc_stat = "median_range")

test_that("format of resultant data frame is as expected", {
  expect_named(
    errorstats_mean_se,
    c("Group", "y", "ymin", "ymax")
  )
  expect_setequal(
    errorstats_mean_se$Group,
    colnames(iris_sub)
  )
})

test_that("column means are calculated as expected", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  suppressWarnings(expect_setequal(
    round(errorstats_mean_se$y,3),
    round(as.numeric(sapply(iris_sub, mean)),3)
  ))
})

test_that("column medians are calculated as expected", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_on_os("windows")
  suppressWarnings(expect_setequal(
    round(errorstats_median_iqr$y,3),
    round(as.numeric(sapply(iris_sub, median)),3)
  ))
})

test_that("column errors bars are calculated as expected", {
  # when error bars use standard errors
  suppressWarnings(expect_setequal(
    errorstats_mean_se$ymin,
    (sapply(iris_sub, mean) - (sapply(iris_sub, sd) / sqrt(nrow(iris_sub))))
  ))
  suppressWarnings(expect_setequal(
    errorstats_mean_se$ymax,
    (sapply(iris_sub, mean) + (sapply(iris_sub, sd) / sqrt(nrow(iris_sub))))
  ))

  # when error bars use standard deviations
  suppressWarnings(expect_setequal(
    errorstats_mean_sd$ymin,
    (sapply(iris_sub, mean) - (sapply(iris_sub, sd)))
  ))
  suppressWarnings(expect_setequal(
    errorstats_mean_sd$ymax,
    (sapply(iris_sub, mean) + (sapply(iris_sub, sd)))
  ))

  # when error bars use interquartile range
  suppressWarnings(expect_setequal(
    errorstats_median_iqr$ymin,
    sapply(iris_sub, quantile, probs = 0.25)
  ))
  suppressWarnings(expect_setequal(
    errorstats_median_iqr$ymax,
    sapply(iris_sub, quantile, probs = 0.75)
  ))

  # when error bars use range
  suppressWarnings(expect_setequal(
    errorstats_median_range$ymin,
    sapply(iris_sub, min)
  ))
  suppressWarnings(expect_setequal(
    errorstats_median_range$ymax,
    sapply(iris_sub, max)
  ))
})
