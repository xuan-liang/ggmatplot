test_that("matclean works", {
  d <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "1 day")
  dat2 <- data.frame(val1 = rnorm(length(d)), val2 = rnorm(length(d)))

  expect_equal(matclean(d, dat2)$data$x, rep(d, each = 2))
  expect_equal(matclean(dat2, d)$data$y, rep(d, each = 2))

})
