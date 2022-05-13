
test_that("converts a single matrix into long format", {
  x <- data.frame(a = 1:3, b = 11:13, c = 31:33)
  outputList <- matclean(x)

  expect_equal(outputList$xname, "Observation")
  expect_equal(outputList$yname, "x")
  expect_named(outputList$data, c("Observation", "Group", "x"))


  expect_equal(outputList$data$Group, factor(rep(names(x), nrow(x))))
  expect_equal(outputList$data$x, c(1, 11, 31, 2, 12, 32, 3, 13, 33))
  expect_equal(
    outputList$data$Observation,
    unlist(lapply(1:nrow(x), function(var) rep(var, ncol(x))))
  )
})

test_that("converts two matrices(x and y) into long format
          (ncolx>ncoly & ncoly=1)", {
  x <- data.frame(a = 1:3, b = 11:13, c = 31:33)
  y <- data.frame(d = 51:53)
  outputList <- matclean(x, y)

  expect_equal(outputList$xname, "x")
  expect_equal(outputList$yname, names(y))
  expect_named(outputList$data, c(names(y), "Observation", "Group", "x"))

  expect_equal(outputList$data$Group, factor(rep(names(x), nrow(x))))
  expect_equal(outputList$data$x, c(1, 11, 31, 2, 12, 32, 3, 13, 33))
  expect_equal(
    outputList$data$Observation,
    unlist(lapply(1:nrow(x), function(var) rep(var, ncol(x))))
  )
  expect_equal(
    outputList$data$d,
    unlist(lapply(y$d, function(var) rep(var, ncol(x))))
  )
})

test_that("converts two matrices(x and y) into long format
          (ncolx<ncoly & ncolx=1)", {
  x <- data.frame(d = 51:53)
  y <- data.frame(a = 1:3, b = 11:13, c = 31:33)
  outputList <- matclean(x, y)

  expect_equal(outputList$xname, names(x))
  expect_equal(outputList$yname, "y")
  expect_named(outputList$data, c(names(x), "Observation", "Group", "y"))

  expect_equal(outputList$data$Group, factor(rep(names(y), nrow(x))))
  expect_equal(outputList$data$y, c(1, 11, 31, 2, 12, 32, 3, 13, 33))
  expect_equal(
    outputList$data$Observation,
    unlist(lapply(1:nrow(y), function(var) rep(var, ncol(y))))
  )
  expect_equal(
    outputList$data$d,
    unlist(lapply(x$d, function(var) rep(var, ncol(y))))
  )
})

test_that("converts two matrices(x and y) into long format (ncolx=ncoly)", {
  x <- data.frame(d = 51:53, e = 61:63)
  y <- data.frame(a = 1:3, b = 11:13)
  outputList <- matclean(x, y)

  expect_equal(outputList$xname, "x")
  expect_equal(outputList$yname, "y")
  expect_named(outputList$data, c("Observation", "Group", "x", "y"))

  expect_equal(
    outputList$data$Group,
    factor(unlist(lapply(
      paste0("Column ", 1:ncol(x)),
      function(var) rep(var, nrow(x))
    )))
  )
  expect_equal(outputList$data$x, c(51, 52, 53, 61, 62, 63))
  expect_equal(outputList$data$y, c(1, 2, 3, 11, 12, 13))
  expect_equal(outputList$data$Observation, 1:(nrow(x) * ncol(x)))
})

test_that("matrices of different column sizes (!ncolx=1 & !ncoly=1)
          throws an error", {
  x <- data.frame(d = 51:53, e = 61:63)
  y <- data.frame(a = 1:3, b = 11:13, c = 31:33)

  expect_error(
    matclean(x, y),
    "Either x or y must have only 1 column, or both x and y must have the same number of columns"
  )
})
