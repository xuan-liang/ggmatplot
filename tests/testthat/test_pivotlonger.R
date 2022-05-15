test_that("can pivot columns to long format based on a single id column", {
  df <- data.frame(x = 1:3, y = 11:13, z = 31:33)
  pv <- pivotlonger(df, names_to = "Group", values_to = "Value", id_cols = 1)

  expect_named(pv, c("x", "Group", "Value"))
  expect_equal(pv$Group, rep(names(df[, 2:3]), nrow(df)))
  expect_equal(pv$Value, c(11, 31, 12, 32, 13, 33))
})

test_that("can pivot columns to long format based on a multiple id columns", {
  df <- data.frame(x = 1:3, y = 11:13, z = 31:33, a = 41:43, b = 51:53)
  pv <- pivotlonger(df,
    names_to = "Group", values_to = "Value",
    id_cols = c(1, 2, 3)
  )

  expect_named(pv, c("x", "y", "z", "Group", "Value"))
  expect_equal(pv$Group, rep(names(df[, 4:5]), nrow(df)))
  expect_equal(pv$Value, c(41, 51, 42, 52, 43, 53))
})

test_that("NA values are preserved in both id columns and pivoted columns", {
  df <- data.frame(x = c(1, 2, NA), y = c(NA, 2, 3), z = c(3, 4, 5))
  pv <- pivotlonger(df, names_to = "Group", values_to = "Value", id_cols = 1)

  expect_named(pv, c("x", "Group", "Value"))
  expect_equal(pv$x, c(1, 1, 2, 2, NA, NA))
  expect_equal(pv$Group, rep(names(df[, 2:3]), nrow(df)))
  expect_equal(pv$Value, c(NA, 3, 2, 4, 3, 5))
})

test_that("id columns can have different data types", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), z = 31:33, stringsAsFactors = FALSE)
  pv <- pivotlonger(df,
    names_to = "Group",
    values_to = "Value",
    id_cols = c(1, 2)
  )

  expect_named(pv, c("x", "y", "Group", "Value"))
  expect_equal(pv$x, c(1, 2, 3))
  expect_equal(pv$y, c("a", "b", "c"))
  expect_equal(pv$Group, rep(names(df[, 3, drop = FALSE]), nrow(df)))
  expect_equal(pv$Value, c(31, 32, 33))
})
