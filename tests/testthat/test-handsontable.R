test_that("handsontable creates widget with basic data", {
  ht <- handsontable(mtcars)

  expect_s3_class(ht, "htmlwidget")
  expect_equal(ht$name, "handsontable")
  expect_true(length(ht$x$data) > 0)
})

test_that("hot_cols modifies column configuration", {
  ht <- handsontable(iris) |> hot_cols(colWidths = 100)

  expect_equal(ht$x$colWidths, 100)
})

test_that("hot_validate adds validation to columns", {
  ht <- handsontable(mtcars) |>
    hot_validate(cols = 1:3, type = "numeric", min = 0)

  expect_true(!is.null(ht$x$columns))
  expect_equal(ht$x$columns[[1]]$validator$type, "numeric")
})

test_that("hot_to_r converts data correctly", {
  test_data <- list(
    c(1, 2, 3),
    c(4, 5, 6)
  )

  result <- hot_to_r(test_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
})
