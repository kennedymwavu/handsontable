test_that("handsontable creates widget with basic data", {
  ht <- handsontable(mtcars)

  expect_s3_class(ht, "htmlwidget")
  expect_true("handsontable" %in% class(ht))
  expect_true(length(ht$x$data) > 0)
  expect_equal(length(ht$x$data), nrow(mtcars))
  expect_equal(length(ht$x$data[[1]]), ncol(mtcars))
})

test_that("hot_validate adds validation to columns", {
  ht <- handsontable(mtcars) |>
    hot_validate(col = 1:3, type = "numeric", min = 0)

  expect_true(!is.null(ht$x$columns))
  expect_equal(ht$x$columns[[1]]$validator$type, "numeric")
  expect_equal(ht$x$columns[[1]]$validator$min, 0)
  expect_equal(ht$x$columns[[1]]$validator$allowInvalid, FALSE)
  expect_equal(ht$x$columns[[2]]$validator$type, "numeric")
  expect_equal(ht$x$columns[[3]]$validator$type, "numeric")
  expect_null(ht$x$columns[[1]]$validator$strict) # strict parameter removed
})

test_that("hot_validate handles list validation", {
  ht <- handsontable(iris) |>
    hot_validate(
      col = 5,
      type = "list",
      source = c("setosa", "versicolor", "virginica")
    )

  expect_equal(ht$x$columns[[5]]$validator$type, "list")
  expect_equal(
    ht$x$columns[[5]]$validator$source,
    c("setosa", "versicolor", "virginica")
  )
  expect_equal(ht$x$columns[[5]]$validator$allowInvalid, FALSE)
})

test_that("hot_validate handles regexp validation", {
  test_data <- data.frame(email = c("test@example.com", "user@domain.org"))
  ht <- handsontable(test_data) |>
    hot_validate(
      col = 1,
      type = "regexp",
      pattern = "^[\\w\\._%+-]+@[\\w\\.-]+\\.[A-Za-z]{2,}$"
    )

  expect_equal(ht$x$columns[[1]]$validator$type, "regexp")
  expect_equal(
    ht$x$columns[[1]]$validator$pattern,
    "^[\\w\\._%+-]+@[\\w\\.-]+\\.[A-Za-z]{2,}$"
  )
  expect_equal(ht$x$columns[[1]]$validator$allowInvalid, FALSE)
})

test_that("hot_to_r converts data correctly", {
  test_data <- list(
    data = list(
      c(1, 2, 3),
      c(4, 5, 6)
    ),
    colnames = c("x1", "x2", "x3")
  )

  result <- hot_to_r(test_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("x1", "x2", "x3"))
})

test_that("hot_to_r handles column names", {
  test_data <- list(
    data = list(
      c("Alice", 25, "Engineer"),
      c("Bob", 30, "Manager")
    ),
    colnames = c("Name", "Age", "Role")
  )

  result <- hot_to_r(test_data)
  expect_equal(names(result), c("Name", "Age", "Role"))
  expect_equal(result$Name, c("Alice", "Bob"))
})

test_that("hot_to_r handles empty data", {
  result <- hot_to_r(list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})

# Tests for general table options
test_that("handsontable configures general table options", {
  ht <- handsontable(
    iris,
    filters = TRUE,
    manualColumnSorting = TRUE,
    search = TRUE,
    undo = TRUE
  ) |>
    hot_context_menu()

  expect_true(!is.null(ht$x$contextMenu))
  expect_true(ht$x$filters)
  expect_true(ht$x$manualColumnSorting)
  expect_true(ht$x$search)
  expect_true(ht$x$undo)
})


test_that("handsontable removes NULL values", {
  ht <- handsontable(iris, filters = NULL, search = FALSE)

  expect_false(ht$x$search)
  expect_null(ht$x$filters)
})

# Tests for hot_context_menu function - these need updating for new API
# Remove these old tests as they use the deprecated API

# Tests for hot_col function
test_that("hot_col configures individual column properties", {
  ht <- handsontable(iris) |>
    hot_col(col = 1, type = "numeric", width = 100, readOnly = TRUE)

  expect_equal(ht$x$columns[[1]]$type, "numeric")
  expect_equal(ht$x$columns[[1]]$width, 100)
  expect_true(ht$x$columns[[1]]$readOnly)
})

test_that("hot_col handles dropdown column type", {
  ht <- handsontable(iris) |>
    hot_col(
      col = 5,
      type = "dropdown",
      source = c("setosa", "versicolor", "virginica"),
      strict = TRUE
    )

  expect_equal(ht$x$columns[[5]]$type, "dropdown")
  expect_equal(ht$x$columns[[5]]$source, c("setosa", "versicolor", "virginica"))
  expect_true(ht$x$columns[[5]]$strict)
})

test_that("hot_col handles checkbox column type", {
  test_df <- data.frame(active = c(TRUE, FALSE, TRUE))
  ht <- handsontable(test_df) |>
    hot_col(
      col = 1,
      type = "checkbox",
      checkedTemplate = 1,
      uncheckedTemplate = 0
    )

  expect_equal(ht$x$columns[[1]]$type, "checkbox")
  expect_equal(ht$x$columns[[1]]$checkedTemplate, 1)
  expect_equal(ht$x$columns[[1]]$uncheckedTemplate, 0)
})

test_that("hot_col works with column names", {
  ht <- handsontable(iris, colHeaders = TRUE) |>
    hot_col(col = "Sepal.Length", type = "numeric", format = "0.00")

  expect_equal(ht$x$columns[[1]]$type, "numeric")
  expect_equal(ht$x$columns[[1]]$format, "0.00")
})

test_that("hot_col throws error for invalid column name", {
  ht <- handsontable(iris, colHeaders = TRUE)

  expect_error(
    hot_col(ht, col = "NonExistent", type = "text"),
    "Column `NonExistent` not found in `colHeaders`"
  )
})

# Tests for hidden columns functionality
test_that("hot_col hides column by setting width to 0.1", {
  ht <- handsontable(iris) |>
    hot_col(col = 2, hidden = TRUE)

  expect_equal(ht$x$columns[[2]]$width, 0.1)
})

test_that("hot_col overrides user width when hidden is TRUE", {
  ht <- handsontable(iris) |>
    hot_col(col = 3, width = 200, hidden = TRUE)

  expect_equal(ht$x$columns[[3]]$width, 0.1)
})

test_that("hot_col respects user width when hidden is FALSE", {
  ht <- handsontable(iris) |>
    hot_col(col = 1, width = 150, hidden = FALSE)

  expect_equal(ht$x$columns[[1]]$width, 150)
})

test_that("hot_col hidden defaults to FALSE and preserves width", {
  ht <- handsontable(iris) |>
    hot_col(col = 4, width = 100)

  expect_equal(ht$x$columns[[4]]$width, 100)
})

test_that("hot_col can hide multiple columns", {
  ht <- handsontable(iris) |>
    hot_col(col = 1, hidden = TRUE) |>
    hot_col(col = 3, hidden = TRUE) |>
    hot_col(col = 5, hidden = TRUE)

  expect_equal(ht$x$columns[[1]]$width, 0.1)
  expect_equal(ht$x$columns[[3]]$width, 0.1)
  expect_equal(ht$x$columns[[5]]$width, 0.1)
})

test_that("hot_col hidden works with column names", {
  ht <- handsontable(iris, colHeaders = TRUE) |>
    hot_col(col = "Sepal.Width", hidden = TRUE)

  expect_equal(ht$x$columns[[2]]$width, 0.1)
})

# Tests for hot_row function
test_that("hot_row configures individual row properties", {
  ht <- handsontable(iris) |>
    hot_row(row = 1, readOnly = TRUE)

  expect_true(!is.null(ht$x$rowConfig))
  expect_true(ht$x$rowConfig[["0"]]$readOnly) # 0-based indexing for JS
  expect_true(!is.null(ht$x$cells))
})

test_that("hot_row handles multiple row configurations", {
  ht <- handsontable(iris) |>
    hot_row(row = 1, readOnly = TRUE) |>
    hot_row(row = 3, readOnly = FALSE)

  expect_true(ht$x$rowConfig[["0"]]$readOnly) # Row 1 -> JS index 0
  expect_false(ht$x$rowConfig[["2"]]$readOnly) # Row 3 -> JS index 2
  expect_true(!is.null(ht$x$cells))
})

# Tests for handsontable core function edge cases
test_that("handsontable handles matrix input", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  ht <- handsontable(mat)

  expect_s3_class(ht, "htmlwidget")
  expect_equal(length(ht$x$data), 3)
  expect_equal(length(ht$x$data[[1]]), 4)
})

test_that("handsontable throws error for invalid input", {
  expect_error(
    handsontable(c(1, 2, 3)),
    "data must be a data.frame or matrix"
  )
})

test_that("handsontable handles custom column headers", {
  custom_headers <- c("Col1", "Col2", "Col3", "Col4", "Col5")
  ht <- handsontable(iris, colHeaders = custom_headers)

  expect_equal(ht$x$colHeaders, custom_headers)
})

test_that("handsontable handles factor conversion", {
  test_df <- data.frame(
    category = factor(c("A", "B", "C")),
    value = c(1, 2, 3),
    stringsAsFactors = TRUE
  )

  ht <- handsontable(test_df)

  expect_equal(ht$x$data[[1]][[1]], "A")
  expect_equal(ht$x$data[[2]][[1]], "B")
  expect_equal(ht$x$data[[3]][[1]], "C")
})

# Tests for Shiny integration functions
test_that("handsontableOutput creates proper output", {
  output <- handsontableOutput("test_table", width = "500px", height = "300px")

  expect_s3_class(output, "shiny.tag.list")
  expect_true(length(output) > 0)
  expect_s3_class(output[[1]], "shiny.tag")
  expect_equal(output[[1]]$name, "div")
})

test_that("renderHandsontable creates render function", {
  render_func <- renderHandsontable({
    handsontable(mtcars)
  })

  expect_s3_class(render_func, "shiny.render.function")
})

# Tests for set_data function
test_that("set_data requires Shiny session", {
  expect_error(
    set_data("table", 1, 1, "value", session = NULL),
    "set_data must be called within a Shiny session"
  )
})

# Tests for JavaScript change detection
test_that("JavaScript includes change detection hooks", {
  js_file <- system.file(
    "htmlwidgets",
    "handsontable.js",
    package = "handsontable"
  )
  js_content <- readLines(js_file, warn = FALSE)
  js_text <- paste(js_content, collapse = "\n")

  # Check for key event hooks
  expect_true(grepl("afterChange", js_text))
  expect_true(grepl("afterCreateRow", js_text))
  expect_true(grepl("afterRemoveRow", js_text))
  expect_true(grepl("afterSelection", js_text))
  expect_true(grepl("Shiny.setInputValue", js_text))
})
