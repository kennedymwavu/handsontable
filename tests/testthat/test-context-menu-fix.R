test_that("custom context menu options are properly structured", {
  custom_options <- list(
    list(name = "Copy data", key = "copy"),
    list(name = "Paste data", key = "paste"),
    list(name = "Clear content", key = "clear")
  )

  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(
      opts = c("row_above", "row_below"),
      customOpts = custom_options
    )

  # Check that contextMenu has opts and customOpts structure
  expect_true(!is.null(ht$x$contextMenu))
  expect_equal(ht$x$contextMenu$opts, c("row_above", "row_below"))
  expect_equal(ht$x$contextMenu$customOpts, custom_options)
})

test_that("context menu works with only built-in options", {
  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(opts = c("copy", "cut", "clear_column"))

  expect_equal(ht$x$contextMenu$opts, c("copy", "cut", "clear_column"))
})

test_that("context menu uses defaults when opts is NULL", {
  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu()

  expected_defaults <- c(
    "row_above",
    "row_below",
    "col_left",
    "col_right",
    "---------",
    "remove_row",
    "remove_col",
    "clear_column",
    "undo",
    "redo",
    "cut",
    "copy",
    "---------",
    "export_csv"
  )

  expect_equal(ht$x$contextMenu$opts, expected_defaults)
})

test_that("custom options work without built-in options", {
  custom_options <- list(
    list(name = "Custom Action 1"),
    list(name = "Custom Action 2")
  )

  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(opts = character(0), customOpts = custom_options)

  expect_equal(ht$x$contextMenu$opts, character(0))
  expect_equal(ht$x$contextMenu$customOpts, custom_options)
})

test_that("separators work correctly", {
  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(opts = c("copy", "---------", "remove_row"))

  expect_equal(ht$x$contextMenu$opts, c("copy", "---------", "remove_row"))
  expect_equal(ht$x$contextMenu$opts[2], "---------")
})

