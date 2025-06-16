test_that("custom context menu options are properly structured", {
  custom_options <- list(
    "copy" = list(name = "Copy data"),
    "paste" = list(name = "Paste data"),
    "sep1" = "---------",
    "clear" = list(name = "Clear content")
  )
  
  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(customOpts = custom_options)
  
  # Check that customOpts is stored correctly
  expect_true(is.list(ht$x$contextMenu))
  expect_true(!is.null(ht$x$contextMenu$customOpts))
  expect_equal(ht$x$contextMenu$customOpts, custom_options)
  
  # Check specific custom options
  expect_equal(ht$x$contextMenu$customOpts$copy$name, "Copy data")
  expect_equal(ht$x$contextMenu$customOpts$paste$name, "Paste data")
  expect_equal(ht$x$contextMenu$customOpts$sep1, "---------")
  expect_equal(ht$x$contextMenu$customOpts$clear$name, "Clear content")
})

test_that("context menu works without custom options", {
  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  
  expect_true(is.list(ht$x$contextMenu))
  expect_true(ht$x$contextMenu$allowRowEdit)
  expect_false(ht$x$contextMenu$allowColEdit)
  expect_null(ht$x$contextMenu$customOpts)
})

test_that("vignette example format works", {
  # Test the exact format from the vignette
  custom_options <- list(
    "copy" = list(name = "Copy data"),
    "paste" = list(name = "Paste data"),
    "sep1" = "---------",
    "clear" = list(name = "Clear content")
  )
  
  ht <- handsontable(iris[1:5, 1:3]) |>
    hot_context_menu(customOpts = custom_options)
  
  expect_true(is.list(ht$x$contextMenu$customOpts))
  expect_length(ht$x$contextMenu$customOpts, 4)
  expect_named(ht$x$contextMenu$customOpts, c("copy", "paste", "sep1", "clear"))
})

test_that("boolean TRUE values work for built-in actions", {
  # Test the new boolean TRUE format for built-in actions
  builtin_options <- list(
    "copy" = TRUE,
    "paste" = TRUE,
    "sep1" = "---------",
    "clear_column" = TRUE,
    "remove_row" = TRUE
  )
  
  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(customOpts = builtin_options)
  
  expect_true(is.list(ht$x$contextMenu$customOpts))
  expect_length(ht$x$contextMenu$customOpts, 5)
  expect_true(ht$x$contextMenu$customOpts$copy)
  expect_true(ht$x$contextMenu$customOpts$paste)
  expect_equal(ht$x$contextMenu$customOpts$sep1, "---------")
  expect_true(ht$x$contextMenu$customOpts$clear_column)
  expect_true(ht$x$contextMenu$customOpts$remove_row)
})

test_that("FALSE values are excluded from context menu", {
  # Test that FALSE values are properly excluded
  mixed_options <- list(
    "copy" = TRUE,
    "paste" = FALSE,  # This should be excluded
    "clear_column" = TRUE
  )
  
  ht <- handsontable(iris[1:3, 1:3]) |>
    hot_context_menu(customOpts = mixed_options)
  
  expect_true(is.list(ht$x$contextMenu$customOpts))
  expect_length(ht$x$contextMenu$customOpts, 3)  # paste should be excluded in JS transformation
  expect_true(ht$x$contextMenu$customOpts$copy)
  expect_false(ht$x$contextMenu$customOpts$paste)  # Still in R config
  expect_true(ht$x$contextMenu$customOpts$clear_column)
})