#' Oxford Paste
#'
#' @param x Character vector.
#' @param border String. Padded at the start and end of each
#' item in `x`.
#' @param sep String. Padded after each item in `x`, unless
#' it's the last.
#' @return String.
#' @keywords internal
#' @noRd
oxford_paste <- function(x, border = "`", sep = ", ") {
  if (identical(length(x), 0L)) {
    return(x)
  }

  x <- paste0(border, x, border)

  if (identical(length(x), 1L)) {
    return(x)
  }

  if (identical(length(x), 2L)) {
    return(
      paste(x, collapse = " and ")
    )
  }

  res <- ""
  for (idx in seq_along(x)) {
    if (identical(idx, length(x))) {
      sep <- paste0(sep, "and ")
    }

    res <- paste0(res, sep, x[idx])
    if (identical(idx, 1L)) {
      res <- x[1L]
    }
  }

  res
}

#' Get Column Indices
#'
#' @param hot A handsontable widget object.
#' @param col Character vector of column names or numeric vector
#' of column indices.
#' @return Integer vector.
#' @keywords internal
#' @noRd
get_col_idx <- function(hot, col) {
  if (is.character(col) && is.null(hot$x$colHeaders)) {
    stop("Column names can only be used when `colHeaders` are defined.")
  }

  col_idx <- col
  if (is.character(col)) {
    col_idx <- match(col, hot$x$colHeaders)
    missing_idx <- which(x = is.na(col_idx))
    missing_cols <- col[missing_idx]
    if (length(missing_cols)) {
      msg <- paste(
        "Column",
        oxford_paste(missing_cols),
        "not found in `colHeaders`."
      )

      stop(msg, call. = FALSE)
    }
  }

  col_idx
}

#' Configure Data Validation
#'
#' @param hot A handsontable widget object
#' @param col Column indices to apply validation (numeric vector)
#' @param type Validation type eg. "numeric", "list", "regexp"
#' @param allowInvalid Logical, allow invalid values to be entered
#' @param ... Additional validation options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Data Validation"),
#'     handsontableOutput("table")
#'   )
#'
#'   server <- function(input, output, session) {
#'     df <- data.frame(
#'       score = c(85, 92, 78, 95, 88),
#'       grade = c("A", "A", "B", "A", "B"),
#'       email = c(
#'         "john@test.com", "jane@test.com", "bob@test.com",
#'         "alice@test.com", "charlie@test.com"
#'       )
#'     )
#'
#'     output$table <- renderHandsontable({
#'       handsontable(df) |>
#'         hot_validate(col = 1, type = "numeric", min = 0, max = 100) |>
#'         hot_validate(
#'           col = 2,
#'           type = "list",
#'           source = c("A", "B", "C", "D", "F")
#'         ) |>
#'         hot_validate(
#'           col = 3,
#'           type = "regexp",
#'           pattern = "^[\\w\\._%+-]+@[\\w\\.-]+\\.[A-Za-z]{2,}$"
#'         )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
hot_validate <- function(
  hot,
  col,
  type = NULL,
  allowInvalid = FALSE,
  ...
) {
  col_idx <- get_col_idx(hot, col)

  validator <- list(
    type = type,
    allowInvalid = allowInvalid,
    ...
  )

  # Create columns configuration if it doesn't exist
  if (is.null(hot$x$columns)) {
    # Determine number of columns from data
    ncols <- if (!is.null(hot$x$data) && length(hot$x$data)) {
      length(hot$x$data[[1]])
    } else {
      max(col_idx)
    }
    hot$x$columns <- vector(mode = "list", length = ncols)
  }

  # Apply validation to specified columns
  for (idx in col_idx) {
    if (is.null(hot$x$columns[[idx]])) {
      hot$x$columns[[idx]] <- list()
    }
    hot$x$columns[[idx]]$validator <- validator
  }

  hot
}

#' Configure Context Menu
#'
#' @param hot A handsontable widget object
#' @param opts Character vector of built-in context menu options.
#' If NULL, uses default options:
#' - "row_above",
#' - "row_below",
#' - "col_left",
#' - "col_right",
#' - "---------",
#' - "remove_row",
#' - "remove_col",
#' - "clear_column",
#' - "make_read_only",
#' - "undo",
#' - "redo",
#' - "cut",
#' - "copy",
#' - "---------",
#' - "export_csv"
#'
#' @param customOpts List of custom context menu options.
#' Each custom option should be a list with elements like
#' `name`, `callback`, `disabled`, etc. Example:
#'   ```r
#'   list(
#'     list(
#'       name = "Custom Action",
#'       callback = htmlwidgets::JS("function() {...}")
#'     )
#'   )
#'   ```
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   # Basic usage with default options
#'   ui <- fluidPage(
#'     titlePanel("Context Menu Configuration"),
#'     handsontableOutput("table1"),
#'     br(),
#'     handsontableOutput("table2"),
#'     br(),
#'     handsontableOutput("table3")
#'   )
#'
#'   server <- function(input, output, session) {
#'     # Default context menu
#'     output$table1 <- renderHandsontable({
#'       handsontable(mtcars[1:5, 1:4]) |>
#'         hot_context_menu()
#'     })
#'
#'     # Custom selection of built-in options
#'     output$table2 <- renderHandsontable({
#'       handsontable(mtcars[1:5, 1:4]) |>
#'         hot_context_menu(opts = c("row_above", "row_below", "---------", "copy", "cut"))
#'     })
#'
#'     # With custom options
#'     output$table3 <- renderHandsontable({
#'       handsontable(mtcars[1:5, 1:4]) |>
#'         hot_context_menu(
#'           opts = c("copy", "cut"),
#'           customOpts = list(
#'             list(
#'               name = "Alert",
#'               callback = htmlwidgets::JS("function() { alert('Custom action!'); }")
#'             )
#'           )
#'         )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
hot_context_menu <- function(hot, opts = NULL, customOpts = NULL) {
  # Default context menu options
  if (is.null(opts)) {
    opts <- c(
      "row_above",
      "row_below",
      "col_left",
      "col_right",
      "---------",
      "remove_row",
      "remove_col",
      "clear_column",
      "make_read_only",
      "undo",
      "redo",
      "cut",
      "copy",
      "---------",
      "export_csv"
    )
  }

  context_menu <- list(opts = opts)
  if (!is.null(customOpts)) {
    context_menu$customOpts <- customOpts
  }

  # Set context menu configuration
  hot$x$contextMenu <- context_menu

  hot
}

#' Convert Handsontable Data to R
#'
#' Helper function to convert JavaScript Handsontable data back to R format.
#' Primarily used in Shiny applications. Automatically uses original column names
#' sent from the Handsontable widget.
#'
#' @param data Object. Handsontable input object from Shiny (contains data and colnames)
#'
#' @return data.frame
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Data Conversion Demo"),
#'     handsontableOutput("input_table"),
#'     br(),
#'     h4("Converted R Data:"),
#'     verbatimTextOutput("converted_data")
#'   )
#'
#'   server <- function(input, output, session) {
#'     initial_data <- data.frame(
#'       Name = c("John", "Jane", "Bob"),
#'       Age = c(25, 30, 35),
#'       Role = c("Engineer", "Manager", "Developer")
#'     )
#'
#'     output$input_table <- renderHandsontable({
#'       handsontable(initial_data)
#'     })
#'
#'     output$converted_data <- renderPrint({
#'       if (!is.null(input$input_table)) {
#'         # Column names are automatically preserved!
#'         converted <- hot_to_r(input$input_table)
#'         str(converted)
#'         converted
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
hot_to_r <- function(data) {
  if (!length(data) || !length(data$data)) {
    return(data.frame())
  }

  df <- lapply(X = data$data, FUN = \(row) {
    x <- lapply(row, \(value) {
      if (is.null(value)) {
        value <- NA
      }
      value
    }) |>
      data.frame()

    if (ncol(x)) {
      names(x) <- paste0("x", seq_along(x))
    }
    x
  }) |>
    do.call(what = rbind)

  # Use the original column names sent from JavaScript
  if (!is.null(data$colnames) && identical(length(data$colnames), ncol(df))) {
    names(df) <- data$colnames
  }

  df
}

#' Configure Individual Column
#'
#' @param hot A handsontable widget object
#' @param col Character vector or Integer vector.
#' Column name or index to configure.
#' @param readOnly Logical, make column read-only
#' @param hidden Logical. Should the column be hidden?
#' @param width Column width in pixels
#' @param ... Additional column configuration options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Individual Column Configuration"),
#'     handsontableOutput("table")
#'   )
#'
#'   server <- function(input, output, session) {
#'     df <- data.frame(
#'       ID = 1:5,
#'       Name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
#'       Score = c(85.5, 92.3, 78.1, 95.7, 88.9),
#'       Grade = c("B", "A", "C", "A", "B"),
#'       Active = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#'     )
#'
#'     output$table <- renderHandsontable({
#'       handsontable(df) |>
#'         hot_col(col = 1, readOnly = TRUE, width = 60) |>
#'         hot_col(col = 3, type = "numeric", format = "0.0") |>
#'         hot_col(
#'           col = 4, type = "dropdown",
#'           source = c("A", "B", "C", "D", "F")
#'         ) |>
#'         hot_col(col = 5, type = "checkbox")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
hot_col <- function(
  hot,
  col,
  readOnly = NULL,
  hidden = FALSE,
  width = NULL,
  ...
) {
  col_idx <- get_col_idx(hot, col)

  # Create columns configuration if it doesn't exist
  if (is.null(hot$x$columns)) {
    # Determine number of columns from data or colHeaders
    ncols <- if (!is.null(hot$x$colHeaders)) {
      length(hot$x$colHeaders)
    } else if (!is.null(hot$x$data) && length(hot$x$data) > 0) {
      length(hot$x$data[[1]])
    } else {
      max(col_idx)
    }
    hot$x$columns <- vector("list", ncols)
  }

  # Ensure we have enough columns
  if (col_idx > length(hot$x$columns)) {
    length(hot$x$columns) <- col_idx
  }

  # Set column properties
  col_config <- list(
    readOnly = readOnly,
    width = if (hidden) 0.1 else width,
    ...
  ) |>
    Filter(f = Negate(is.null))

  for (idx in col_idx) {
    # Initialize column config if NULL
    if (is.null(hot$x$columns[[idx]])) {
      hot$x$columns[[idx]] <- list()
    }

    # Merge with existing column configuration
    hot$x$columns[[idx]] <- utils::modifyList(
      hot$x$columns[[idx]],
      col_config
    )
  }

  hot
}

#' Configure Row Properties
#'
#' @param hot A handsontable widget object
#' @param row Integer vector. Row indices to configure
#' @param readOnly Logical, make row read-only
#' @param ... Additional row configuration options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Row Configuration"),
#'     p("Rows 1, 3, and 5 are read-only"),
#'     handsontableOutput("table")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$table <- renderHandsontable({
#'       hot_table <- handsontable(iris[1:8, ])
#'       for (i in c(1, 3, 5)) {
#'         hot_table <- hot_table |> hot_row(row = i, readOnly = TRUE)
#'       }
#'       hot_table
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
hot_row <- function(hot, row, readOnly = NULL, ...) {
  # Store row configuration for cells function
  if (is.null(hot$x$rowConfig)) {
    hot$x$rowConfig <- list()
  }

  # Build row configuration
  row_config <- list(
    readOnly = readOnly,
    ...
  ) |>
    Filter(f = Negate(is.null))

  if (length(row_config)) {
    # Convert 1-based R index to 0-based JavaScript index
    js_row <- row - 1

    for (idx in js_row) {
      hot$x$rowConfig[[as.character(idx)]] <- row_config
    }

    # Create cells function that applies row configurations
    hot$x$cells <- htmlwidgets::JS(
      paste0(
        "function(row, col, prop) {",
        "  var cellProperties = {};",
        "  var rowConfig = ",
        jsonlite::toJSON(hot$x$rowConfig, auto_unbox = TRUE),
        ";",
        "  if (rowConfig[row]) {",
        "    Object.assign(cellProperties, rowConfig[row]);",
        "  }",
        "  return cellProperties;",
        "}"
      )
    )
  }

  hot
}

#' Update Cell Data Programmatically
#'
#' @param id String. Output ID of the handsontable widget.
#' @param row Integer vector. Row index.
#' @param col Integer vector. Column index.
#' @param val Any. Value to set in the cell(s).
#' @param session Shiny session object
#'
#' @return NULL (called for side effects)
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Programmatic Cell Updates"),
#'     handsontableOutput("my_table"),
#'     br(),
#'     actionButton("update_btn", "Update Cell (1,1) & (2,1)")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$my_table <- renderHandsontable({
#'       handsontable(data.frame(
#'         A = c(1, 2, 3),
#'         B = c("a", "b", "c")
#'       ))
#'     })
#'
#'     observeEvent(input$update_btn, {
#'       set_data("my_table", row = 1:2, col = 1, val = runif(2, 100, 999))
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
set_data <- function(
  id,
  row,
  col,
  val,
  session = shiny::getDefaultReactiveDomain()
) {
  if (is.null(session)) {
    stop("set_data must be called within a Shiny session")
  }

  max_len <- max(lengths(list(row, col, val)))
  row <- rep(row, length.out = max_len)
  col <- rep(col, length.out = max_len)
  val <- rep(val, length.out = max_len)

  # Convert 1-based R indexing to 0-based JavaScript indexing
  js_row <- row - 1
  js_col <- col - 1

  # Send update message to client
  session$sendCustomMessage(
    type = "handsontable-set-data",
    message = list(
      id = session$ns(id),
      row = js_row,
      col = js_col,
      value = val
    )
  )
}
