#' Configure Handsontable Columns
#'
#' @param hot A handsontable widget object
#' @param colWidths Numeric vector or single value for column widths
#' @param fixedColumnsLeft Number of columns to freeze on the left
#' @param manualColumnResize Logical, enable manual column resizing
#' @param manualColumnMove Logical, enable manual column reordering
#' @param ... Additional column configuration options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Column Configuration"),
#'   handsontableOutput("table")
#' )
#'
#' server <- function(input, output, session) {
#'   output$table <- renderHandsontable({
#'     handsontable(mtcars[1:10, 1:6]) |>
#'       hot_cols(colWidths = 120, fixedColumnsLeft = 2,
#'                manualColumnMove = TRUE)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
hot_cols <- function(
  hot,
  colWidths = NULL,
  fixedColumnsLeft = 0,
  manualColumnResize = TRUE,
  manualColumnMove = FALSE,
  ...
) {
  config <- list(
    colWidths = colWidths,
    fixedColumnsLeft = fixedColumnsLeft,
    manualColumnResize = manualColumnResize,
    manualColumnMove = manualColumnMove,
    ...
  ) |>
    Filter(f = Negate(is.null))

  # Merge with existing configuration
  hot$x <- utils::modifyList(hot$x, config)

  hot
}

#' Configure Handsontable Rows
#'
#' @param hot A handsontable widget object
#' @param fixedRowsTop Number of rows to freeze at the top
#' @param fixedRowsBottom Number of rows to freeze at the bottom
#' @param manualRowResize Logical, enable manual row resizing
#' @param manualRowMove Logical, enable manual row reordering
#' @param ... Additional row configuration options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Row Configuration"),
#'   handsontableOutput("table")
#' )
#'
#' server <- function(input, output, session) {
#'   output$table <- renderHandsontable({
#'     handsontable(mtcars[1:15, 1:5]) |>
#'       hot_rows(fixedRowsTop = 2, manualRowMove = TRUE,
#'                manualRowResize = TRUE)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
hot_rows <- function(
  hot,
  fixedRowsTop = 0,
  fixedRowsBottom = 0,
  manualRowResize = TRUE,
  manualRowMove = FALSE,
  ...
) {
  config <- list(
    fixedRowsTop = fixedRowsTop,
    fixedRowsBottom = fixedRowsBottom,
    manualRowResize = manualRowResize,
    manualRowMove = manualRowMove,
    ...
  ) |>
    Filter(f = Negate(is.null))

  # Merge with existing configuration
  hot$x <- utils::modifyList(hot$x, config)

  hot
}

#' Configure General Table Options
#'
#' @param hot A handsontable widget object
#' @param contextMenu Logical or list, enable/configure context menu
#' @param dropdownMenu Logical or list, enable/configure dropdown menu
#' @param filters Logical, enable column filters
#' @param manualColumnSorting Logical, enable manual column sorting
#' @param multiColumnSorting Logical, enable multi-column sorting
#' @param search Logical, enable search functionality
#' @param undo Logical, enable undo/redo functionality
#' @param outsideClickDeselects Logical, deselect on outside click
#' @param fillHandle Logical or object, configure fill handle
#' @param themeName Character, theme name (e.g., "ht-theme-main-dark-auto")
#' @param ... Additional table configuration options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Table Features"),
#'   handsontableOutput("table")
#' )
#'
#' server <- function(input, output, session) {
#'   output$table <- renderHandsontable({
#'     handsontable(iris[1:20, ]) |>
#'       hot_table(
#'         contextMenu = TRUE,
#'         filters = TRUE,
#'         manualColumnSorting = TRUE,
#'         search = TRUE,
#'         undo = TRUE
#'       )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
hot_table <- function(
  hot,
  contextMenu = NULL,
  dropdownMenu = NULL,
  filters = NULL,
  manualColumnSorting = NULL,
  multiColumnSorting = NULL,
  search = NULL,
  undo = NULL,
  outsideClickDeselects = NULL,
  fillHandle = NULL,
  themeName = NULL,
  ...
) {
  config <- list(
    contextMenu = contextMenu,
    dropdownMenu = dropdownMenu,
    filters = filters,
    manualColumnSorting = manualColumnSorting,
    multiColumnSorting = multiColumnSorting,
    search = search,
    undo = undo,
    outsideClickDeselects = outsideClickDeselects,
    fillHandle = fillHandle,
    themeName = themeName,
    ...
  ) |>
    Filter(f = Negate(is.null))

  # Merge with existing configuration
  hot$x <- utils::modifyList(hot$x, config)

  hot
}

#' Configure Data Validation
#'
#' @param hot A handsontable widget object
#' @param cols Column indices to apply validation (numeric vector)
#' @param type Validation type: "numeric", "list", "regexp"
#' @param source For "list" type, vector of allowed values
#' @param pattern For "regexp" type, regular expression pattern
#' @param min,max For "numeric" type, minimum and maximum values
#' @param allowInvalid Logical, allow invalid values to be entered
#' @param strict Logical, strict validation mode
#' @param ... Additional validation options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Data Validation"),
#'   handsontableOutput("table")
#' )
#'
#' server <- function(input, output, session) {
#'   df <- data.frame(
#'     score = c(85, 92, 78, 95, 88),
#'     grade = c("A", "A", "B", "A", "B"),
#'     email = c("john@test.com", "jane@test.com", "bob@test.com",
#'               "alice@test.com", "charlie@test.com")
#'   )
#'
#'   output$table <- renderHandsontable({
#'     handsontable(df) |>
#'       hot_validate(cols = 1, type = "numeric", min = 0, max = 100) |>
#'       hot_validate(cols = 2, type = "list",
#'                    source = c("A", "B", "C", "D", "F")) |>
#'       hot_validate(cols = 3, type = "regexp",
#'                    pattern = "^[\\w\\._%+-]+@[\\w\\.-]+\\.[A-Za-z]{2,}$")
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
hot_validate <- function(
  hot,
  cols,
  type = c("numeric", "list", "regexp"),
  source = NULL,
  pattern = NULL,
  min = NULL,
  max = NULL,
  allowInvalid = TRUE,
  strict = FALSE,
  ...
) {
  type <- match.arg(arg = type)

  validator <- list(
    type = type,
    allowInvalid = allowInvalid,
    strict = strict,
    ...
  )

  # Add type-specific options
  switch(
    EXPR = type,
    numeric = {
      if (!is.null(min)) {
        validator$min <- min
      }

      if (!is.null(max)) {
        validator$max <- max
      }
    },
    list = {
      if (!is.null(source)) {
        validator$source <- source
      }
    },
    regexp = {
      if (!is.null(pattern)) {
        validator$pattern <- pattern
      }
    }
  )

  # Create columns configuration if it doesn't exist
  if (is.null(hot$x$columns)) {
    # Determine number of columns from data
    ncols <- if (!is.null(hot$x$data) && length(hot$x$data) > 0) {
      length(hot$x$data[[1]])
    } else {
      max(cols)
    }
    hot$x$columns <- vector("list", ncols)
  }

  # Apply validation to specified columns
  for (col in cols) {
    if (is.null(hot$x$columns[[col]])) {
      hot$x$columns[[col]] <- list()
    }
    hot$x$columns[[col]]$validator <- validator
  }

  hot
}

#' Configure Context Menu
#'
#' @param hot A handsontable widget object
#' @param allowRowEdit Logical, allow row operations in context menu
#' @param allowColEdit Logical, allow column operations in context menu
#' @param customOpts List of custom context menu options
#' @param ... Additional context menu options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Context Menu Configuration"),
#'   handsontableOutput("table")
#' )
#'
#' server <- function(input, output, session) {
#'   output$table <- renderHandsontable({
#'     handsontable(mtcars[1:8, 1:5]) |>
#'       hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
hot_context_menu <- function(
  hot,
  allowRowEdit = TRUE,
  allowColEdit = TRUE,
  customOpts = NULL,
  ...
) {
  if (is.logical(hot$x$contextMenu) && !hot$x$contextMenu) {
    # If context menu is disabled, enable it first
    hot$x$contextMenu <- TRUE
  }

  menu_config <- list(
    allowRowEdit = allowRowEdit,
    allowColEdit = allowColEdit,
    ...
  )

  if (!is.null(customOpts)) {
    menu_config$customOpts <- customOpts
  }

  # If contextMenu is TRUE, convert to configuration object
  if (isTRUE(hot$x$contextMenu)) {
    hot$x$contextMenu <- menu_config
  } else if (is.list(hot$x$contextMenu)) {
    hot$x$contextMenu <- utils::modifyList(hot$x$contextMenu, menu_config)
  }

  hot
}

#' Convert Handsontable Data to R
#'
#' Helper function to convert JavaScript Handsontable data back to R format.
#' Primarily used in Shiny applications.
#'
#' @param data List. Handsontable data (typically from Shiny input)
#' @param colnames Character vector. Optional column names
#' @param stringsAsFactors Logical. Convert strings to factors?
#'
#' @return data.frame
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Data Conversion Demo"),
#'   handsontableOutput("input_table"),
#'   br(),
#'   h4("Converted R Data:"),
#'   verbatimTextOutput("converted_data")
#' )
#'
#' server <- function(input, output, session) {
#'   initial_data <- data.frame(
#'     Name = c("John", "Jane", "Bob"),
#'     Age = c(25, 30, 35),
#'     Role = c("Engineer", "Manager", "Developer")
#'   )
#'
#'   output$input_table <- renderHandsontable({
#'     handsontable(initial_data)
#'   })
#'
#'   output$converted_data <- renderPrint({
#'     if (!is.null(input$input_table)) {
#'       converted <- hot_to_r(input$input_table$data,
#'                              colnames = names(initial_data))
#'       str(converted)
#'       converted
#'     }
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
hot_to_r <- function(data, colnames = NULL, stringsAsFactors = FALSE) {
  if (!length(data)) {
    return(data.frame())
  }

  df <- as.data.frame(do.call(rbind, data), stringsAsFactors = stringsAsFactors)

  if (length(colnames) == ncol(df)) {
    names(df) <- colnames
  }

  df
}

#' Configure Individual Column
#'
#' @param hot A handsontable widget object
#' @param col Column name or index to configure
#' @param type Column type: "text", "numeric", "date", "dropdown",
#'   "checkbox", etc.
#' @param source For dropdown type, vector of allowed values
#' @param strict Logical, strict validation for dropdowns
#' @param readOnly Logical, make column read-only
#' @param width Column width in pixels
#' @param format For numeric columns, number format
#' @param dateFormat For date columns, date format
#' @param checkedTemplate For checkbox columns, value when checked
#' @param uncheckedTemplate For checkbox columns, value when unchecked
#' @param visibleRows For dropdown type, number of visible rows in dropdown
#' @param allowInvalid Logical, allow invalid values
#' @param ... Additional column configuration options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Individual Column Configuration"),
#'   handsontableOutput("table")
#' )
#'
#' server <- function(input, output, session) {
#'   df <- data.frame(
#'     ID = 1:5,
#'     Name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
#'     Score = c(85.5, 92.3, 78.1, 95.7, 88.9),
#'     Grade = c("B", "A", "C", "A", "B"),
#'     Active = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#'   )
#'
#'   output$table <- renderHandsontable({
#'     handsontable(df) |>
#'       hot_col(col = 1, readOnly = TRUE, width = 60) |>
#'       hot_col(col = 3, type = "numeric", format = "0.0") |>
#'       hot_col(col = 4, type = "dropdown",
#'               source = c("A", "B", "C", "D", "F")) |>
#'       hot_col(col = 5, type = "checkbox")
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
hot_col <- function(
  hot,
  col,
  type = NULL,
  source = NULL,
  strict = NULL,
  readOnly = NULL,
  width = NULL,
  format = NULL,
  dateFormat = NULL,
  checkedTemplate = NULL,
  uncheckedTemplate = NULL,
  visibleRows = NULL,
  allowInvalid = NULL,
  ...
) {
  # Convert column name to index if needed
  if (is.character(col) && !is.null(hot$x$colHeaders)) {
    col_idx <- match(col, hot$x$colHeaders)
    if (is.na(col_idx)) {
      stop("Column '", col, "' not found in colHeaders")
    }
  } else if (is.character(col)) {
    stop("Column names can only be used when colHeaders are defined")
  } else {
    col_idx <- col
  }

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

  # Initialize column config if NULL
  if (is.null(hot$x$columns[[col_idx]])) {
    hot$x$columns[[col_idx]] <- list()
  }

  # Set column properties
  col_config <- list(
    type = type,
    source = source,
    strict = strict,
    readOnly = readOnly,
    width = width,
    format = format,
    dateFormat = dateFormat,
    checkedTemplate = checkedTemplate,
    uncheckedTemplate = uncheckedTemplate,
    allowInvalid = allowInvalid,
    ...
  ) |>
    Filter(f = Negate(is.null))

  # Handle dropdown-specific configuration
  if (!is.null(type) && type == "dropdown") {
    if (!is.null(visibleRows)) {
      col_config$visibleRows <- visibleRows
    }
    # Set default strict mode for dropdowns if not specified
    if (is.null(strict)) {
      col_config$strict <- TRUE
    }
  }

  # Merge with existing column configuration
  hot$x$columns[[col_idx]] <- utils::modifyList(
    hot$x$columns[[col_idx]],
    col_config
  )

  hot
}

#' Configure Row Properties
#'
#' @param hot A handsontable widget object
#' @param row Row index to configure
#' @param readOnly Logical, make row read-only
#' @param ... Additional row configuration options
#'
#' @return Modified handsontable widget
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Row Configuration"),
#'   p("Rows 1, 3, and 5 are read-only"),
#'   handsontableOutput("table")
#' )
#'
#' server <- function(input, output, session) {
#'   output$table <- renderHandsontable({
#'     hot_table <- handsontable(iris[1:8, ])
#'     for (i in c(1, 3, 5)) {
#'       hot_table <- hot_table |> hot_row(row = i, readOnly = TRUE)
#'     }
#'     hot_table
#'   })
#' }
#'
#' shinyApp(ui, server)
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

  if (length(row_config) > 0) {
    # Convert 1-based R index to 0-based JavaScript index
    js_row <- row - 1
    hot$x$rowConfig[[as.character(js_row)]] <- row_config

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
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Programmatic Cell Updates"),
#'   handsontableOutput("my_table"),
#'   br(),
#'   actionButton("update_btn", "Update Cell (1,1) & (2,1)")
#' )
#'
#' server <- function(input, output, session) {
#'   output$my_table <- renderHandsontable({
#'     handsontable(data.frame(
#'       A = c(1, 2, 3),
#'       B = c("a", "b", "c")
#'     ))
#'   })
#'
#'   observeEvent(input$update_btn, {
#'     set_data("my_table", row = 1:2, col = 1, val = runif(2, 100, 999))
#'   })
#' }
#'
#' shinyApp(ui, server)
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
