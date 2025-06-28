#' Create a Handsontable Widget
#'
#' Creates an interactive data grid using the Handsontable JavaScript library.
#' The widget provides spreadsheet-like functionality including data validation,
#' conditional formatting, formulas, and extensive customization options.
#'
#' @param data A data.frame, matrix, or other rectangular data structure
#' @param width,height Widget dimensions. Can be specified as pixels
#'   (e.g., "400px") or percentage (e.g., "100%"). If NULL, dimensions are
#'   automatically determined.
#' @param adaptiveHeight Logical. If TRUE, the table's height
#'   will automatically adjust based on the number of rows displayed.
#'   If FALSE (default), the specified height will be used.
#' @param colHeaders Logical or character vector. If TRUE, shows default
#'   column headers.
#'   If character vector, uses custom column names.
#' @param rowHeaders Logical. If TRUE, shows row numbers as headers.
#' @param readOnly Logical. If TRUE, makes the entire table read-only.
#' @param colWidths Numeric vector or single value for column widths
#' @param fixedColumnsLeft Number of columns to freeze on the left
#' @param manualColumnResize Logical, enable manual column resizing
#' @param manualColumnMove Logical, enable manual column reordering
#' @param fixedRowsTop Number of rows to freeze at the top
#' @param fixedRowsBottom Number of rows to freeze at the bottom
#' @param manualRowResize Logical, enable manual row resizing
#' @param manualRowMove Logical, enable manual row reordering
#' @param stretchH `stretchH` option.
#' @param ... Additional configuration options passed to Handsontable
#'
#' @return An htmlwidget object for rendering the Handsontable
#'
#' @examples
#' if (interactive()) {
#'   # Basic usage
#'   handsontable(mtcars)
#'
#'   # With custom headers
#'   handsontable(iris, colHeaders = c(
#'     "S.Length", "S.Width", "P.Length",
#'     "P.Width", "Species"
#'   ))
#'
#'   # Read-only table with context menu
#'   handsontable(airquality, readOnly = TRUE) |>
#'     hot_context_menu()
#' }
#'
#' @export
handsontable <- function(
  data,
  width = NULL,
  height = NULL,
  adaptiveHeight = FALSE,
  colHeaders = TRUE,
  rowHeaders = TRUE,
  readOnly = FALSE,
  colWidths = NULL,
  fixedColumnsLeft = 0,
  manualColumnResize = TRUE,
  manualColumnMove = FALSE,
  fixedRowsTop = 0,
  fixedRowsBottom = 0,
  manualRowResize = TRUE,
  manualRowMove = FALSE,
  stretchH = "all",
  ...
) {
  # Validate inputs
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a data.frame or matrix")
  }

  # Convert data to appropriate format
  if (is.matrix(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  # Handle column headers
  if (is.logical(colHeaders) && colHeaders) {
    colHeaders <- names(data)
  } else if (is.logical(colHeaders) && !colHeaders) {
    colHeaders <- NULL
  }

  # Convert data to list format for JavaScript
  data_list <- lapply(seq_len(nrow(data)), function(i) {
    row <- as.list(data[i, , drop = FALSE])
    # Convert factors to character for JS compatibility
    row <- lapply(row, function(x) {
      if (is.factor(x)) as.character(x) else x
    })
    unname(row)
  })

  # Create configuration object
  config <- list(
    data = data_list,
    colHeaders = colHeaders,
    rowHeaders = rowHeaders,
    readOnly = readOnly,
    adaptiveHeight = adaptiveHeight,
    colWidths = colWidths,
    fixedColumnsLeft = fixedColumnsLeft,
    manualColumnResize = manualColumnResize,
    manualColumnMove = manualColumnMove,
    fixedRowsTop = fixedRowsTop,
    fixedRowsBottom = fixedRowsBottom,
    manualRowResize = manualRowResize,
    manualRowMove = manualRowMove,
    stretchH = stretchH,
    # Store original column names for hot_to_r()
    originalColnames = names(data),
    ...
  ) |>
    Filter(f = Negate(is.null))

  # Create widget
  htmlwidgets::createWidget(
    name = "handsontable",
    x = config,
    width = width,
    height = height,
    package = "handsontable",
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = 400,
      padding = 0,
      viewer.defaultWidth = "100%",
      viewer.defaultHeight = 400,
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = 400,
      browser.fill = TRUE
    )
  )
}

#' Widget output function for use in Shiny
#'
#' @param outputId Output variable name
#' @param width,height Widget dimensions
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Handsontable Example"),
#'     handsontableOutput("my_table")
#'   )
#'
#'   server <- function(input, output) {
#'     output$my_table <- renderHandsontable({
#'       handsontable(mtcars[1:5, 1:4])
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
handsontableOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "handsontable",
    width,
    height,
    package = "handsontable"
  )
}

#' Widget render function for use in Shiny
#'
#' @param expr Expression that generates a handsontable
#' @param env Environment in which to evaluate expression
#' @param quoted Logical indicating whether expression is quoted
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Interactive Handsontable"),
#'     handsontableOutput("editable_table")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$editable_table <- renderHandsontable({
#'       handsontable(iris[1:10, ], readOnly = FALSE, colWidths = 120)
#'         hot_context_menu()
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
renderHandsontable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, handsontableOutput, env, quoted = TRUE)
}
