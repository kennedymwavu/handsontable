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
#' @param fixedRowsTop Number of rows to freeze at the top
#' @param fixedRowsBottom Number of rows to freeze at the bottom
#' @param manualColumnResize Logical, enable manual column resizing
#' @param manualColumnMove Logical, enable manual column reordering
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
  fixedRowsTop = 0,
  fixedRowsBottom = 0,
  manualColumnResize = TRUE,
  manualColumnMove = FALSE,
  manualRowResize = TRUE,
  manualRowMove = FALSE,
  stretchH = c("all", "last", "none"),
  ...
) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a data.frame or matrix")
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  # ----colHeaders----
  if (isTRUE(colHeaders)) {
    colHeaders <- colnames(data)
  }

  if (isFALSE(colHeaders)) {
    colHeaders <- NULL
  }

  # ----rowHeaders----
  if (isTRUE(rowHeaders)) {
    rowHeaders <- rownames(data)
  }

  # ----stretchH----
  stretchH <- match.arg(arg = stretchH)

  # Convert data to list format for JavaScript
  data_list <- lapply(
    X = seq_len(nrow(data)),
    FUN = function(row_idx) {
      row <- as.list(data[row_idx, , drop = FALSE])

      # Convert factors to character for JS compatibility
      for (idx in seq_along(row)) {
        value <- row[[idx]]
        if (is.factor(value)) {
          row[[idx]] <- as.character(value)
        }
      }

      unname(row)
    }
  )

  # Create configuration object
  config <- list(
    data = data_list,
    colHeaders = colHeaders,
    rowHeaders = rowHeaders,
    readOnly = readOnly,
    adaptiveHeight = adaptiveHeight,
    colWidths = colWidths,
    fixedColumnsLeft = fixedColumnsLeft,
    fixedRowsTop = fixedRowsTop,
    fixedRowsBottom = fixedRowsBottom,
    manualColumnResize = manualColumnResize,
    manualColumnMove = manualColumnMove,
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
