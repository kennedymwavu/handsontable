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
  )

  # Remove NULL values
  config <- config[!sapply(config, is.null)]

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
  )

  # Remove NULL values
  config <- config[!sapply(config, is.null)]

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
  )

  # Remove NULL values
  config <- config[!sapply(config, is.null)]

  # Merge with existing configuration
  hot$x <- utils::modifyList(hot$x, config)

  hot
}

#' Configure Data Validation
#'
#' @param hot A handsontable widget object
#' @param cols Column indices to apply validation (numeric vector)
#' @param type Validation type: "numeric", "date", "list", "regexp"
#' @param source For "list" type, vector of allowed values
#' @param pattern For "regexp" type, regular expression pattern
#' @param min,max For "numeric" type, minimum and maximum values
#' @param allowInvalid Logical, allow invalid values to be entered
#' @param strict Logical, strict validation mode
#' @param ... Additional validation options
#'
#' @return Modified handsontable widget
#' @export
hot_validate <- function(
  hot,
  cols = NULL,
  type = "numeric",
  source = NULL,
  pattern = NULL,
  min = NULL,
  max = NULL,
  allowInvalid = TRUE,
  strict = FALSE,
  ...
) {
  if (is.null(cols)) {
    stop("cols parameter is required for validation")
  }

  validator <- list(
    type = type,
    allowInvalid = allowInvalid,
    strict = strict,
    ...
  )

  # Add type-specific options
  if (type == "numeric") {
    if (!is.null(min)) {
      validator$min <- min
    }
    if (!is.null(max)) validator$max <- max
  } else if (type == "list") {
    if (!is.null(source)) validator$source <- source
  } else if (type == "regexp") {
    if (!is.null(pattern)) validator$pattern <- pattern
  }

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
  if (is.logical(hot$x$contextMenu) && hot$x$contextMenu) {
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
#' @param data Handsontable data (typically from Shiny input)
#' @param colnames Optional column names
#' @param stringsAsFactors Logical, convert strings to factors
#'
#' @return A data.frame
#' @export
hot_to_r <- function(data, colnames = NULL, stringsAsFactors = FALSE) {
  if (is.null(data) || length(data) == 0) {
    return(data.frame())
  }

  # Convert to data frame
  df <- as.data.frame(do.call(rbind, data), stringsAsFactors = stringsAsFactors)

  # Apply column names if provided
  if (!is.null(colnames) && length(colnames) == ncol(df)) {
    names(df) <- colnames
  }

  df
}
