#' @importFrom htmlwidgets createWidget shinyWidgetOutput shinyRenderWidget sizingPolicy
#' @importFrom jsonlite toJSON
#' @importFrom magrittr %>%
#' @importFrom utils modifyList
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling \code{rhs(lhs)}.
NULL

.onLoad <- function(libname, pkgname) {
  # Package startup message
  packageStartupMessage(
    "Loaded handsontable package. ",
    "Remember: Handsontable requires a license for commercial use. ",
    "See https://handsontable.com/pricing for details."
  )
}
