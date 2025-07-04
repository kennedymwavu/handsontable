% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hot_functions.R
\name{hot_context_menu}
\alias{hot_context_menu}
\title{Configure Context Menu}
\usage{
hot_context_menu(hot, opts = NULL, customOpts = NULL)
}
\arguments{
\item{hot}{A handsontable widget object}

\item{opts}{Character vector of built-in context menu options.
If NULL, uses default options:
\itemize{
\item "row_above",
\item "row_below",
\item "col_left",
\item "col_right",
\item "---------",
\item "remove_row",
\item "remove_col",
\item "clear_column",
\item "make_read_only",
\item "undo",
\item "redo",
\item "cut",
\item "copy",
\item "---------",
\item "export_csv"
}}

\item{customOpts}{List of custom context menu options.
Each custom option should be a list with elements like
\code{name}, \code{callback}, \code{disabled}, etc. Example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{list(
  list(
    name = "Custom Action",
    callback = htmlwidgets::JS("function() \{...\}")
  )
)
}\if{html}{\out{</div>}}}
}
\value{
Modified handsontable widget
}
\description{
Configure Context Menu
}
\examples{
if (interactive()) {
  library(shiny)

  # Basic usage with default options
  ui <- fluidPage(
    titlePanel("Context Menu Configuration"),
    handsontableOutput("table1"),
    br(),
    handsontableOutput("table2"),
    br(),
    handsontableOutput("table3")
  )

  server <- function(input, output, session) {
    # Default context menu
    output$table1 <- renderHandsontable({
      handsontable(mtcars[1:5, 1:4]) |>
        hot_context_menu()
    })

    # Custom selection of built-in options
    output$table2 <- renderHandsontable({
      handsontable(mtcars[1:5, 1:4]) |>
        hot_context_menu(opts = c("row_above", "row_below", "---------", "copy", "cut"))
    })

    # With custom options
    output$table3 <- renderHandsontable({
      handsontable(mtcars[1:5, 1:4]) |>
        hot_context_menu(
          opts = c("copy", "cut"),
          customOpts = list(
            list(
              name = "Alert",
              callback = htmlwidgets::JS("function() { alert('Custom action!'); }")
            )
          )
        )
    })
  }

  shinyApp(ui, server)
}

}
