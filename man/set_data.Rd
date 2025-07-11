% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hot_functions.R
\name{set_data}
\alias{set_data}
\title{Update Cell Data Programmatically}
\usage{
set_data(id, row, col, val, session = shiny::getDefaultReactiveDomain())
}
\arguments{
\item{id}{String. Output ID of the handsontable widget.}

\item{row}{Integer vector. Row index.}

\item{col}{Integer vector. Column index.}

\item{val}{Any. Value to set in the cell(s).}

\item{session}{Shiny session object}
}
\value{
NULL (called for side effects)
}
\description{
Update Cell Data Programmatically
}
\examples{
if (interactive()) {
  library(shiny)

  ui <- fluidPage(
    titlePanel("Programmatic Cell Updates"),
    handsontableOutput("my_table"),
    br(),
    actionButton("update_btn", "Update Cell (1,1) & (2,1)")
  )

  server <- function(input, output, session) {
    output$my_table <- renderHandsontable({
      handsontable(data.frame(
        A = c(1, 2, 3),
        B = c("a", "b", "c")
      ))
    })

    observeEvent(input$update_btn, {
      set_data("my_table", row = 1:2, col = 1, val = runif(2, 100, 999))
    })
  }

  shinyApp(ui, server)
}

}
