% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handsontable.R
\name{handsontableOutput}
\alias{handsontableOutput}
\title{Widget output function for use in Shiny}
\usage{
handsontableOutput(outputId, width = "100\%", height = "400px")
}
\arguments{
\item{outputId}{Output variable name}

\item{width, height}{Widget dimensions}
}
\description{
Widget output function for use in Shiny
}
\examples{
if (interactive()) {
  library(shiny)

  ui <- fluidPage(
    titlePanel("Handsontable Example"),
    handsontableOutput("my_table")
  )

  server <- function(input, output) {
    output$my_table <- renderHandsontable({
      handsontable(mtcars[1:5, 1:4])
    })
  }

  shinyApp(ui, server)
}

}
