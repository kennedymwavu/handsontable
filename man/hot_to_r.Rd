% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hot_functions.R
\name{hot_to_r}
\alias{hot_to_r}
\title{Convert Handsontable Data to R}
\usage{
hot_to_r(data)
}
\arguments{
\item{data}{Object. Handsontable input object from Shiny (contains data and colnames)}
}
\value{
data.frame
}
\description{
Helper function to convert JavaScript Handsontable data back to R format.
Primarily used in Shiny applications. Automatically uses original column names
sent from the Handsontable widget.
}
\examples{
if (interactive()) {
  library(shiny)

  ui <- fluidPage(
    titlePanel("Data Conversion Demo"),
    handsontableOutput("input_table"),
    br(),
    h4("Converted R Data:"),
    verbatimTextOutput("converted_data")
  )

  server <- function(input, output, session) {
    initial_data <- data.frame(
      Name = c("John", "Jane", "Bob"),
      Age = c(25, 30, 35),
      Role = c("Engineer", "Manager", "Developer")
    )

    output$input_table <- renderHandsontable({
      handsontable(initial_data)
    })

    output$converted_data <- renderPrint({
      if (!is.null(input$input_table)) {
        # Column names are automatically preserved!
        converted <- hot_to_r(input$input_table)
        str(converted)
        converted
      }
    })
  }

  shinyApp(ui, server)
}

}
