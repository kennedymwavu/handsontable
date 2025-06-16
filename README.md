# handsontable

<!-- badges: start -->

[![R-CMD-check](https://github.com/kennedymwavu/handsontable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kennedymwavu/handsontable/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/handsontable)](https://CRAN.R-project.org/package=handsontable)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

R interface to the [Handsontable](https://handsontable.com/) JavaScript library [v6.2.2](https://github.com/handsontable/handsontable/tree/6.2.2).

Create interactive spreadsheet-like data grids with data validation, sorting, filtering, and seamless Shiny integration.

## Installation

```r
# Install from GitHub
devtools::install_github("kennedymwavu/handsontable")
```

## Quick Example

```r
library(handsontable)

# Basic table
handsontable(mtcars[1:5, 1:4])

# With validation and formatting
handsontable(iris[1:8, ]) |>
  hot_col(
    col = 5,
    type = "dropdown",
    source = c("setosa", "versicolor", "virginica")
  ) |>
  hot_validate(
    cols = 1:4,
    type = "numeric",
    min = 0
  ) |>
  hot_table(contextMenu = TRUE, search = TRUE)
```

## Shiny Integration

```r
library(shiny)

ui <- fluidPage(handsontableOutput("table"))

server <- function(input, output) {
  output$table <- renderHandsontable({
    handsontable(iris[1:10, ])
  })

  observeEvent(input$table, {
    # Detect changes
    if (!is.null(input$table)) {
      updated_data <- hot_to_r(
        data = input$table$data,
        colnames = names(iris)
      )
      cat("Updated data:\n")
      print(updated_data)
      # Use updated_data...
    }
  })
}

shinyApp(ui, server)
```

## Documentation

- [**Getting Started**](https://kennedymwavu.github.io/handsontable/articles/getting-started.html)
- [**Configuration Options**](https://kennedymwavu.github.io/handsontable/articles/configuration-options.html)
- [**Data Validation & Customization**](https://kennedymwavu.github.io/handsontable/articles/validation-and-customization.html)
- [**Shiny Integration**](https://kennedymwavu.github.io/handsontable/articles/shiny-integration.html)
- [**Function Reference**](https://kennedymwavu.github.io/handsontable/reference/)
