# handsontable

<!-- badges: start -->

[![R-CMD-check](https://github.com/kennedymwavu/handsontable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kennedymwavu/handsontable/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/handsontable)](https://CRAN.R-project.org/package=handsontable)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

R interface to the [Handsontable](https://handsontable.com/) JavaScript library
[v6.2.2 (MIT license)](https://github.com/handsontable/handsontable/tree/6.2.2).

create interactive spreadsheet-like data grids with data validation, sorting, and filtering.

## installation

```r
# install from CRAN
install.packages("handsontable")
```

```r
# or install from GitHub
devtools::install_github("kennedymwavu/handsontable")
```

```{r}
# load the package:
library(handsontable)
```

## quick example

```{r}
# basic table:
handsontable(mtcars[1:5, ], adaptiveHeight = TRUE)

# col specific config + validation + context menu:
handsontable(
  iris[1:8, ],
  adaptiveHeight = TRUE
) |>
  hot_col(
    col = "Species",
    type = "dropdown",
    source = c("setosa", "versicolor", "virginica")
  ) |>
  hot_validate(
    col = 1:4,
    type = "numeric",
    min = 0
  ) |>
  hot_context_menu()
```

- right-click to see the context menu
- try entering an invalid value (like a letter) in
  the first 4 columns.

## shiny integration

```r
library(shiny)

ui <- fluidPage(handsontableOutput("table"))

server <- function(input, output) {
  output$table <- renderHandsontable({
    handsontable(iris[1:10, ]) |>
      hot_context_menu()
  })

  # detect changes:
  observeEvent(input$table, {
    updated_data <- hot_to_r(data = input$table)
    cat("Updated data:\n")
    print(updated_data)
    # use updated data...
  })
}

shinyApp(ui, server)
```

## similar packages

### {rhandsontable}

the [rhandsontable](https://github.com/jrowen/rhandsontable) package
provides an R interface to handsontable and has been a valuable tool for the
R community. however, it hasn't been actively maintained in recent years.

`{handsontable}` was developed to **ensure active maintenance**
with regular updates, bug fixes, and feature enhancements that the community needs.
