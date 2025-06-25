library(shiny)
library(handsontable)

# Sample data for demonstrations
create_sample_data <- function(rows = 20) {
  data.frame(
    Name = paste("Item", 1:rows),
    Price = round(runif(rows, 10, 100), 2),
    Category = sample(c("A", "B", "C"), rows, replace = TRUE),
    Stock = sample(1:50, rows),
    Available = sample(c(TRUE, FALSE), rows, replace = TRUE),
    Date = sample(
      seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
      rows
    ),
    Notes = sample(c("Good", "Fair", "Excellent", "Poor"), rows, replace = TRUE)
  )
}

create_product_data <- function() {
  data.frame(
    ID = 1:8,
    Product = paste("Product", LETTERS[1:8]),
    Price = round(runif(8, 20, 200), 2),
    Quantity = sample(1:20, 8),
    Total = round(runif(8, 100, 1000), 2)
  )
}

create_stretching_data <- function() {
  data.frame(
    Code = c("A1", "B2", "C3", "D4", "E5"),
    Name = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"),
    Value = c(10, 20, 30, 40, 50),
    Status = c("Active", "Inactive", "Active", "Pending", "Active")
  )
}

create_freezing_data <- function() {
  data.frame(
    ID = 1:10,
    Name = paste("Item", LETTERS[1:10]),
    Category = sample(
      c("Electronics", "Clothing", "Books"),
      10,
      replace = TRUE
    ),
    Price = round(runif(10, 15, 150), 2),
    Stock = sample(5:50, 10),
    Supplier = paste("Company", sample(1:5, 10, replace = TRUE)),
    Location = sample(
      c("Warehouse A", "Warehouse B", "Store"),
      10,
      replace = TRUE
    )
  )
}

create_task_data <- function() {
  data.frame(
    Task = paste("Task", 1:6),
    Priority = sample(c("High", "Medium", "Low"), 6, replace = TRUE),
    Status = sample(c("Todo", "In Progress", "Done"), 6, replace = TRUE),
    Assignee = sample(c("Alice", "Bob", "Charlie"), 6, replace = TRUE)
  )
}

create_quarterly_data <- function() {
  data.frame(
    Quarter = c("Q1", "Q2", "Q3", "Q4"),
    Revenue = c(1200, 1350, 1180, 1420),
    Expenses = c(800, 900, 850, 950),
    Profit = c(400, 450, 330, 470)
  )
}

ui <- fluidPage(
  titlePanel("Handsontable Interactive Features Showcase"),

  tabsetPanel(
    # Fixed Headers Tab
    tabPanel(
      "Fixed Headers",
      h3("Fixed Headers"),
      p(
        "Keep specified rows and columns visible while scrolling through the table."
      ),

      fluidRow(
        column(
          6,
          h4("Basic Fixed Headers"),
          p("Fix top 2 rows and left 1 column:"),
          handsontableOutput("fixed_basic", height = "300px", width = "400px")
        ),
        column(
          6,
          h4("Configuration Options"),
          selectInput(
            "fixed_rows_top",
            "Fixed Rows Top:",
            choices = 0:3,
            selected = 2
          ),
          selectInput(
            "fixed_cols_left",
            "Fixed Columns Left:",
            choices = 0:3,
            selected = 1
          ),
          selectInput(
            "fixed_rows_bottom",
            "Fixed Rows Bottom:",
            choices = 0:2,
            selected = 0
          ),
          handsontableOutput("fixed_config", height = "300px", width = "400px")
        )
      )
    ),

    # Interactive Moving Tab
    tabPanel(
      "Interactive Moving",
      h3("Interactive Moving"),
      p(
        "Enable drag-and-drop reordering of columns and rows through interactive handles."
      ),

      fluidRow(
        column(
          6,
          h4("Column and Row Movement"),
          checkboxInput("enable_col_move", "Enable Column Moving", TRUE),
          checkboxInput("enable_row_move", "Enable Row Moving", TRUE),
          handsontableOutput("moving_basic", height = "300px")
        ),
        column(
          6,
          h4("Movement with Fixed Elements"),
          p("First column stays fixed while others can be moved:"),
          handsontableOutput("moving_fixed", height = "300px")
        )
      )
    ),

    # Column Stretching Tab
    tabPanel(
      "Column Stretching",
      h3("Column Stretching"),
      p(
        "Control how columns fill the available horizontal space in the table container."
      ),

      fluidRow(
        column(
          4,
          h4("Stretch All"),
          p("stretchH = 'all'"),
          handsontableOutput("stretch_all", height = "200px")
        ),
        column(
          4,
          h4("Stretch Last"),
          p("stretchH = 'last'"),
          handsontableOutput("stretch_last", height = "200px")
        ),
        column(
          4,
          h4("No Stretching"),
          p("stretchH = 'none'"),
          handsontableOutput("stretch_none", height = "200px")
        )
      ),

      br(),
      fluidRow(
        column(
          12,
          h4("Interactive Stretching Configuration"),
          selectInput(
            "stretch_mode",
            "Stretch Mode:",
            choices = c("all", "last", "none"),
            selected = "all"
          ),
          handsontableOutput("stretch_interactive", height = "250px")
        )
      )
    ),

    # Manual Freezing Tab
    tabPanel(
      "Manual Freezing",
      h3("Manual Freezing"),
      p(
        "Allow users to freeze and unfreeze columns interactively through the context menu."
      ),
      p(
        strong("Instructions:"),
        "Right-click on any column header to access freezing options."
      ),

      fluidRow(
        column(
          6,
          h4("Basic Manual Freezing"),
          handsontableOutput(
            "freezing_basic",
            height = "350px",
            width = "400px"
          )
        ),
        column(
          6,
          h4("Freezing with Initial Fixed Columns"),
          p("Starts with first column already fixed:"),
          handsontableOutput(
            "freezing_initial",
            height = "350px",
            width = "400px"
          )
        )
      )
    ),

    # Context Menu Tab
    tabPanel(
      "Context Menu",
      h3("Context Menu"),
      p(
        "Provide a right-click context menu with customizable table manipulation options."
      ),

      fluidRow(
        column(
          6,
          h4("Default Context Menu"),
          handsontableOutput("context_default", height = "250px")
        ),
        column(
          6,
          h4("Custom Menu Selection"),
          checkboxGroupInput(
            "menu_options",
            "Select Menu Options:",
            choices = c(
              "row_above",
              "row_below",
              "col_left",
              "col_right",
              "remove_row",
              "remove_col",
              "undo",
              "redo",
              "copy",
              "cut"
            ),
            selected = c("row_above", "row_below", "copy", "cut")
          ),
          handsontableOutput("context_custom", height = "250px")
        )
      ),

      br(),
      fluidRow(
        column(
          12,
          h4("Menu with Custom Actions"),
          p("Includes custom menu items with JavaScript callbacks:"),
          handsontableOutput("context_callbacks", height = "250px")
        )
      )
    ),

    # Custom Borders Tab
    tabPanel(
      "Custom Borders",
      h3("Custom Borders"),
      p("Add custom borders to cells for visual emphasis and styling."),

      fluidRow(
        column(
          6,
          h4("Header Border"),
          p("Top and bottom borders on first row:"),
          handsontableOutput("borders_header", height = "250px")
        ),
        column(
          6,
          h4("Range Borders"),
          p("Border around a range of cells:"),
          handsontableOutput("borders_range", height = "250px")
        )
      ),

      br(),
      fluidRow(
        column(
          6,
          h4("Multiple Border Styles"),
          p("Different borders for different areas:"),
          handsontableOutput("borders_multiple", height = "250px")
        ),
        column(
          6,
          h4("Individual Border Sides"),
          p("Each cell shows a different border side:"),
          handsontableOutput("borders_sides", height = "250px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Fixed Headers Examples
  output$fixed_basic <- renderHandsontable({
    data <- create_sample_data()
    handsontable(
      data,
      fixedRowsTop = 2,
      fixedColumnsLeft = 1
    )
  })

  output$fixed_config <- renderHandsontable({
    data <- create_sample_data()
    handsontable(
      data,
      fixedRowsTop = as.numeric(input$fixed_rows_top),
      fixedColumnsLeft = as.numeric(input$fixed_cols_left),
      fixedRowsBottom = as.numeric(input$fixed_rows_bottom)
    )
  })

  # Interactive Moving Examples
  output$moving_basic <- renderHandsontable({
    data <- create_product_data()
    handsontable(
      data,
      manualColumnMove = input$enable_col_move,
      manualRowMove = input$enable_row_move
    )
  })

  output$moving_fixed <- renderHandsontable({
    data <- create_product_data()
    handsontable(
      data,
      fixedColumnsLeft = 1,
      manualColumnMove = TRUE,
      manualRowMove = TRUE
    )
  })

  # Column Stretching Examples
  output$stretch_all <- renderHandsontable({
    data <- create_stretching_data()
    handsontable(data, stretchH = "all")
  })

  output$stretch_last <- renderHandsontable({
    data <- create_stretching_data()
    handsontable(data, stretchH = "last")
  })

  output$stretch_none <- renderHandsontable({
    data <- create_stretching_data()
    handsontable(data, stretchH = "none")
  })

  output$stretch_interactive <- renderHandsontable({
    data <- create_stretching_data()
    handsontable(
      data,
      stretchH = input$stretch_mode
    )
  })

  # Manual Freezing Examples
  output$freezing_basic <- renderHandsontable({
    data <- create_freezing_data()
    handsontable(
      data,
      contextMenu = TRUE,
      manualColumnFreeze = TRUE,
      adaptiveHeight = TRUE
    )
  })

  output$freezing_initial <- renderHandsontable({
    data <- create_freezing_data()
    handsontable(
      data,
      fixedColumnsLeft = 1,
      contextMenu = TRUE,
      manualColumnFreeze = TRUE,
      adaptiveHeight = TRUE
    )
  })

  # Context Menu Examples
  output$context_default <- renderHandsontable({
    data <- create_task_data()
    handsontable(data) |>
      hot_context_menu()
  })

  output$context_custom <- renderHandsontable({
    data <- create_task_data()
    selected_opts <- input$menu_options
    if (length(selected_opts) == 0) {
      selected_opts <- c("copy")
    }

    handsontable(data) |>
      hot_context_menu(opts = selected_opts)
  })

  output$context_callbacks <- renderHandsontable({
    data <- create_task_data()
    handsontable(data) |>
      hot_context_menu(
        opts = c("copy", "cut", "---------"),
        # customOpts must be a named list containing named lists
        customOpts = list(
          a = list(
            name = "Alert Selected",
            callback = htmlwidgets::JS(
              "function() { alert('Custom action triggered!'); }"
            )
          ),
          b = list(
            name = "Log Data",
            callback = htmlwidgets::JS(
              "function() { console.log('Table data:', this.getData()); }"
            )
          )
        )
      )
  })

  # Custom Borders Examples
  output$borders_header <- renderHandsontable({
    data <- create_quarterly_data()
    handsontable(
      data,
      adaptiveHeight = TRUE,
      customBorders = list(
        list(
          range = list(
            from = list(row = 0, col = 0),
            to = list(row = 0, col = 3)
          ),
          top = list(width = 2, color = "#ff6b6b"),
          bottom = list(width = 2, color = "#ff6b6b")
        )
      )
    )
  })

  output$borders_range <- renderHandsontable({
    data <- create_quarterly_data()
    handsontable(
      data,
      adaptiveHeight = TRUE,
      customBorders = list(
        list(
          range = list(
            from = list(row = 1, col = 1),
            to = list(row = 3, col = 2)
          ),
          left = list(width = 2, color = "#45b7d1"),
          right = list(width = 2, color = "#45b7d1"),
          top = list(width = 2, color = "#45b7d1"),
          bottom = list(width = 2, color = "#45b7d1")
        )
      )
    )
  })

  output$borders_multiple <- renderHandsontable({
    data <- create_quarterly_data()
    handsontable(
      data,
      adaptiveHeight = TRUE,
      customBorders = list(
        # Header border
        list(
          range = list(
            from = list(row = 0, col = 0),
            to = list(row = 0, col = 3)
          ),
          bottom = list(width = 3, color = "#2c3e50")
        ),
        # Data section border
        list(
          range = list(
            from = list(row = 1, col = 0),
            to = list(row = 3, col = 3)
          ),
          left = list(width = 1, color = "#bdc3c7"),
          right = list(width = 1, color = "#bdc3c7"),
          top = list(width = 1, color = "#bdc3c7"),
          bottom = list(width = 1, color = "#bdc3c7")
        ),
        # Highlight specific cell
        list(
          row = 2,
          col = 3,
          left = list(width = 4, color = "#e74c3c"),
          right = list(width = 4, color = "#e74c3c"),
          top = list(width = 4, color = "#e74c3c"),
          bottom = list(width = 4, color = "#e74c3c")
        )
      )
    )
  })

  output$borders_sides <- renderHandsontable({
    data <- create_quarterly_data()
    handsontable(
      data,
      adaptiveHeight = TRUE,
      customBorders = list(
        # Top border only
        list(
          row = 0,
          col = 0,
          top = list(width = 4, color = "#f39c12")
        ),
        # Bottom border only
        list(
          row = 1,
          col = 1,
          bottom = list(width = 3, color = "#27ae60")
        ),
        # Left border only
        list(
          row = 2,
          col = 2,
          left = list(width = 3, color = "#8e44ad")
        ),
        # Right border only
        list(
          row = 3,
          col = 3,
          right = list(width = 3, color = "#e67e22")
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)
