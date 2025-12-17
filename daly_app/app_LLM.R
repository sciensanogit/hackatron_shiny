library(shiny)
library(bslib)
library(querychat)

# 1. Create a QueryChat instance with your data
qc <- QueryChat$new(mtcars)

ui <- page_sidebar(
  # 2. Use qc$sidebar() in a bslib::page_sidebar.
  #    Alternatively, use qc$ui() elsewhere if you don't want your
  #    chat interface to live in a sidebar.
  sidebar = qc$sidebar(),
  DT::DTOutput("dt")
)

server <- function(input, output, session) {
  # 3. Initialize the QueryChat server (returns session-specific reactive values)
  qc_vals <- qc$server()
  
  output$dt <- DT::renderDT({
    # 4. Use the filtered/sorted data frame anywhere you wish, via qc_vals$df()
    DT::datatable(qc_vals$df())
  })
}

shinyApp(ui, server)