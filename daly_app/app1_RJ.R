library(shiny)
library(bslib)
library(rintrojs)
library(ggplot2)
library(dplyr)

data <- read.csv2("./daly_data/Belgium_export-nation.csv", sep = ";")
data$date <- as.Date(data$date)
data$value_pmmv <- as.numeric(data$value_pmmv)

ui <- page_navbar(
  introjsUI(),
  nav_spacer(),
  # Navigation bar ----
  introBox(
    actionButton("Help", "Press for instruction")
  ),
  fillable=F, navbar_options = navbar_options(underline = FALSE, theme="dark"),
  nav_item(tags$a(tags$span(id="lg", "NL"))),
  nav_item(tags$a(tags$span(id="lg", "FR"))),
  nav_item(tags$a(tags$span(id="lg", "EN"))),
  nav_item(),
  
  # Theme ----
  
  theme = bs_theme(preset="bootstrap",
                   version=5,
                   fg = "#69aa41",
                   bg = "white",
                   base_font="'Open Sans', sans-serif",
                   "spacers"= "(
                                                  0: 0,
                                                  1: 1rem * .25,
                                                  2: 1rem * .5,
                                                  3: 1rem,
                                                  4: 1rem * 1.5,
                                                  5: 1rem * 3,
                                                  6: 1rem * 4
                                                )",
                   "font-size-root"="13.5px",
                   "line-height-base"="1.8",
                   "primary" = "#58595b",
                   "secondary" = "#69aa41",
                   "info" = "#bccf00",
                   "grid-gutter-width"="0rem",
                   "navbar-padding-y" = "13px",
                   "navbar-nav-link-padding-x" = "0rem",
                   "card-bg"="white",
                   "card-border-radius"="5px",
                   "enable-shadows"="true",
                   "card-box-shadow"="0 5px 10px #ddd",
                   "tooltip-max-width"="400px",
                   "tooltip-border-radius"="5px",
                   "spinner-animation-speed"="1.5s",
                   "form-select-border-radius" = "5px",
                   "form-select-box-shadow" = "white",
                   "form-select-focus-border-color" ="#69aa41",
                   "btn-border-radius" = "5px",
                   "btn-box-shadow" = "white"
  ),
  
  # Interface ----
    nav_panel(class="m-0 p-0",title="",
              introBox(
            card(class= "border-0 mt-2 mt-xxl-6 mb-0 p-0 sm-shadow card_custom",
                 card_header(class="text-center bg-white fw-bold h5 border-bottom border-secondary border-3 mx-3 mx-sm-4 my-4",
                             "BeBOD estimates of the burden of disease"),
                 
                 card_body(class="mx-3 mx-sm-4 mt-3 pt-1 p-0",
                           layout_column_wrap(
                             value_box(class="shadow-none",
                                       theme = value_box_theme(fg = "white", bg = "#69aa41"),
                                       title = "Disability-Adjusted Life Year (DALYs) are a measure of overall disease burden, representing the healthy life years lost due to morbidity and mortality. DALYs are calculated as the sum of Years of Life Lost (YLLs) and Years Lived with Disability (YLDs) for each of the considered diseases.",
                                       value = "Disability-Adjusted Life Years (DALYs)"
                             ),
                             value_box(class="shadow-none",
                                       theme = value_box_theme(fg = "white", bg = "#69aa41"),
                                       title = "YLDs quantify the morbidity impact of diseases. They are calculated as the product of the number of prevalent cases with the disability weight (DW), averaged over the different health states of the disease. The DWs reflect the relative reduction in quality of life, on a scale from 0 (perfect health) to 1 (death). We calculate YLDs using the Global Burden of Disease DWs.", 
                                       value = "Years Lived with Disability (YLDs)"
                             ),
                             value_box(class="shadow-none",
                                       theme = value_box_theme(fg = "white", bg = "#69aa41"),
                                       title = "YLLs quantify the mortality impact of diseases. They are calculated as the product of the number of deaths due to the disease with the residual life expectancy at the age of death. We calculate YLLs using the Global Burden of Disease reference life table, which represents the theoretical maximum number of years that people can expect to live.", 
                                       value = "Years of Life Lost (YLLs)"
                             )
                           )
                          
                 )
            ),
            data.step = 1,
            data.intro = "This is an introduction."
              ), 
                 card(class= "border-0 mt-2 mt-xxl-6 mb-0 p-0 sm-shadow card_custom",
                      card_header(class="text-center bg-white fw-bold h5 border-bottom border-secondary border-3 mx-3 mx-sm-4 my-4",
                                  "Explore our estimates"),
                      
                      card_body(class="mx-3 mx-sm-4 mt-3 pt-1 p-0",
                                layout_column_wrap(
                                  value_box(class="shadow-none",
                                            theme = value_box_theme(fg = "white", bg = "#69aa41"),
                                            title = "Compare estimates of causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years by age, sex, region and year.",
                                            value = "Patterns"
                                  ),
                                  value_box(class="shadow-none",
                                            theme = value_box_theme(fg = "white", bg = "#69aa41"),
                                            title = "Explore the main causes of death, years of life lost, number of cases/prevalence, years lived with disability, and disability-adjusted life years by age, sex, region and year.", 
                                            value = "Trends"
                                  ),
                                  value_box(class="shadow-none",
                                            theme = value_box_theme(fg = "white", bg = "#69aa41"),
                                            title = "Explore ranks in causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years by age, sex, region and year.", 
                                            value = "Rankings"
                                  ),
                                  value_box(class="shadow-none",
                                            theme = value_box_theme(fg = "white", bg = "#69aa41"),
                                            title = "Explore the main causes of death, years of life lost, number of cases/prevalence, years lived with disability, and disability-adjusted life years by age, sex, region and year.", 
                                            value = "Treemap"
                                  )
                                )
                                
                      ),
                 nav_spacer(),
                 card(class="border-2 border-info text-primary mx-0 mx-sm-2 mb-xxl-4 p-0 shadow-none",
                      card_body(
                        plotOutput("plot", click = "plot_click"), 
                        textOutput("coords")
                        )
                      ),
                 card(class="border-2 border-info text-primary mx-0 mx-sm-2 mb-xxl-4 p-0 shadow-none",
                      layout_column_wrap(width = NULL, class="map_plot_resize gap-0", heights_equal = "row",
                                         card_body(class="m-lg-1 m-lg-3 p-0","Robby De Pauw, Vanessa Gorasso, Aline Scohy, Sarah Nayani, Rani Claerman, Laura Van den Borre, Jozefien Wellekens & Brecht Devleesschauwer. (2025). BeBOD estimates of mortality, years of life lost, prevalence, years lived with disability, and disability-adjusted life years for 38 causes, 2013-2022 (v2025-06-26) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.15574409")
                      )
                 )
                 )
                 
            )
)

server <- function(input, output, session) {
  observeEvent(input$Help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back",
                                               "skipLabel"="Close"))
  )
  
  output$plot <- renderPlot({ 
    data |> 
      ggplot(aes(date, value_pmmv)) + 
      geom_point() +
      scale_x_date(date_labels = "%d/%m/%y (W%V)", breaks = "8 weeks") +
      labs(title = "Click anywhere on the plot", x = "", y = "Viral ratio")
  })
  
  output$coords <- renderText({ 
    req(input$plot_click) # Ensure values are available before proceeding 
    x <- round(input$plot_click$x, 2) 
    y <- round(input$plot_click$y, 2) 
    glue::glue("({x}, {y})") 
  })
  
}

shinyApp(ui, server)