### BeBOD SHINY APP > DALY
### burden.sciensano.be/shiny/daly

## required packages
library(dplyr)
library(DT)
library(forcats)
library(googleVis)
library(highcharter)
library(bslib)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyr)
library(arrow)
library(data.table)
library(shinyBS)
library(DBI)
library(RPostgres)
library(dbplyr)
library(shiny.i18n)
library(shinychat)
library(ellmer)
library(curl)
library(querychat)
library(shinychat)

## translation
# i18n <- Translator$new(translation_csvs_path = "./daly_data/")
i18n <- Translator$new(translation_json_path = "translations/translation.json")
i18n$set_translation_language("nl")
# i18n$set_translation_language("fr")
# i18n$set_translation_language("nl")

## additional info
zenodo_link <- "10.5281/zenodo.15574409"
dta_time <- "2022" ## year of update

## include year as comparison?
include_year <- TRUE
year_options <- c("Location", "Sex", "Age", "Year")
no_year_options <- c("Location", "Sex", "Age")

## helpers
today <-
  function () {
    return(format(Sys.time(), "%Y%m%d"))
  }

fct_age <-
  function(x) {
    factor(x,
           levels = c("<5", "5-14", "15-44", "45-64", "65-84", "85+", "ALL", "ESP", "BSP"), 
           labels = c("0-4", "5-14", "15-44", "45-64", "65-84", "85+", "ALL", "ESP", "BSP"))
  }

fct_region <-
  function(x) {
    factor(x,
           levels = c("BE", "BR", "FL", "WA"), 
           labels = c("Belgium", "Brussels Capital Region", "Flemish Region", "Walloon Region"))
  }

fct_sex <-
  function(x) {
    factor(x,
           levels = c("MF", "M", "F"), 
           labels = c("Both sexes", "Men", "Women"))
  }

firstlower <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

fx_infobutton <- function(label) {
  shinyBS:::addClass(
    bsButton(
      label, 
      label = HTML("<i class='fa-solid fa-circle-info' style='font-size: 1.5em; color: #A5A5A5;'></i>"),
      size = "extra-small", disabled = FALSE),
    "btn-help")
}

fx_infotext_levels <- function(label) {
  bsPopover(
    id = label,
    title = "More information",
    content = paste0(
      ## ANY HTML can go here (HTML(), p(), a())
      HTML("<p>The Belgian national Burden of Disease (BeBOD) study categorizes causes of ill health in a structure with three levels.</p>"),
      HTML("<p><strong>Level 1 causes</strong>: This level defines 3 general groups of health issues, i.e. non-communicable diseases, injuries, and infectious (communicable) diseases.</p>"),
      HTML("<p><strong>Level 2 causes</strong>: This level defines broad disease groups, such as neoplasms, cardiovascular problems, or neurological disorders.</p>"),
      HTML("<p><strong>Level 3 causes</strong>: This level defines individual causes of ill health, such as low back pain, stroke, and road injuries.<p>")
    ),
    placement = "right",
    trigger = "hover",
    options = list(container = "body")
  )
}

fx_infotext_age <- function(label) {
  bsPopover(
    id = label,
    title = "More information",
    content = paste0(
      ## ANY HTML can go here (HTML(), p(), a())
      HTML("<p>The Belgian national Burden of Disease study (BeBOD) uses age standardisation to compare and analyze data from different regions or over time by eliminating the effects of varying age structures By applying age standardisation, researchers and policymakers can gain insights into trends or disparities while minimizing the confounding impact of age-related variations.</p>"),
      HTML("<p><strong>All ages</strong>: Non standardised (i.e., crude) estimate.</p>"),
      HTML("<p><strong>Age-standardised (BSP)</strong>: Age standardisation according to the Belgian population in the last observed year.</p>"),
      HTML("<p><strong>Age-standardised (ESP)</strong>: Age standardisation according to the European standard population 2013.<p>")
    ),
    placement = "right",
    trigger = "hover",
    options = list(container = "body")
  )
}

## validation message when combining ESP/BSP with NR
validate_agestd <-
  "Age-standardised values are not available for absolute numbers"

## groups
## .. age groups
agegroups_std <- list(
  "ALL" = "All ages",
  "0-4" = "0-4 years",
  "5-14" = "5-14 years",
  "15-44" = "15-44 years",
  "45-64" = "45-64 years",
  "65-84" = "65-84 years",
  "85+" = "85 years and over",
  "BSP" = "Age-standardised (BSP)",
  "ESP" = "Age-standardised (ESP)")

## .. sex groups
sexgroups_std <- list(
  "MF" = "both sexes",
  "M" = "men",
  "F" = "women")

## .. region groups
regiongroups_std <- list(
  "BE" = "Belgium",
  "BR" = "Brussels Capital Region",
  "FL" = "Flemish Region",
  "WA" = "Walloon Region")

## import data treemap ---
if (Sys.info()[1] == "Windows") {
  dta3 <- read_feather("daly_data/BeBoD-DALY-ALL-WIDE-2013-2022.feather")
} else {
  ## create connection to DB
  dta3 <- read_feather("/data/burden/data/daly2022/BeBoD-DALY-ALL-WIDE-2013-2022.feather")
}
dta3$AGE <- fct_age(dta3$AGE)
colnames(dta3) <- tolower(colnames(dta3))

# .. calculate file size
# if (Sys.info()[1] == "Windows") {
#   dta3_size <-
#     round(file.size("//10.0.1.81/epi_rstudio/burden/data/daly2022/BeBoD-DALY-ALL-WIDE-2013-2022.csv") / (1024^2), 1)
# } else {
#   dta3_size <-
#     round(file.size("/data/burden/data/daly2022/BeBoD-DALY-ALL-WIDE-2013-2022.csv") / (1024^2), 1)
# }

## import data trends ---
if (Sys.info()[1] == "Windows") {
  dta <- read_feather("daly_data/BeBoD-DALY-ALL-LONG-2013-2022.feather")
} else {
  dta <- dbReadTable(con, "daly_all_long_2013_2022")
  # dta <- read_feather("/data/burden/data/daly2022/BeBoD-DALY-ALL-LONG-2013-2022.feather")
}
dta$AGE <- fct_age(dta$AGE)

dta <- arrange(dta, YEAR, SEX, REGION, AGE, LEVEL, CAUSE)
colnames(dta) <- tolower(colnames(dta))

## add level-cause column
dta$level_cause <- paste0(dta$level, "|", dta$cause)


# .. calculate file size and date
if (Sys.info()[1] == "Windows") {
  # dta_size <-
  #   round(file.size("//10.0.1.81/epi_rstudio/burden/data/daly2022/BeBoD-DALY-ALL-LONG-2013-2022.csv") / (1024^2), 1)
  # dta_time <-
  #   format(file.mtime("//10.0.1.81/epi_rstudio/burden/data/daly2022/BeBoD-DALY-ALL-LONG-2013-2022.csv"), "%d %b %Y")
  
} else {
  # dta_size <-
  #   round(file.size("/data/burden/data/daly2022/BeBoD-DALY-ALL-LONG-2013-2022.csv") / (1024^2), 1)
  dta_time <-
    format(file.mtime("daly_data/BeBoD-DALY-ALL-LONG-2013-2022.csv"), "%d %b %Y")
}

## import data causelist ---
# dta_cause <- openxlsx::read.xlsx("www/data/BeBOD-causelist.xlsx")

# # .. create long file
# dta_cause_l <- pivot_longer(dta_cause, cols = c(Level0:Level3), 
#                             names_to = "level", 
#                             values_to = "cause")
# .. load colors

if (Sys.info()[1] == "Windows") {
  settings <- readRDS("www/BeBOD-DALY-latestsettings.rds")
} else {
  settings <- readRDS("www/BeBOD-DALY-latestsettings.rds")
}

# col_all <- settings$highcharter$col

# .. add to table

## specify query of database
# qc <- QueryChat$new(dta)

# Define UI for application that draws a histogram
ui <- tagList(
  shiny.i18n::usei18n(i18n),
  navbarPage(
    #### POSITION ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    position = c("fixed-top"),
    
    #### HEADER ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # header =  tagList(
    #   tags$ul(
    #     class = "nav navbar-nav navbar-right",
    #     tags$li(
    #       style = "display: flex; align-items: center; padding-right: 15px;",
    #       radioGroupButtons('lang',
    #                         label="Choose Language",
    #                         choices = i18n$get_languages(),
    #                         selected = i18n$get_key_translation()
    #                         )
    #       )
    #     )
    #   ),
    header = tagList(
      tags$ul(
        class = "nav navbar-nav navbar-right",
        # Dynamically create one button per language
        lapply(i18n$get_languages(), function(lang) {
          tags$li(
            tags$a(
              href = "#",
              class = "navbar-lang-btn",
              actionLink(
                inputId = paste0("lang_", lang),
                label = toupper(lang)
              ),
              style = "display: inline-block; padding: 10px 8px; line-height: 20px; font-size: 14px;"
            )
          )
        })
      )
    ),

    title = HTML(paste0(
      "<a href='https://www.sciensano.be'><img src='sciensano.png' height='20px'></a>&nbsp;&nbsp;&nbsp; <span style='color:#69aa41; font-size:1.1em;'>BeBOD &rsaquo; ",
      i18n$t("Disability-Adjusted Life Years"),
      "<span>"
    )),

    windowTitle = "BeBOD > Disability-Adjusted Life Years",

    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##+                   INFO #####
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    tabPanel(
      i18n$t("Info"),
      icon = icon("info-circle"),
      
      HTML(paste0("<h1 style='color:#69aa41;'>",i18n$t("Belgian National Burden of Disease Study"), " (BeBOD)</h1>")),
      tags$p(i18n$t(
        "What are the most important diseases in Belgium? Which risk factors contribute most to the overall disease burden? How is the burden of disease evolving over time, and how does it differ across the country? In a context of increasing budgetary constraints, a precise answer to these basic questions is more than ever necessary to inform policy-making."
      )),
      HTML("<p>To address this need, <b>Sciensano</b> is conducting a <b>national burden of disease study</b>. In addition to generating internally consistent estimates of death rate (mortality) or how unhealthy we are (morbidity) by age, sex and region, the burden of disease will also be quantified using Disability-Adjusted Life Years (DALYs). The use of DALYs allows to estimate the years of life lost from premature death and years of life lived with disabilities. It therefore permits a truly comparative ranking of the burden of various diseases, injuries and risk factors.</p>"),
      HTML("<p>For more info, please visit the <a href='https://www.sciensano.be/en/projects/belgian-national-burden-disease-study' target='_blank'>BeBOD project page</a> or read the <a href='https://www.sciensano.be/en/biblio/belgian-national-burden-disease-study-guidelines-calculation-disability-adjusted-life-years-belgium-0' target='_blank'>BeBOD protocol</a>.</p>"),
      
      HTML("<h4><i class='fa-solid fa-chart-simple'></i>&nbsp; Other BeBOD visualisation tools</h4>"),
      HTML("<p><a href='https://burden.sciensano.be/shiny/mortality' target='_blank'>Estimates of the fatal burden of 131 causes of death</a></p>"),
      HTML("<p><a href='https://burden.sciensano.be/shiny/cancer' target='_blank'>Estimates of the non-fatal burden of 57 cancer sites</a></p>"),
      HTML("<p><a href='https://burden.sciensano.be/shiny/risk' target='_blank'>Estimates of the risk factor attributable burden</a></p>"),
      HTML("<p><a href='https://burden.sciensano.be/shiny/projections' target='_blank'>Estimates of projected non-fatal burden of 33 causes</a></p>"),
      
      HTML("<h2 style='color:#69aa41;'>BeBOD estimates of the burden of disease</h2>"),
      
      h4("Disability-Adjusted Life Years (DALYs)"),
      p("Disability-Adjusted Life Year (DALYs) are a measure of overall disease burden, representing the healthy life years lost due to morbidity and mortality. DALYs are calculated as the sum of Years of Life Lost (YLLs) and Years Lived with Disability (YLDs) for each of the considered diseases."),
      
      h4("Years Lived with Disability (YLDs)"),
      HTML("<p>YLDs quantify the morbidity impact of diseases. They are calculated as the product of the number of prevalent cases with the disability weight (DW), averaged over the different health states of the disease. The DWs reflect the relative reduction in quality of life, on a scale from 0 (perfect health) to 1 (death). We calculate YLDs using the Global Burden of Disease DWs.</p>"),
      
      h4("Years of Life Lost (YLLs)"),
      p("YLLs quantify the mortality impact of diseases. They are calculated as the product of the number of deaths due to the disease with the residual life expectancy at the age of death. We calculate YLLs using the Global Burden of Disease reference life table, which represents the theoretical maximum number of years that people can expect to live."),
      
      h4("Number of prevalent cases"),
      p("The number of prevalent cases for each disease was calculated based on a variety of Belgian data sources, including the Belgian Cancer Registry, the Intermutualistic Agency, the Belgian Health Interview Survey, the Hospital Discharge Data, the Intego general practice sentinel network, and the European kidney registry (ERA-EDTA)."),
      
      h4("Causes of death"),
      HTML("<p>The number of deaths by cause of death are based on the official causes of death database compiled by <a href='https://statbel.fgov.be/en/themes/population/mortality-life-expectancy-and-causes-death/causes-death' target='_blank'>Statbel</a> We first map the ICD-10 codes of the underlying causes of death to the Global Burden of Disease cause list, consisting of 131 unique causes of deaths. Next, we perform a probabilistic redistribution of ill-defined deaths to specific causes, to obtain a specific cause of death for each deceased person (see <a href='https://archpublichealth.biomedcentral.com/articles/10.1186/s13690-023-01163-7' target='_blank'>Devleesschauwer et al. (2023)</a>).</p>"),
      
      HTML("<h2 style='color:#69aa41;'>Explore our estimates</h2>"),
      
      HTML("<h4><i class='fa fa-th-large'></i>&nbsp; Treemap</h4>"),
      p("Explore the main causes of death, years of life lost, number of cases/prevalence, years lived with disability, and disability-adjusted life years by age, sex, region and year."),
      
      HTML("<h4><i class='fa fa-chart-line'></i>&nbsp; Trends</h4>"),
      p("Explore trends in causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years, by age, sex and region."),
      
      HTML("<h4><i class='fa fa-sort-amount-up'></i>&nbsp; Rankings</h4>"),
      p("Explore ranks in causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years by age, sex, region and year."),
      
      HTML("<h4><i class='fa fa-chart-bar'></i>&nbsp; Patterns</h4>"),
      p("Compare estimates of causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years by age, sex, region and year."),
      
      HTML("<h4><i class='fa fa-database'></i>&nbsp; Results</h4>"),
      p("Explore and download our estimates of causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years."),
      
      HTML("<h2 style='color:#69aa41;'>Download all estimates</h2>"),
      HTML("<p>All estimates can be downloaded in <a href='https://arrow.apache.org/docs/r/reference/read_parquet.html' target='_blank'>Parquet format</a> via Zenodo:</p>"),
      HTML(sprintf("<p><a href='https://doi.org/%s' target='_blank'><img src='https://zenodo.org/badge/DOI/%s.svg' alt='DOI'></a></p>", zenodo_link, zenodo_link)),
      
      HTML("<h2 style='color:#69aa41;'>Citation</h2>"),
      HTML(sprintf("<p class='alert alert-warning'>Robby De Pauw, Vanessa Gorasso, Aline Scohy, Sarah Nayani, Rani Claerman, Laura Van den Borre, Jozefien Wellekens & Brecht Devleesschauwer. (2025). BeBOD estimates of mortality, years of life lost, prevalence, years lived with disability, and disability-adjusted life years for 38 causes, 2013-2022 (v2025-06-26) [Data set]. Zenodo. https://doi.org/%s</p>", zenodo_link)),
      HTML("<p><a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a><br />This work is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/' target='_blank'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.</p>")#,
      
      
      # HTML("<h2 style='color:#69aa41;'>Download all estimates</h2>"),
      # p("All estimates can be downloaded by clicking the button below."),
      # 
      # downloadButton(
      #   "downloadData",
      #   sprintf("Download all estimates (%s MB)", dta_size)
      # )
    ),
    
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##+                   TREEMAP #####
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    tabPanel(
      i18n$t("Treemap"),
      icon = icon("th-large"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h3(i18n$t("Chart settings"), style = "color:#69aa41;"),
          
          pickerInput(
            inputId = "measure_treemap", 
            label = "Measure",
            choices = list(
              "Disability-Adjusted Life Years (DALYs)" = "DALY",
              "Years Lived with Disability (YLDs)" = "YLD",
              "Years of Life Lost (YLLs)" = "YLL",
              "Prevalent cases" = "CASES",
              "Deaths" = "DEATHS"), 
            selected = "DALY"),
          
          sliderInput(
            inputId = "year_treemap",
            label = "Year",
            min = min(dta3$year),
            max = max(dta3$year),
            step = 1,
            value = max(dta3$year),
            sep = ""),
          
          selectInput(
            inputId = "age_treemap",
            label = "Age",
            choices = list(
              "All ages" = "ALL",
              "0-4 years" = "0-4",
              "5-14 years" = "5-14",
              "15-44 years" = "15-44",
              "45-64 years" = "45-64",
              "65-84 years" = "65-84",
              "85 years and over" = "85+"
            )
          ),
          
          radioGroupButtons(
            inputId = "sex_treemap",
            label = "Sex",
            choices = list(
              "Both sexes" = "MF",
              "Men" = "M",
              "Women" = "F"
            ),
            justified = TRUE
          ),
          
          pickerInput(
            inputId = "region_treemap",
            label = "Location",
            choices = list(
              "Belgium" = "BE",
              "Brussels Capital Region" = "BR",
              "Flemish Region" = "FL",
              "Walloon Region" = "WA"
            ),
            selected = "BE"
          ),
          
          hr(),
          
          radioGroupButtons(
            inputId = "level_treemap",
            label = tags$span(
              "Show level", 
              fx_infobutton("btn_level_treemap")
            ),
            choices = list(
              "Level 1" = 1,
              "Level 2" = 2,
              "Level 3" = 3
            ),
            justified = TRUE,
            selected = 3
          ),
          fx_infotext_levels("btn_level_treemap")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          HTML("<p><b>Left-click</b> on any disease group to drill down. <b>Right-click</b> to return to the higher level.</p>"),
          htmlOutput("tm")
        )
      )
    ),
    
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##+                   TRENDS #####
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    tabPanel(
      i18n$t("Trends"),
      icon = icon("chart-line"),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h3(i18n$t("Chart settings"), style = "color:#69aa41;"),
          
          conditionalPanel(
            condition = "input.group_trends == 'Cause'",
            selectizeInput("cause_trends_multi",
                          "Cause",
                          multiple = TRUE,
                          choices = NULL)
          ),
          
          conditionalPanel(
            condition = "input.group_trends != 'Cause'",
            selectizeInput("cause_trends",
                          "Cause",
                          choices = NULL)
          ),
          
          pickerInput(
            inputId = "measure_trends", 
            label = "Measure",
            choices = list(
              "Disability-Adjusted Life Years (DALYs)" = "DALY",
              "Years Lived with Disability (YLDs)" = "YLD",
              "Years of Life Lost (YLLs)" = "YLL",
              "Prevalent cases" = "CASES",
              "Deaths" = "DEATHS"), 
            selected = "DALY"),
          radioGroupButtons(inputId = "unit_trends",
                            label = "Metric",
                            choices = list("Number" = "NR", "Rate" = "RT"),
                            justified = TRUE,
                            selected = "RT"),
          radioGroupButtons(inputId = "group_trends",
                            label = "Comparison",
                            choices = c("Location", "Sex", "Age", "Cause"),
                            justified = TRUE),
          hr(),
          conditionalPanel(
            condition = "input.group_trends != 'Age'",
            
            selectInput(
              inputId = "age_trends",
              label = tags$span(
                "Age", 
                fx_infobutton("btn_age_trends")
              ),
              choices = list(
                "All ages" = "ALL",
                "Age-standardised (BSP)" = "BSP",
                "Age-standardised (ESP)" = "ESP",
                "0-4 years" = "0-4",
                "5-14 years" = "5-14",
                "15-44 years" = "15-44",
                "45-64 years" = "45-64",
                "65-84 years" = "65-84",
                "85 years and over" = "85+"
              ),
              selected = "BSP"),
            fx_infotext_age("btn_age_trends")
          ),
          conditionalPanel(
            condition = "input.group_trends != 'Sex'",
            
            radioGroupButtons(
              inputId = "sex_trends",
              label = "Sex",
              choices = list(
                "Both sexes" = "MF",
                "Men" = "M",
                "Women" = "F"
              ),
              justified = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.group_trends != 'Location'",
            
            pickerInput(
              inputId = "region_trends",
              label = "Location",
              choices = list(
                "Belgium" = "BE",
                "Brussels Capital Region" = "BR",
                "Flemish Region" = "FL",
                "Walloon Region" = "WA"
              ),
              selected = "BE"
            ),
          ),
          hr(),
          materialSwitch(
            inputId = "scaleaxis_trends",
            label = "Start Y axis at zero", 
            value = FALSE,
            status = "success"
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          highchartOutput("hc", height = "550px")
        )
      )
    ),
    
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##+                   RANKINGS #####
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    tabPanel(
      i18n$t("Rankings"),
      icon = icon("sort-amount-up"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h3(i18n$t("Chart settings"), style = "color:#69aa41;"),
          
          pickerInput(
            inputId = "measure_heatmap", 
            label = "Measure",
            choices = list(
              "Disability-Adjusted Life Years (DALYs)" = "DALY",
              "Years Lived with Disability (YLDs)" = "YLD",
              "Years of Life Lost (YLLs)" = "YLL",
              "Prevalent cases" = "CASES",
              "Deaths" = "DEATHS"), 
            selected = "DALY"),
          
          radioGroupButtons(
            inputId = "rank_heatmap",
            label = "Metric",
            choices = list(
              "Rate" = FALSE,
              "Rank" = TRUE
            ),
            justified = TRUE),
          
          radioGroupButtons(
            inputId = "group_heatmap",
            label = "Comparison",
            choices = c("Location", "Sex", "Age", "Year"),
            justified = TRUE),
          
          hr(),
          
          conditionalPanel(
            condition = "input.group_heatmap == 'Location'",
            
            selectizeInput(
              inputId = "sort_heatmap_region",
              label = "Sort by",
              choices = 
                list(
                  "Belgium" = "BE",
                  "Brussels Capital Region" = "BR",
                  "Flemish Region" = "FL",
                  "Walloon Region" = "WA"
                )
            )
          ),
          
          conditionalPanel(
            condition = "input.group_heatmap == 'Sex'",
            
            radioGroupButtons(
              inputId = "sort_heatmap_sex",
              label = "Sort by",
              choices = list(
                "Both sexes" = "MF", 
                "Men" = "M", 
                "Women" = "F"),
              justified = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.group_heatmap == 'Age'",
            
            selectInput(
              inputId = "sort_heatmap_age",
              label = "Sort by",
              choices = list(
                "0-4 years" = "0-4",
                "5-14 years" = "5-14",
                "15-44 years" = "15-44",
                "45-64 years" = "45-64",
                "65-84 years" = "65-84",
                "85 years and over" = "85+"),
              selected = "85+")
          ),
          
          conditionalPanel(
            condition = "input.group_heatmap == 'Year'",
            
            selectInput(
              inputId = "sort_heatmap_year",
              label = "Sort by",
              choices = unique(dta$year),
              selected = max(dta$year))
          ),
          
          conditionalPanel(
            condition = "input.group_heatmap != 'Year'",
            
            sliderInput(
              inputId = "year_heatmap",
              label = "Year",
              min = min(dta$year),
              max = max(dta$year),
              step = 1,
              value = max(dta$year),
              sep = "")
          ),
          
          conditionalPanel(
            condition = "input.group_heatmap != 'Age'",
            
            selectInput(
              inputId = "age_heatmap",
              label = tags$span(
                "Age", 
                fx_infobutton("btn_age_heatmap")
              ),
              choices = list(
                "All ages" = "ALL",
                "Age-standardised (BSP)" = "BSP",
                "Age-standardised (ESP)" = "ESP",
                "0-4 years" = "0-4",
                "5-14 years" = "5-14",
                "15-44 years" = "15-44",
                "45-64 years" = "45-64",
                "65-84 years" = "65-84",
                "85 years and over" = "85+"
              ),
              selected = "BSP"),
            fx_infotext_age("btn_age_heatmap")
          ),
          
          conditionalPanel(
            condition = "input.group_heatmap != 'Sex'",
            
            radioGroupButtons(
              inputId = "sex_heatmap",
              label = "Sex",
              choices = list(
                "Both sexes" = "MF",
                "Men" = "M",
                "Women" = "F"),
              justified = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.group_heatmap != 'Location'",
            
            pickerInput(
              inputId = "region_heatmap",
              label = "Location",
              choices = list(
                "Belgium" = "BE",
                "Brussels Capital Region" = "BR",
                "Flemish Region" = "FL",
                "Walloon Region" = "WA"
              ),
              selected = "BE"
            ),
          ),
          
          radioGroupButtons(
            inputId = "level_heatmap",
            label = tags$span(
              "Show level", 
              fx_infobutton("btn_level_heatmap")
            ),
            choices = list(
              "Level 1" = 1,
              "Level 2" = 2,
              "Level 3" = 3
            ),
            justified = TRUE,
            selected = 3
          ),
          fx_infotext_levels("btn_level_heatmap")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          highchartOutput("hm", height = "80vh")
        )
      )
    ),
    
    # tabPanel(
    #   "Rankings",
    #   icon = icon("sort-amount-up"),
    #   
    #   # Sidebar with a slider input for number of bins 
    #   sidebarLayout(
    #     sidebarPanel(
    #       width = 3,
    #       h3(i18n$t("Chart settings"), style = "color:#69aa41;"),
    #       
    #       pickerInput(
    #         inputId = "measure_heatmap", 
    #         label = "Measure",
    #         choices = list(
    #           "Disability-Adjusted Life Years (DALYs)" = "DALY",
    #           "Years Lived with Disability (YLDs)" = "YLD",
    #           "Years of Life Lost (YLLs)" = "YLL",
    #           "Prevalent cases" = "CASES",
    #           "Deaths" = "DEATHS"), 
    #         selected = "DALY"),
    #       
    #       radioGroupButtons(
    #         inputId = "rank_heatmap",
    #         label = "Metric",
    #         choices = list(
    #           "Rate" = FALSE,
    #           "Rank" = TRUE
    #         ),
    #         justified = TRUE),
    #       
    #       radioGroupButtons(
    #         inputId = "group_heatmap",
    #         label = "Comparison",
    #         choices = (if (include_year) {year_options} else {no_year_options}),
    #         justified = TRUE),
    #       
    #       hr(),
    #       
    #       uiOutput("sort"),
    #       
    #       conditionalPanel(
    #         condition = "input.group_heatmap != 'Year'",
    #         
    #         sliderInput(
    #           inputId = "year_heatmap",
    #           label = "Year",
    #           min = min(dta$year),
    #           max = max(dta$year),
    #           step = 1,
    #           value = max(dta$year),
    #           sep = "")
    #       ),
    #       
    #       conditionalPanel(
    #         condition = "input.group_heatmap != 'Age'",
    #         
    #         selectInput(
    #           inputId = "age_heatmap",
    #           label = "Age",
    #           choices = list(
    #             "All ages" = "ALL",
    #             "0-5" = "[0,5)",
    #             "5-14" = "[5,15)",
    #             "15-44" = "[15,45)",
    #             "45-64" = "[45,65)",
    #             "65+" = "[65,Inf)",
    #             "Age-standardised (BSP)" = "BSP",
    #             "Age-standardised (ESP)" = "ESP"),
    #           selected = "ALL")
    #       ),
    #       
    #       conditionalPanel(
    #         condition = "input.group_heatmap != 'Sex'",
    #         
    #         radioGroupButtons(
    #           inputId = "sex_heatmap",
    #           label = "Sex",
    #           choices = list(
    #             "Both sexes" = "MF",
    #             "Men" = "M",
    #             "Women" = "F"),
    #           justified = TRUE)
    #       ),
    #       
    #       conditionalPanel(
    #         condition = "input.group_heatmap != 'Region'",
    #         
    #         radioGroupButtons(
    #           inputId = "region_heatmap",
    #           label = "Region",
    #           choices = list(
    #             "Belgium" = "BE",
    #             "Brussels" = "BR",
    #             "Flanders" = "FL",
    #             "Wallonia" = "WA"),
    #           justified = TRUE)
    #       ),
    #       
    #       radioGroupButtons(
    #         inputId = "level_heatmap",
    #         label = "Show level",
    #         choices = list(
    #           "Level 1" = 1,
    #           "Level 2" = 2,
    #           "Level 3" = 3
    #         ),
    #         justified = TRUE,
    #         selected = 3)
    #     ),
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #       highchartOutput("hm", height = "80vh")
    #     )
    #   )
    # ),
    
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##+                   PATTERNS #####
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    tabPanel(
      i18n$t("Patterns"),
      icon = icon("chart-bar"),
      
      # Sidebar layout
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h3(i18n$t("Chart settings"), style = "color:#69aa41;"),
          
          pickerInput(
            inputId = "measure_bars", 
            label = "Measure",
            choices = list(
              "Disability-Adjusted Life Years (DALYs)" = "DALY",
              "Years Lived with Disability (YLDs)" = "YLD",
              "Years of Life Lost (YLLs)" = "YLL",
              "Prevalent cases" = "CASES",
              "Deaths" = "DEATHS"), 
            selected = "DALY"),
          
          radioGroupButtons(
            inputId = "metric_bars",
            label = "Metric",
            choices = c("Number" = "NR", "Rate" = "RT"),
            justified = TRUE),
          
          
          radioGroupButtons(
            inputId = "group_bars",
            label = "Comparison",
            choices = year_options, 
            justified = TRUE,
            selected = "Age"),
          
          hr(),
          
          conditionalPanel(
            condition = "input.group_bars != 'Year'",
            
            sliderInput(
              inputId = "year_bars",
              label = "Year",
              min = min(dta$year),
              max = max(dta$year),
              step = 1,
              value = max(dta$year),
              sep = "")
          ),
          
          conditionalPanel(
            condition = "input.group_bars != 'Age'",
            
            selectInput(
              inputId = "age_bars",
              label = tags$span(
                "Age", 
                fx_infobutton("btn_age_bars")
              ),
              choices = list(
                "All ages" = "ALL",
                "Age-standardised (BSP)" = "BSP",
                "Age-standardised (ESP)" = "ESP",
                "0-4 years" = "0-4",
                "5-14 years" = "5-14",
                "15-44 years" = "15-44",
                "45-64 years" = "45-64",
                "65-84 years" = "65-84",
                "85 years and over" = "85+"
              ),
              selected = "BSP"),
            fx_infotext_age("btn_age_bars")
          ),
          
          conditionalPanel(
            condition = "input.group_bars != 'Sex'",
            
            radioGroupButtons(
              inputId = "sex_bars",
              label = "Sex",
              choices = list(
                "Both sexes" = "MF",
                "Men" = "M",
                "Women" = "F"),
              justified = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.group_bars != 'Location'",
            
            pickerInput(
              inputId = "region_bars",
              label = "Location",
              choices = list(
                "Belgium" = "BE",
                "Brussels Capital Region" = "BR",
                "Flemish Region" = "FL",
                "Walloon Region" = "WA"
              ),
              selected = "BE"
            ),
          ),
          
          radioGroupButtons(
            inputId = "level_bars",
            label = tags$span(
              "Show level", 
              fx_infobutton("btn_level_bars")
            ),
            choices = list(
              "Level 1" = 1,
              "Level 2" = 2,
              "Level 3" = 3
            ),
            justified = TRUE,
            selected = 2
          ),
          fx_infotext_levels("btn_level_bars"),
          
          radioGroupButtons(
            inputId = "stacked_bars",
            label = "Display mode",
            choices = list(
              "Values" = "normal",
              "Percentages" = "percent"),
            justified = TRUE, 
            selected = "percent")
        ),
        
        # Show plot
        mainPanel(
          highchartOutput("bars", height = "80vh")
        )
      )
    ),
    
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##+                   RESULTS #####
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    tabPanel(
      i18n$t("Results"),
      icon = icon("database"),
      
      # Sidebar layout
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h3("Table settings", style = "color:#69aa41;"),
          helpText("You can select multiple options per variable."),
          hr(),
          
          selectizeInput(
            "cause_dt",
            "Cause",
            choices = NULL,
            multiple = TRUE),
          
          selectizeInput(
            "year_dt",
            "Year",
            choices = unique(dta$year),
            multiple = TRUE,
            selected = max(dta$year)),
          
          pickerInput(
            inputId = "measure_dt", 
            label = "Measure",
            choices = list(
              "Disability-Adjusted Life Years (DALYs)" = "DALY",
              "Years Lived with Disability (YLDs)" = "YLD",
              "Years of Life Lost (YLLs)" = "YLL",
              "Prevalent cases" = "CASES",
              "Deaths" = "DEATHS"), 
            multiple = TRUE,
            selected = "DALY"),
          
          checkboxGroupButtons(
            inputId = "metric_dt",
            label = "Metric",
            choices = list(
              "Number" = "NR",
              "Rate" = "RT"),
            justified = TRUE,
            selected = "NR"),
          
          selectInput(
            inputId = "age_dt",
            label = tags$span(
              "Age", 
              fx_infobutton("btn_age_dt")
            ),
            choices = list(
              "All ages" = "ALL",
              "Age-standardised (BSP)" = "BSP",
              "Age-standardised (ESP)" = "ESP",
              "0-4 years" = "0-4",
              "5-14 years" = "5-14",
              "15-44 years" = "15-44",
              "45-64 years" = "45-64",
              "65-84 years" = "65-84",
              "85 years and over" = "85+"
            ),
            multiple = TRUE,
            selected = "ALL"),
          fx_infotext_age("btn_age_dt"),
          
          checkboxGroupButtons(
            inputId = "sex_dt",
            label = "Sex",
            choices = list(
              "Both sexes" = "MF",
              "Men" = "M",
              "Women" = "F"),
            justified = TRUE,
            selected = "MF"),
          
          pickerInput(
            inputId = "region_dt",
            label = "Location",
            choices = list(
              "Belgium" = "BE",
              "Brussels Capital Region" = "BR",
              "Flemish Region" = "FL",
              "Walloon Region" = "WA"
            ),
            multiple = TRUE,
            selected = "BE"
          ),
          
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          DT::dataTableOutput("dt")
        )
      )
    ),
    
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##+                   LLM interaction with data #####
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # tabPanel(
    #   title = "AI",
    #   icon = icon("database"),
    #   sidebarLayout(
    #     sidebarPanel(
    #       width = 3,
    #       qc$ui()
    #     ),
    #     mainPanel(
    #       DT::DTOutput("dt_llm")
    #       )
    #   )
    # ),
    

    #### FOOTER ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    footer = tagList(
      tags$script(src = "https://kit.fontawesome.com/dd752de6fc.js"),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(HTML('
            var fakeClick = function(tabName) {
              var dropdownList = document.getElementsByTagName("a");
              for (var i = 0; i < dropdownList.length; i++) {
                var link = dropdownList[i];
                if(link.getAttribute("data-value") == tabName) {
                  link.click();
                };
              }
            };
          '))
      ),
      
      tags$footer(
        HTML(sprintf("Sciensano %s &bullet; Last update", format(Sys.Date(), "%Y")),
            dta_time,
            "&bullet; <a href='mailto:hsr@sciensano.be?subject=BeBOD'>Contact</a>"),
        align = "left")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## --- update translation ----
  # observeEvent(input$lang, {
  #   print(input$lang)
  #   shiny.i18n::update_lang(input$lang, session=session)
  # })
  # Observe each language button
  lapply(i18n$get_languages(), function(lang) {
    observeEvent(input[[paste0("lang_", lang)]], {
      shiny.i18n::update_lang(lang)
    }, ignoreInit = TRUE)
  })

  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   SHOW/HIDE #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$include_year <- reactive({include_year})
  
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   TREEMAP #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## TREEMAP
  output$tm <-
    renderGvis({
      dta_tm <-
        subset(dta3,
               measure == input$measure_treemap &
                 age == input$age_treemap &
                 sex == input$sex_treemap &
                 region == input$region_treemap &
                 year == input$year_treemap &
                 metric == "NR")
      
      ## only non-missing
      dta_tm <- dta_tm[!is.na(dta_tm$value),]
      
      dta_tm$level2 <- toupper(dta_tm$level2)
      
      dtaG1 <-
        dta_tm[, c("level3", "level2", "value")]
      
      dtaG2 <-
        aggregate(value ~ level2, dtaG1, sum)
      dtaG2 <-
        data.frame(
          CHILD = dtaG2$level2,
          PARENT =
            toupper(
              dta_tm$level1[
                match(dtaG2$level2,
                      toupper(dta_tm$level2))]
            ),
          value = dtaG2$value)
      
      dtaG3 <-
        aggregate(value ~ PARENT, dtaG2, sum)
      dtaG3 <-
        data.frame(
          CHILD = dtaG3$PARENT,
          PARENT = "TOTAL",
          value = dtaG3$value)
      
      dtaG4 <-
        data.frame(
          CHILD = "TOTAL",
          PARENT = NA,
          value = sum(dtaG3$value))
      
      names(dtaG1) <- c("CHILD", "PARENT", "value")
      dtaG <- rbind(dtaG1, dtaG2, dtaG3, dtaG4)
      
      dtaG$pct <- round(dtaG$value / max(dtaG$value, na.rm = TRUE), 3)
      dtaG$value <- round(dtaG$value, 0)
      
      tm <-
        gvisTreeMap(
          dtaG,
          idvar = "CHILD",
          parentvar = "PARENT",
          sizevar = "value",
          colorvar = "pct",
          options =
            list(
              showScale = TRUE,
              #maxColor = grDevices::rgb(red = 201, green = 81, blue = 53, alpha = 23, maxColorValue = 255),
              #midColor = grDevices::rgb(red = 250, green = 213, blue = 0, alpha = 255, maxColorValue = 255),
              #minColor = grDevices::rgb(red = 58, green = 170, blue = 53, alpha = 255, maxColorValue = 255),
              maxColor = "#c95135",
              #midColor = "#fad500",
              minColor = "#3aaa35",
              maxDepth = input$level_treemap,
              maxPostDepth = 3,
              height = "600",
              width = "900",
              highlightOnMouseOver = TRUE))
      
      tm$html$chart["jsDrawChart"] <-
        gsub(
          ";\n\n\n    var chart",
          "\noptions[\"generateTooltip\"] = showTooltip;

  function showTooltip(row, size, value) {
    return '<div style=\"background:#fd9; padding:10px; border-style:solid\">' +
           '<b>' + data.getValue(row, 0) + '</b>' +
           '<br>' +
           'Value: ' + numberFormat.formatValue(size) +
           '<br>' +
           '% of total: ' + percentFormat.formatValue(data.getValue(row, 3)) +
           '</div>';
  }

  var numberFormat = new google.visualization.NumberFormat({
      pattern: '#,##0',
    });

  var percentFormat = new google.visualization.NumberFormat({
      pattern: '###.#%',
    });\n\n\n    var chart",
          tm$html$chart["jsDrawChart"])
      
      tm
      
      
    })
  
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   RANKINGS - HEATMAP #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$hm <-
    renderHighchart({
      yTitle <-
        switch(input$measure_heatmap,
               "DEATHS" = "Death rate per 100,000 persons",
               "CASES" = "Prevalence per 100,000 persons",
               "YLL" = "YLL rate per 100,000 persons",
               "YLD" = "YLD rate per 100,000 persons",
               "DALY" = "DALY rate per 100,000 persons")
      
      yTitle2 <-
        switch(input$measure_heatmap,
               "DEATHS" = "death rate per 100,000 persons",
               "CASES" = "prevalence per 100,000 persons",
               "YLL" = "YLL rate per 100,000 persons",
               "YLD" = "YLD rate per 100,000 persons",
               "DALY" = "DALY rate per 100,000 persons")
      
      mainTitle <-
        paste0("Top causes of disease burden by ",
               tolower(input$group_heatmap),
               ", according to ",
               yTitle2,
               if (input$group_heatmap != "Age")
                 paste0(", ",
                        firstlower(agegroups_std[[input$age_heatmap]])),
               if (input$group_heatmap != "Sex")
                 paste0(", ",
                        firstlower(sexgroups_std[[input$sex_heatmap]])),
               if (!(input$group_heatmap %in% c("Location")))
                 paste0(", ",
                        regiongroups_std[[input$region_heatmap]]),
               if (input$group_heatmap != "Year")
                 paste0(", ",
                        input$year_heatmap))
      
      if (input$group_heatmap != "Age")
        yTitle <- paste0(yTitle, ", ", firstlower(agegroups_std[[input$age_heatmap]]))
      
      if (input$group_heatmap == "Location") {
        dta_hm <-
          data.table(dta)[age %in% input$age_heatmap][
            year %in% input$year_heatmap][
              sex %in% input$sex_heatmap][
                level %in% input$level_heatmap][
                  metric %in% "RT"][
                    measure %in% input$measure_heatmap]
        dta_hm$group <- dta_hm$region
        sort_heatmap <- input$sort_heatmap_region
        dta_hm$value <- round(dta_hm$value, 2)
        
      } else if (input$group_heatmap == "Sex") {
        dta_hm <-
          data.table(dta)[age %in% input$age_heatmap][
            year %in% input$year_heatmap][
              region %in% input$region_heatmap][
                level %in% input$level_heatmap][
                  metric %in% "RT"][
                    measure %in% input$measure_heatmap]
        dta_hm$group <- dta_hm$sex
        sort_heatmap <- input$sort_heatmap_sex
        dta_hm$value <- round(dta_hm$value, 2)
        
      } else if (input$group_heatmap == "Age") {
        dta_hm <-
          data.table(dta)[year %in% input$year_heatmap][
            region %in% input$region_heatmap][
              sex %in% input$sex_heatmap][
                level %in% input$level_heatmap][
                  metric %in% "RT"][
                    measure %in% input$measure_heatmap][
                      !(age %in% c("ALL", "BSP", "ESP"))]
        dta_hm$group <- dta_hm$age
        sort_heatmap <- input$sort_heatmap_age
        dta_hm$value <- round(dta_hm$value, 2)
        
      } else {
        dta_hm <-
          data.table(dta)[age %in% input$age_heatmap][
            region %in% input$region_heatmap][
              sex %in% input$sex_heatmap][
                level %in% input$level_heatmap][
                  metric %in% "RT"][
                    measure %in% input$measure_heatmap]
        dta_hm$group <- as.character(dta_hm$year)
        sort_heatmap <- input$sort_heatmap_year
        dta_hm$value <- round(dta_hm$value, 2)
        
      }
      
      class(dta_hm) <- "data.frame"
      
      if (input$rank_heatmap) {
        dta_hm <- dta_hm[order(dta_hm$group), ]
        dta_hm$value <- 
          unlist(with(dta_hm, tapply(-value, group, rank)))
        
        dta_hm_top_causes <-
          tail(subset(dta_hm, group == sort_heatmap)[
            order(subset(dta_hm, group == sort_heatmap)$value,
                  decreasing = input$rank_heatmap),
            "cause"], 20)
        dta_hm_top <-
          subset(dta_hm, cause %in% dta_hm_top_causes)
        dta_hm_top$cause <-
          factor(dta_hm_top$cause, dta_hm_top_causes)
        
        if (nrow(dta_hm_top) > 0)
          if(input$group_heatmap == "Age"){
            # dta_hm_top$group <- fct_age(dta_hm_top$group)
          } else if(input$group_heatmap == "Sex"){
            dta_hm_top$group <- fct_sex(dta_hm_top$group)
          } else if(input$group_heatmap == "Location"){
            dta_hm_top$group <- fct_region(dta_hm_top$group)
          }
        
        hchart(dta_hm_top,
               hcaes(x = group, y = cause, value = value),
               type = "heatmap",
               name = yTitle) %>%
          hc_exporting(
            enabled = TRUE,
            buttons = list(
              contextButton = list(
                menuItems = list(
                  'viewFullscreen', 
                  'separator', 
                  'downloadCSV', 
                  'downloadXLS')
              ))
          ) %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_yAxis(title = list(text = "")) %>%
          hc_xAxis(title = list(text = NULL)) %>%
          hc_plotOptions(
            series = list(dataLabels = list(
              enabled = TRUE,
              format = "{point.value:,.0f}"))) %>% 
          hc_colorAxis(
            dataClassColor = "category",
            dataClasses = list(
              list(from = 1, to = 1, color = "rgb(215, 48, 39)"),
              list(from = 2, to = 2, color = "rgb(252, 141, 89)"),
              list(from = 3, to = 4, color = "rgb(254, 224, 144)"),
              list(from = 5, to = 7, color = "rgb(224, 243, 248)"),
              list(from = 8, to = 13, color = "rgb(145, 191, 219)"),
              list(from = 14, to = 20, color = "rgb(69, 117, 180)"),
              list(from = 21, color = "black"))) %>%
          hc_legend(enabled = FALSE) %>%
          hc_tooltip(
            formatter = JS("function(){
  return this.series.xAxis.categories[this.point.x] + '<br>' + this.series.name + '<br><b>' +  this.series.yAxis.categories[this.point.y] + ': #' +
  Highcharts.numberFormat(this.point.value, 0) + '</b>';}")) %>%
          hc_title(text = mainTitle)
        
      } else {
        dta_hm_top_causes <-
          tail(subset(dta_hm, group == sort_heatmap)[
            order(subset(dta_hm, group == sort_heatmap)$value,
                  decreasing = input$rank_heatmap),
            "cause"], 20)
        dta_hm_top <-
          subset(dta_hm, cause %in% dta_hm_top_causes)
        dta_hm_top$cause <-
          factor(dta_hm_top$cause, dta_hm_top_causes)
        
        if (nrow(dta_hm_top) > 0)
          if(input$group_heatmap == "Age"){
            # dta_hm_top$group <- fct_age(dta_hm_top$group)
          } else if(input$group_heatmap == "Sex"){
            dta_hm_top$group <- fct_sex(dta_hm_top$group)
          } else if(input$group_heatmap == "Location"){
            dta_hm_top$group <- fct_region(dta_hm_top$group)
          }
        
        hchart(dta_hm_top,
               hcaes(x = group, y = cause, value = value),
               type = "heatmap",
               name = yTitle, 
               showInLegend = FALSE) %>%
          hc_exporting(
            enabled = TRUE,
            buttons = list(
              contextButton = list(
                menuItems = list(
                  'viewFullscreen', 
                  'separator', 
                  'downloadCSV', 
                  'downloadXLS')
              ))
          ) %>%        
          hc_add_theme(hc_theme_smpl()) %>%
          hc_yAxis(title = list(text = "")) %>%
          hc_xAxis(title = NULL) %>%
          hc_plotOptions(
            series = list(dataLabels = list(
              enabled = TRUE,
              format = "{point.value:,.2f}"))) %>%
          hc_colorAxis(
            stops = color_stops(10, rev(RColorBrewer::brewer.pal(10, "Spectral"))),
            min = 0) %>%
          # hc_legend(enabled = TRUE) %>%
          hc_tooltip(
            formatter = JS("function(){
return this.series.xAxis.categories[this.point.x] + '<br>' + this.series.name + '<br><b>' +  this.series.yAxis.categories[this.point.y] + ': ' +
Highcharts.numberFormat(this.point.value, 2) + '</b>';}")) %>%
          hc_title(text = mainTitle)
      }
    })
  
  # ## conditional UI based on choice of comparison
  # output$sort <-
  #   renderUI({
  #     req(input$group_heatmap)
  #     if (input$group_heatmap == "Region") {
  #       radioGroupButtons(
  #         inputId = "sort_heatmap",
  #         label = "Sort by",
  #         choices = c("Belgium" = "BE", 
  #                     "Brussels" = "BR", 
  #                     "Flanders" = "FL", 
  #                     "Wallonia" = "WA"),
  #         justified = TRUE)
  #       
  #     } else if (input$group_heatmap == "Sex") {
  #       radioGroupButtons(
  #         inputId = "sort_heatmap",
  #         label = "Sort by",
  #         choices = c("Both sexes" = "MF", 
  #                     "Men" = "M", 
  #                     "Women" = "F"),
  #         justified = TRUE)
  #       
  #     } else if (input$group_heatmap == "Age") {
  #       selectInput(
  #         inputId = "sort_heatmap",
  #         label = "Sort by",
  #         choices = list(
  #           "0-5" = "[0,5)",
  #           "5-14" = "[5,15)",
  #           "15-44" = "[15,45)",
  #           "45-64" = "[45,65)",
  #           "65+" = "[65,Inf)"),
  #         selected = "[65,Inf)")
  #       
  #     } else {
  #       selectInput(
  #         inputId = "sort_heatmap",
  #         label = "Sort by",
  #         choices = unique(dta$year),
  #         selected = max(dta$year))
  #     }
  #   }) ## END CONDITIONAL UI
  # 
  # ## RENDER HIGHCHART HEATMAP
  # output$hm <-
  #   renderHighchart({
  #     
  #     req(input$measure_heatmap)
  #     req(input$age_heatmap)
  #     req(input$sex_heatmap)
  #     req(input$region_heatmap)
  #     
  #     yTitle <-
  #       switch(input$measure_heatmap,
  #              "DEATHS" = "Death rate per 100,000 persons",
  #              "CASES" = "Prevalence per 100,000 persons",
  #              "YLL" = "YLL rate per 100,000 persons",
  #              "YLD" = "YLD rate per 100,000 persons",
  #              "DALY" = "DALY rate per 100,000 persons")
  # 
  #     if (input$group_heatmap != "Age")
  #       yTitle <- paste0(yTitle, ", ",
  #                        switch(input$age_heatmap,
  #                               "ALL" = "all ages",
  #                               "[0,5)" = "age group 0-4",
  #                               "[5,14)" = "age group 5-14",
  #                               "[15,44)" = "age group 15-44",
  #                               "[45,64)" = "age group 45-64",
  #                               "[65,84)" = "age group 65-84",
  #                               "[85,Inf)" = "age group 85+",
  #                               "BSP" = "age-standardised (BSP)",
  #                               "ESP" = "age-standardised (ESP)"))
  #     
  #     if (input$group_heatmap == "Region") {
  #       dta_hm <-
  #         subset(dta,
  #                level == input$level_heatmap &
  #                  metric == "RT" &
  #                  measure == input$measure_heatmap &
  #                  age == input$age_heatmap &
  #                  sex == input$sex_heatmap &
  #                  year == input$year_heatmap)
  #       dta_hm$group <- dta_hm$region
  #       
  #     } else if (input$group_heatmap == "Sex") {
  #       dta_hm <-
  #         subset(dta,
  #                level == input$level_heatmap &
  #                  metric == "RT" &
  #                  measure == input$measure_heatmap &
  #                  age == input$age_heatmap &
  #                  region == input$region_heatmap &
  #                  year == input$year_heatmap)
  #       dta_hm$group <- dta_hm$sex
  #       
  #     } else if (input$group_heatmap == "Age") {
  #       dta_hm <-
  #         subset(dta,
  #                level == input$level_heatmap &
  #                  metric == "RT" &
  #                  measure == input$measure_heatmap &
  #                  sex == input$sex_heatmap &
  #                  region == input$region_heatmap &
  #                  year == input$year_heatmap &
  #                  !(age %in% c("ALL", "BSP", "ESP")))
  #       dta_hm$group <- dta_hm$age
  #       
  #     } else {
  #       dta_hm <-
  #         subset(dta,
  #                level == input$level_heatmap &
  #                  metric == "RT" &
  #                  measure == input$measure_heatmap &
  #                  age == input$age_heatmap &
  #                  sex == input$sex_heatmap &
  #                  region == input$region_heatmap)
  #       dta_hm$group <- as.character(dta_hm$year)
  #     }
  #     
  #     if (input$rank_heatmap) {
  #       dta_hm <- dta_hm[order(dta_hm$group), ]
  #       dta_hm$value <- 
  #         unlist(with(dta_hm, tapply(-value, group, dplyr::dense_rank)))
  #       
  #       dta_hm_top_causes <-
  #         tail(subset(dta_hm, group == input$sort_heatmap)[
  #           order(subset(dta_hm, group == input$sort_heatmap)$value,
  #                 decreasing = input$rank_heatmap),
  #           "cause"], 20)
  #       dta_hm_top <-
  #         subset(dta_hm, cause %in% pull(dta_hm_top_causes))
  #       dta_hm_top$cause <-
  #         factor(dta_hm_top$cause, pull(dta_hm_top_causes))
  #       
  #       if (nrow(dta_hm_top) > 0)
  #         ## relabel group
  #         if (input$group_heatmap == "Region") {
  #           dta_hm_top$group <- fct_region(dta_hm_top$group)
  #         } else if (input$group_heatmap == "Sex") {
  #           dta_hm_top$group <- fct_sex(dta_hm_top$group)
  #         } else if (input$group_heatmap == "Age") {
  #           dta_hm_top$group <- fct_age(dta_hm_top$group)
  #         } else {
  #           dta_hm_top$group <- dta_hm_top$group
  #         }
  #         
  #         ## create chart
  #         hchart(dta_hm_top,
  #                hcaes(x = group, y = cause, value = value),
  #                type = "heatmap",
  #                name = yTitle,
  #                showInLegend = FALSE) %>%
  #         hc_exporting(enabled = TRUE) %>%
  #         hc_add_theme(hc_theme_smpl()) %>%
  #         hc_yAxis(title = list(text = "")) %>%
  #         hc_xAxis(title = list(text = NULL)) %>%
  #         hc_plotOptions(
  #           series = list(dataLabels = list(
  #             enabled = TRUE,
  #             format = "{point.value:,.0f}"))) %>% 
  #         hc_colorAxis(
  #           dataClassColor = "category",
  #           dataClasses = list(
  #             list(from = 1, to = 1, color = "rgb(215, 48, 39)"),
  #             list(from = 2, to = 2, color = "rgb(252, 141, 89)"),
  #             list(from = 3, to = 4, color = "rgb(254, 224, 144)"),
  #             list(from = 5, to = 7, color = "rgb(224, 243, 248)"),
  #             list(from = 8, to = 13, color = "rgb(145, 191, 219)"),
  #             list(from = 14, to = 20, color = "rgb(69, 117, 180)"),
  #             list(from = 21, color = "black"))) %>%
  #         hc_legend(enabled = FALSE) %>%
  #         hc_tooltip(
  #           formatter = JS("function(){
  # return this.series.xAxis.categories[this.point.x] + '<br>' + this.series.name + '<br><b>' +  this.series.yAxis.categories[this.point.y] + ': #' +
  # Highcharts.numberFormat(this.point.value, 0) + '</b>';}"))
  #       
  #     } else {
  #       dta_hm_top_causes <-
  #         tail(subset(dta_hm, group == input$sort_heatmap)[
  #           order(subset(dta_hm, group == input$sort_heatmap)$value,
  #                 decreasing = input$rank_heatmap),
  #           "cause"], 20)
  #       dta_hm_top <-
  #         subset(dta_hm, cause %in% pull(dta_hm_top_causes))
  #       dta_hm_top$cause <-
  #         factor(dta_hm_top$cause, pull(dta_hm_top_causes))
  #       
  #       if (nrow(dta_hm_top) > 0)
  #         ## relabel group
  #         if (input$group_heatmap == "Region") {
  #           dta_hm_top$group <- fct_region(dta_hm_top$group)
  #         } else if (input$group_heatmap == "Sex") {
  #           dta_hm_top$group <- fct_sex(dta_hm_top$group)
  #         } else if (input$group_heatmap == "Age") {
  #           dta_hm_top$group <- fct_age(dta_hm_top$group)
  #         } else {
  #           dta_hm_top$group <- dta_hm_top$group
  #         }
  #       
  #         ## create chart
  #         hchart(dta_hm_top,
  #                hcaes(x = group, y = cause, value = value),
  #                type = "heatmap",
  #                name = yTitle,
  #                showInLegend = FALSE) %>%
  #         hc_exporting(enabled = TRUE) %>%
  #         hc_add_theme(hc_theme_smpl()) %>%
  #         hc_yAxis(title = list(text = "")) %>%
  #         hc_xAxis(title = NULL) %>% 
  #         hc_plotOptions(
  #           series = list(dataLabels = list(
  #             enabled = TRUE,
  #             format = "{point.value:,.2f}"))) %>% 
  #         hc_colorAxis(
  #           stops = color_stops(10, rev(RColorBrewer::brewer.pal(10, "Spectral"))),
  #           min = 0) %>%
  #         hc_tooltip(
  #           formatter = JS("function(){
  # return this.series.xAxis.categories[this.point.x] + '<br>' + this.series.name + '<br><b>' +  this.series.yAxis.categories[this.point.y] + ': ' +
  # Highcharts.numberFormat(this.point.value, 2) + '</b>';}"))
  #     }
  #   })
  
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   PATTERNS - BARPLOT #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  output$bars <-
    renderHighchart({
      if (input$metric_bars == "NR") {
        yTitle2 <- switch(input$measure_bars,
                          "DEATHS" = "number of deaths",
                          "CASES" = "number of prevalent cases",
                          "YLL" = "number of YLLs",
                          "YLD" = "number of YLDs",
                          "DALY" = "number of DALYs")
      } else {
        yTitle2 <- switch(input$measure_bars,
                          "DEATHS" = "death rate per 100,000 persons",
                          "CASES" = "prevalence per 100,000 persons",
                          "YLL" = "YLL rate per 100,000 persons",
                          "YLD" = "YLD rate per 100,000 persons",
                          "DALY" = "DALY rate per 100,000 persons")
      }
      
      mainTitle <-
        paste0("Contribution of diseases to the ",
               yTitle2,
               ", by ",
               tolower(input$group_bars),
               if (input$group_bars != "Age")
                 paste0(", ",
                        firstlower(agegroups_std[[input$age_bars]])),
               if (input$group_bars != "Sex")
                 paste0(", ",
                        firstlower(sexgroups_std[[input$sex_bars]])),
               if (!(input$group_bars %in% c("Location")))
                 paste0(", ",
                        regiongroups_std[[input$region_bars]]),
               if (input$group_bars != "Year")
                 paste0(", ",
                        input$year_bars))
      
      yTitle <-
        switch(input$measure_bars,
               "DEATHS" = "Death rate per 100,000 persons",
               "CASES" = "Prevalence per 100,000 persons",
               "YLL" = "YLL rate per 100,000 persons",
               "YLD" = "YLD rate per 100,000 persons",
               "DALY" = "DALY rate per 100,000 persons")
      
      if (input$group_bars != "Age")
        yTitle <- paste0(yTitle, ", ", input$age_bars)
      
      if (input$group_bars == "Location") {
        dta_bars <-
          subset(dta,
                 level == input$level_bars &
                   metric == input$metric_bars &
                   measure == input$measure_bars &
                   age == input$age_bars &
                   sex == input$sex_bars &
                   year == input$year_bars)
        dta_bars$group <- dta_bars$region
        dta_bars <- dta_bars[order(dta_bars$region), ]
        dta_bars$value <- round(dta_bars$value, 2)
        
      } else if (input$group_bars == "Sex") {
        dta_bars <-
          subset(dta,
                 level == input$level_bars &
                   metric == input$metric_bars &
                   measure == input$measure_bars &
                   age == input$age_bars &
                   region == input$region_bars &
                   year == input$year_bars)
        dta_bars$group <- dta_bars$sex
        dta_bars <- dta_bars[order(dta_bars$sex), ]
        dta_bars$value <- round(dta_bars$value, 2)
        
      } else if (input$group_bars == "Age") {
        dta_bars <-
          subset(dta,
                 level == input$level_bars &
                   metric == input$metric_bars &
                   measure == input$measure_bars &
                   sex == input$sex_bars &
                   region == input$region_bars &
                   year == input$year_bars &
                   !(age %in% c("ALL", "BSP", "ESP")))
        dta_bars$group <- dta_bars$age
        # dta_bars$group <- fct_age(dta_bars$group)
        dta_bars <- dta_bars[order(dta_bars$age), ]
        dta_bars$value <- round(dta_bars$value, 2)
        
      } else {
        dta_bars <-
          subset(dta,
                 level == input$level_bars &
                   metric == input$metric_bars &
                   measure == input$measure_bars &
                   age == input$age_bars &
                   sex == input$sex_bars &
                   region == input$region_bars)
        dta_bars$group <- as.character(dta_bars$year)
        dta_bars$value <- round(dta_bars$value, 2)
        
      }
      
      ## match colors
      # dta_bars <- left_join(dta_bars, col_all, by = c("cause" = "name"))
      
      if (input$level_bars == 3) {
        dta_bars <- dta_bars[order(match(dta_bars$cause,settings$highcharter$ord)),]
        dta_bars$cause <- factor(dta_bars$cause, levels = unique(dta_bars$cause), ordered = TRUE)
      } else if (input$level_bars == 2) {
        dta_bars <- dta_bars[order(match(dta_bars$cause,settings$highcharter$ord)),]
        dta_bars$cause <- factor(dta_bars$cause, levels = unique(dta_bars$cause), ordered = TRUE)
      }
      
      ## relabel group
      if (input$group_bars == "Location") {
        dta_bars$group <- fct_region(dta_bars$group)
      } else if (input$group_bars == "Sex") {
        dta_bars$group <- fct_sex(dta_bars$group)
      } else if (input$group_bars == "Age") {
        # dta_bars$group <- fct_age(dta_bars$group)
      } else {
        dta_bars$group <- dta_bars$group
      }
      
      ## validate input
      validate(need(!(nrow(dta_bars) == 0), validate_agestd))
      
      ## create highchart
      hchart(dta_bars,
             hcaes(x = group, y = value, group = cause),
             type = "column",
             stacking = input$stacked_bars) %>%
        hc_exporting(
          enabled = TRUE,
          buttons = list(
            contextButton = list(
              menuItems = list(
                'viewFullscreen', 
                'separator', 
                'downloadCSV', 
                'downloadXLS')
            ))
        ) %>% 
        hc_add_theme(hc_theme_smpl()) %>%
        hc_yAxis(
          title = "",
          stackLabels = list(
            enabled = input$stacked_bars == "normal",
            formatter = JS("function() {
              return Highcharts.numberFormat(this.total, 2);
            }"))) %>%
        hc_xAxis(
          title = NULL,
          categories = levels(dta_bars$group)) %>%
        hc_tooltip(
          pointFormat = "<span style='color:{series.color}'>\u25CF</span> {series.name}: <b>{point.y:,.2f}</b> ({point.percentage:.0f}%)<br/>") %>%
        hc_colors(
          c(unique(dta_bars$col))) %>%
        hc_plotOptions(
          series = list(
            groupPadding = 0,
            pointPadding = 0,
            format = "{point.value:,.2f}")) %>%
        hc_title(text = mainTitle)
    })
  
  
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   TRENDS - LINEPLOT #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$hc <-
    renderHighchart({
      yTitle <-
        ifelse(input$measure_trends == "DEATHS",
               "Deaths",
               ifelse(input$measure_trends == "YLL",
                      "Years of Life Lost",
                      ifelse(input$measure_trends == "YLD",
                             "Years Lived with Disability",
                             ifelse(input$measure_trends == "CASES",
                                    "Prevalent cases", "Disability-adjusted life years"))))
      
      mainTitle <-
        paste0(if (input$group_trends != "Cause")
          paste0(gsub(".\\|", "", gsub("ALL CAUSES", "All causes", input$cause_trends, )),
                 ", ",
                 tolower(yTitle)),
          if (input$group_trends == "Cause")
            yTitle,
          " by ",
          tolower(input$group_trends),
          ", ",
          if (input$group_trends == "Location")
            paste(firstlower(agegroups_std[[input$age_trends]]),
                  firstlower(sexgroups_std[[input$sex_trends]]),
                  sep = ", "),
          if (input$group_trends == "Province")
            paste(firstlower(agegroups_std[[input$age_trends]]),
                  firstlower(sexgroups_std[[input$sex_trends]]),
                  sep = ", "),
          if (input$group_trends == "Sex")
            paste(firstlower(agegroups_std[[input$age_trends]]),
                  regiongroups_std[[input$region_trends]],
                  sep = ", "),
          if (input$group_trends == "Age")
            paste(firstlower(sexgroups_std[[input$sex_trends]]),
                  regiongroups_std[[input$region_trends]],
                  sep = ", "),
          if (input$group_trends == "Cause")
            paste(firstlower(agegroups_std[[input$age_trends]]),
                  firstlower(sexgroups_std[[input$sex_trends]]),
                  regiongroups_std[[input$region_trends]],
                  sep = ", "),
          ", 2013-",
          max(dta$year))
      
      if (input$unit_trends == "NR") {
        if (input$group_trends == "Location") {
          dta_hc <-
            subset(dta,
                   level_cause == input$cause_trends &
                     measure == input$measure_trends &
                     age == input$age_trends &
                     sex == input$sex_trends &
                     metric == "NR")
          dta_hc$region <- fct_region(dta_hc$region)
          dta_hc$value <- round(dta_hc$value, 2)
          
          ## validate input
          validate(need(!(nrow(dta_hc) == 0), validate_agestd))
          
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = region),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_colors(c("black", "#0059ff", "#ffa500", "#ff0f21")) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_tooltip(valueDecimals = 2) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
          
          
        } else if (input$group_trends == "Sex") {
          dta_hc <-
            subset(dta,
                   level_cause == input$cause_trends &
                     measure == input$measure_trends &
                     age == input$age_trends &
                     region == input$region_trends &
                     metric == "NR")
          dta_hc$sex <- fct_sex(dta_hc$sex)
          dta_hc$value <- round(dta_hc$value, 2)
          
          ## validate input
          validate(need(!(nrow(dta_hc) == 0), validate_agestd))
          
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = sex),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_colors(c("black", "#d35400", "#2980b9")) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_yAxis(title = list(text = yTitle)) %>%
            hc_tooltip(valueDecimals = 2) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
          
        } else if (input$group_trends == "Age") {
          dta_hc <-
            subset(dta,
                   level_cause == input$cause_trends &
                     measure == input$measure_trends &
                     sex == input$sex_trends &
                     region == input$region_trends &
                     metric == "NR" &
                     !(age %in% c("BSP", "ESP")))
          # dta_hc$age <- fct_age(dta_hc$age)
          dta_hc$value <- round(dta_hc$value, 2)
          
          ## validate input
          validate(need(!(nrow(dta_hc) == 0), validate_agestd))
          
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = age),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%
            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_yAxis(title = list(text = yTitle)) %>%
            hc_tooltip(valueDecimals = 2) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
          
        } else if (input$group_trends == "Cause") {
          dta_hc <-
            subset(dta,
                   level_cause %in% input$cause_trends_multi &
                     measure == input$measure_trends &
                     age == input$age_trends &
                     sex == input$sex_trends &
                     region == input$region_trends &
                     metric == "NR")
          dta_hc$value <- round(dta_hc$value, 2)
          
          ## validate input
          validate(need(!(nrow(dta_hc) == 0), validate_agestd))
          
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = cause),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_yAxis(title = list(text = yTitle)) %>%
            hc_colors(
              c(unique(dta_hc$col))) %>%
            hc_tooltip(valueDecimals = 2) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
        }
        
      } else {
        yTitle <- paste(yTitle, "per 100,000 persons")
        
        if (input$group_trends == "Location") {
          dta_hc <-
            subset(dta,
                   level_cause == input$cause_trends &
                     measure == input$measure_trends &
                     age == input$age_trends &
                     sex == input$sex_trends &
                     metric == "RT")
          dta_hc$region <- fct_region(dta_hc$region)
          dta_hc$value <- round(dta_hc$value, 3)
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = region),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%
            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_colors(c("black", "#0059ff", "#ffa500", "#ff0f21")) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_yAxis(title = list(text = yTitle)) %>%
            hc_tooltip(valueDecimals = 3) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
          
        } else if (input$group_trends == "Sex") {
          dta_hc <-
            subset(dta,
                   level_cause == input$cause_trends &
                     measure == input$measure_trends &
                     age == input$age_trends &
                     region == input$region_trends &
                     metric == "RT")
          dta_hc$sex <- fct_sex(dta_hc$sex)
          dta_hc$value <- round(dta_hc$value, 3)
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = sex),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%
            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_colors(c("black", "#d35400", "#2980b9")) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_yAxis(title = list(text = yTitle)) %>%
            hc_tooltip(valueDecimals = 3) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
          
        } else if (input$group_trends == "Age") {
          dta_hc <-
            subset(dta,
                   level_cause == input$cause_trends &
                     measure == input$measure_trends &
                     sex == input$sex_trends &
                     region == input$region_trends &
                     metric == "RT" &
                     !(age %in% c("BSP", "ESP")))
          # dta_hc$age <- fct_age(dta_hc$age)
          dta_hc$value <- round(dta_hc$value, 3)
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = age),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_yAxis(title = list(text = yTitle)) %>%
            hc_tooltip(valueDecimals = 3) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
          
        } else if (input$group_trends == "Cause") {
          dta_hc <-
            subset(dta,
                   level_cause %in% input$cause_trends_multi &
                     measure == input$measure_trends &
                     age == input$age_trends &
                     sex == input$sex_trends &
                     region == input$region_trends &
                     metric == "RT")
          dta_hc$value <- round(dta_hc$value, 3)
          hc <- hchart(dta_hc,
                       hcaes(x = year, y = value, group = cause),
                       type = "line") %>%
            hc_exporting(
              enabled = TRUE,
              buttons = list(
                contextButton = list(
                  menuItems = list(
                    'viewFullscreen', 
                    'separator', 
                    'downloadCSV', 
                    'downloadXLS')
                ))
            ) %>%            hc_add_theme(hc_theme_smpl()) %>%
            hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
            hc_xAxis(title = NULL, tickInterval = 1) %>%
            hc_yAxis(title = list(text = yTitle)) %>%
            hc_tooltip(valueDecimals = 3) %>%
            hc_colors(
              c(unique(dta_hc$col))) %>%
            hc_title(text = mainTitle)
          
          if (input$scaleaxis_trends) {
            hc %>%
              hc_yAxis(title = list(text = yTitle), min = 0)
          } else {
            hc %>%
              hc_yAxis(title = list(text = yTitle))
          }
        }
      }
      
    })
  
  
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   CHATBOT #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # chat <-
  #   ellmer::chat_openai(
  #     system_prompt = "Respond to the user as succinctly as possible."
  #   )
  # 
  # observeEvent(input$chat_user_input, {
  #   stream <- chat$stream_async(input$chat_user_input)
  #   chat_append("chat", stream)
  # })
  
  # qc_vals <- qc$server()
  
  # output$dt_llm <- DT::renderDT({
  #   qc_vals$df()
  # })
  
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   DATA-TABLE #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dta_dt <-
    reactive({
      out <-
        subset(dta,
               level_cause %in% input$cause_dt &
                 year %in% input$year_dt &
                 measure %in% input$measure_dt &
                 metric %in% input$metric_dt &
                 age %in% input$age_dt &
                 sex %in% input$sex_dt &
                 region %in% input$region_dt)
      out$level_cause <- NULL
      out$parent <- NULL
      ## refactor
      # out$age <- fct_age(out$age)
      out$sex <- fct_sex(out$sex)
      out$region <- fct_region(out$region)
      out$metric <- factor(out$metric, c("NR", "RT"), c("Number", "Rate"))
      out$measure <- factor(out$measure, 
                            c("DALY", "YLD", "YLL", "CASES", "DEATHS"), 
                            c("Disability-Adjusted Life Years (DALYs)", 
                              "Years Lived with Disability (YLDs)",
                              "Years of Life Lost (YLLs)",
                              "Prevalent cases",
                              "Deaths"))
      
      ## to upper
      colnames(out) <- toupper(colnames(out))
      ## return output
      out <- select(out, -LOWER, -UPPER, -COL)
    })
  
  output$dt <-
    DT::renderDataTable({
      DT::datatable(
        dta_dt(),
        rownames = FALSE,
        extensions = "Buttons",
        options =
          list(
            lengthMenu =
              list(c(20, 50, 100, -1),
                   c(20, 50, 100, "All")),
            buttons =
              list(
                list(extend = "copy"),
                list(extend = "csv",
                     filename = paste0("BeBOD-DALY-export-", today())),
                list(extend = "excel",
                     filename = paste0("BeBOD-DALY-export-", today()))
              ),
            dom = "lfrtBip"
          )
      ) %>%
        formatRound(9, 3)
    })
  
  
  
  output$downloadData <-
    downloadHandler(
      filename = function() {
        paste0("BeBOD2020-DALY-", today(), ".csv")
      },
      content = function(con) {
        write.csv(dta, con, row.names = FALSE)
      }
    )
  
  output$downloadData3 <-
    downloadHandler(
      filename = function() {
        paste0("BeBOD2020-DALY-treemap-", today(), ".csv")
      },
      content = function(con) {
        write.csv(dta3, con, row.names = FALSE)
      }
    )
  
  ## selectize input
  cause_list <-
    unique(dta3[, c("level3", "level2", "level1")])
  cause_list <-
    cause_list[with(cause_list, order(level1, level2, level3)), ]
  cause_list <- data.frame(lapply(cause_list, as.character), stringsAsFactors = FALSE)
  
  lst <- "<span style='color:green;'><b>ALL CAUSES</b></span>"
  lab <- "0|ALL CAUSES"
  L1 <- unique(cause_list[, 3])[c(1, 3, 2)]
  
  for (i in seq_along(L1)) {
    lst <- c(lst, sprintf("&nbsp;&nbsp;<span style='color:green;'><b>%s</b></span>", L1[i]))
    lab <- c(lab, paste0("1|", L1[i]))
    L2 <- subset(cause_list, level1 == L1[i])[, 2]
    
    for (j in seq_along(L2)) {
      lst <- c(lst, sprintf("&nbsp;&nbsp;&nbsp;&nbsp;<b>%s</b>", L2[j]))
      lab <- c(lab, paste0("2|", L2[j]))
      
      L3 <- subset(cause_list, level2 == L2[j])[, 1]
      lst <- c(lst, paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style='color:grey;'>", L3, "</span>"))
      lab <- c(lab, paste0("3|", L3))
    }
  }
  
  cause_hierarchy <- as.list(lab)
  names(cause_hierarchy) <- lst
  
  observe({
    updateSelectizeInput(
      session,
      "cause_trends",
      choices = cause_hierarchy,
      selected = "0|ALL CAUSES",
      options =
        list(
          render = I('{item: function(item, escape) {
                         return "<div>" + item.label + "</div>"
                         },
                       option: function(item, escape) {
                         return "<div>" + item.label + "</div>"
                         }
                      }'
          )
        )
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "cause_trends_multi",
      choices = cause_hierarchy,
      selected = input$cause_trends,
      options =
        list(
          render = I('{item: function(item, escape) {
                         return "<div>" + item.label + "</div>"
                         },
                       option: function(item, escape) {
                         return "<div>" + item.label + "</div>"
                         }
                      }'
          )
        )
    )
  })
  
  observe({
    req(cause_hierarchy)
    updateSelectizeInput(
      session,
      "cause_dt",
      choices = cause_hierarchy,
      selected = "0|ALL CAUSES",
      options =
        list(
          render = I('{item: function(item, escape) {
                         return "<div>" + item.label + "</div>"
                         },
                       option: function(item, escape) {
                         return "<div>" + item.label + "</div>"
                         }
                      }'
          )
        )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
