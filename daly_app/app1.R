# qsdfqd ??

library(shiny)
library(rintrojs)
library(bslib)

## additional info
zenodo_link <- "10.5281/zenodo.15574409"
dta_time <- "2022" ## year of update

ui <- navbarPage(
  introjsUI(),
  #### POSITION ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  position = c("fixed-top"),
  
  #### HEADER ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  title =
    HTML(
      "<a href='https://www.sciensano.be'><img src='sciensano.png' height='20px'></a>&nbsp;&nbsp;&nbsp; <span style='color:#69aa41; font-size:1.1em;'>BeBOD &rsaquo; Disability-Adjusted Life Years<span>"
    ),
  windowTitle = "BeBOD > Disability-Adjusted Life Years",
  
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##+                   INFO #####
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabsetPanel(id = "test",
  tabPanel(
    "Info",
    icon = icon("info-circle"),
    
    introBox(
      actionButton("Help", "Press for instruction")
        ),
    
    introBox(
      tags$div(
        HTML("<h1 style='color:#69aa41;'>Belgian National Burden of Disease Study (BeBOD)</h1>"),
        HTML("<p>What are the most important diseases in Belgium? Which risk factors contribute most to the overall disease burden? How is the burden of disease evolving over time, and how does it differ across the country? In a context of increasing budgetary constraints, a precise answer to these basic questions is more than ever necessary to inform policy-making.</p>"),
        HTML("<p>To address this need, <b>Sciensano</b> is conducting a <b>national burden of disease study</b>. In addition to generating internally consistent estimates of death rate (mortality) or how unhealthy we are (morbidity) by age, sex and region, the burden of disease will also be quantified using Disability-Adjusted Life Years (DALYs). The use of DALYs allows to estimate the years of life lost from premature death and years of life lived with disabilities. It therefore permits a truly comparative ranking of the burden of various diseases, injuries and risk factors.</p>"),
        HTML("<p>For more info, please visit the <a href='https://www.sciensano.be/en/projects/belgian-national-burden-disease-study' target='_blank'>BeBOD project page</a> or read the <a href='https://www.sciensano.be/en/biblio/belgian-national-burden-disease-study-guidelines-calculation-disability-adjusted-life-years-belgium-0' target='_blank'>BeBOD protocol</a>.</p>"),
      ),
      data.step = 1,
      data.intro = "This is an introduction."
    ),  

    introBox(
      tags$div(
        HTML("<h4><i class='fa-solid fa-chart-simple'></i>&nbsp; Other BeBOD visualisation tools</h4>"),
        HTML("<p><a href='https://burden.sciensano.be/shiny/mortality' target='_blank'>Estimates of the fatal burden of 131 causes of death</a></p>"),
        HTML("<p><a href='https://burden.sciensano.be/shiny/cancer' target='_blank'>Estimates of the non-fatal burden of 57 cancer sites</a></p>"),
        HTML("<p><a href='https://burden.sciensano.be/shiny/risk' target='_blank'>Estimates of the risk factor attributable burden</a></p>"),
        HTML("<p><a href='https://burden.sciensano.be/shiny/projections' target='_blank'>Estimates of projected non-fatal burden of 33 causes</a></p>"),
      ),
      data.step = 2,
      data.intro = "This is a summary of other BeDOB visualisation tools"
    ),
    
   introBox(
     tags$div(
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
       HTML("<p>The number of deaths by cause of death are based on the official causes of death database compiled by <a href='https://statbel.fgov.be/en/themes/population/mortality-life-expectancy-and-causes-death/causes-death' target='_blank'>Statbel</a> We first map the ICD-10 codes of the underlying causes of death to the Global Burden of Disease cause list, consisting of 131 unique causes of deaths. Next, we perform a probabilistic redistribution of ill-defined deaths to specific causes, to obtain a specific cause of death for each deceased person (see <a href='https://archpublichealth.biomedcentral.com/articles/10.1186/s13690-023-01163-7' target='_blank'>Devleesschauwer et al. (2023)</a>).</p>")
     ),
     data.step = 3,
     data.intro = "Here you can find information on the different estimates of burden of disease."
   ),
   
    
   introBox(
     tags$div(
       HTML("<h2 style='color:#69aa41;'>Explore our estimates</h2>"),
       
       actionLink("link_to_Treemap", HTML("<h4><i class='fa fa-th-large'></i>&nbsp; Treemap</h4>")),
       p("Explore the main causes of death, years of life lost, number of cases/prevalence, years lived with disability, and disability-adjusted life years by age, sex, region and year."),
       
       HTML("<h4><i class='fa fa-chart-line'></i>&nbsp; Trends</h4>"),
       p("Explore trends in causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years, by age, sex and region."),
       
       HTML("<h4><i class='fa fa-sort-amount-up'></i>&nbsp; Rankings</h4>"),
       p("Explore ranks in causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years by age, sex, region and year."),
       
       HTML("<h4><i class='fa fa-chart-bar'></i>&nbsp; Patterns</h4>"),
       p("Compare estimates of causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years by age, sex, region and year."),
       
       HTML("<h4><i class='fa fa-database'></i>&nbsp; Results</h4>"),
       p("Explore and download our estimates of causes of death, years of life lost, cases, years lived with disabilities, and disability-adjusted life years.")
     ),
     data.step = 4,
     data.intro = "Explore the estimates in different formats."
     ),

   introBox(
     tags$div(
       HTML("<h2 style='color:#69aa41;'>Download all estimates</h2>"),
       HTML("<p>All estimates can be downloaded in <a href='https://arrow.apache.org/docs/r/reference/read_parquet.html' target='_blank'>Parquet format</a> via Zenodo:</p>"),
       HTML(sprintf("<p><a href='https://doi.org/%s' target='_blank'><img src='https://zenodo.org/badge/DOI/%s.svg' alt='DOI'></a></p>", zenodo_link, zenodo_link)),
     ),
     data.step = 5,
     data.intro = "Here you can download the estimates."
   ),
    
    HTML("<h2 style='color:#69aa41;'>Citation</h2>"),
    HTML(sprintf("<p class='alert alert-warning'>Robby De Pauw, Vanessa Gorasso, Aline Scohy, Sarah Nayani, Rani Claerman, Laura Van den Borre, Jozefien Wellekens & Brecht Devleesschauwer. (2025). BeBOD estimates of mortality, years of life lost, prevalence, years lived with disability, and disability-adjusted life years for 38 causes, 2013-2022 (v2025-06-26) [Data set]. Zenodo. https://doi.org/%s</p>", zenodo_link)),
    HTML("<p><a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a><br />This work is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/' target='_blank'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.</p>")#,
    

  ),
  
  tabPanel(
    "Treemap",
    icon = icon("th-large")
  )),
  
  
  #### FOOTER ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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


server <- function(input, output, session) {
  observeEvent(input$Help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back",
                                               "skipLabel"="Close"))
               )
  
  observeEvent(input$link_to_Treemap, {
    updateTabsetPanel(session, "test", "Treemap")
  })

}




shinyApp(ui, server)