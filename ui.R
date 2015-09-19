# The user-interface definition of the Shiny web app.
library(shiny)
library(BH)
library(rCharts)
require(markdown)
library(dplyr)
library(DT)

shinyUI(
  navbarPage("Intentional homicide, number and rate per 100,000 population", 
             # multi-page user-interface that includes a navigation bar.
             tabPanel("Explore the Data",
                      sidebarPanel(
                        sliderInput("timeline", 
                                    "Timeline:", 
                                    min = 1995,
                                    max = 2015,
                                    value = c(2000, 2015)),
                        sliderInput("Rate", 
                                    "Number of Rate:",
                                    min = 0,
                                    max = 95,
                                    value = c(30, 90) 
                        ),
                        #format = "####"),
                        uiOutput("CountryControl"), # the id
                        actionButton(inputId = "clearAll", 
                                     label = "Clear selection", 
                                     icon = icon("square-o")),
                        actionButton(inputId = "selectAll", 
                                     label = "Select all", 
                                     icon = icon("check-square-o"))
                        
                      ),
                      mainPanel(
                        tabsetPanel(
                          # Data 
                          tabPanel(p(icon("table"), "Dataset"),
                                   dataTableOutput(outputId="dTable")
                          ), # end of "Dataset" tab panel
                          tabPanel(p(icon("line-chart"), "Visualize the Data"), 
                                   h4('Number of Countries by Year', align = "center"),
                                   showOutput("CountryByYear", "nvd3"),
                                   h4('Rate by Year', align = "center"),
                                   h5('Please hover over each point to see the Country and Source.', 
                                      align ="center"),
                                   showOutput("RateByYear", "nvd3"),
                                   h4('Average Rate by Year', align = "center"),
                                   showOutput("RateByYearAvg", "nvd3"),
                                   h4('Average Rate by Country', align = "center"),
                                   showOutput("RateByCountryAvg", "nvd3")
                          ) # end of "Visualize the Data" tab panel
                          
                        )
                        
                      )     
             ), # end of "Explore Dataset" tab panel
             
             
             
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             ) # end of "About" tab panel
  )
  
)