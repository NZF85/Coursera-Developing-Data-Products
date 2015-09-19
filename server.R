library(shiny)

# Load data processing file
source("data.R")
Country <- sort(unique(data$Country))

# Shiny server
shinyServer(
  function(input, output) {
    output$setid <- renderText({input$setid})
    
    output$address <- renderText({
      input$goButtonAdd
      isolate(paste("http://data.un.org/Data.aspx?d=UNODC&f=tableCode%3a1", 
                    input$setid, sep=""))
      
    })
    
    #     getPage<-function(url) {
    #         return(tags$iframe(src = url, 
    #                            style="width:100%;",  
    #                            frameborder="0", id="iframe", 
    #                            height = "500px"))
    #     }
    
    openPage <- function(url) {
      return(tags$a(href=url, "Click here!", target="_blank"))
    }
    
    output$inc <- renderUI({ 
      input$goButtonDirect
      isolate(openPage(paste("http://data.un.org/Data.aspx?d=UNODC&f=tableCode%3a1", 
                             input$setid, sep=""))) 
    })
    
    
    # Initialize reactive values
    values <- reactiveValues()
    values$Country <- Country
    
    # Create event type checkbox
    output$CountryControl <- renderUI({
      checkboxGroupInput('Country', 'Country:', 
                         Country, selected = values$Country)
    })
    
    # Add observer on select-all button
    observe({
      if(input$selectAll == 0) return()
      values$Country <- Country
    })
    
    # Add observer on clear-all button
    observe({
      if(input$clearAll == 0) return()
      values$Country <- c() # empty list
    })
    
    # Prepare dataset
    dataTable <- reactive({
      groupByCountry(data, input$timeline[1], 
                   input$timeline[2], input$Rate[1],
                   input$Rate[2], input$Country)
    })
    
    dataTableByYear <- reactive({
      groupByYearAgg(data, input$timeline[1], 
                     input$timeline[2], input$Rate[1],
                     input$Rate[2], input$Country)
    })
    
    dataTableByRate <- reactive({
      groupByYearRate(data, input$timeline[1], 
                       input$timeline[2], input$Rate[1],
                       input$Rate[2], input$Country)
    })
    
    dataTableByRateAvg <- reactive({
      groupByRateAvg(data, input$timeline[1], 
                      input$timeline[2], input$Rate[1],
                      input$Rate[2], input$Country)
    })
    
    dataTableByRateCountryAvg <- reactive({
      groupByRateCountryAvg(data, input$timeline[1], 
                           input$timeline[2], input$Rate[1],
                           input$Rate[2], input$Country)
    })
    
    # Render data table
    output$dTable <- renderDataTable({
      dataTable()
    } #, options = list(bFilter = FALSE, iDisplayLength = 50)
    )
    
    output$CountryByYear <- renderChart({
      plotCountryCountByYear(dataTableByYear())
    })
    
    output$RateByYear <- renderChart({
      plotRateByYear(dataTableByRate())
    })
    
    output$RateByYearAvg <- renderChart({
      plotRateByYearAvg(dataTableByRateAvg())
    })
    
    output$RateByCountryAvg <- renderChart({
      plotRateByCountryAvg(dataTableByRateCountryAvg())
    })
    
  } # end of function(input, output)
)