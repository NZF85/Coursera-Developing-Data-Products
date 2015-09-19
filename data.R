# Load required libraries
library(data.table)
library(dplyr)
library(DT)
library(rCharts)

# Read data
data <- fread("UNdata_Export_20150919_104759402.csv")
head(data)
setnames(data, "Country or Area", "Country")
setnames(data, "Source Type", "SourceType")
data$Rate <- as.numeric(data$Rate)
data$Count <- as.numeric(data$Count)
# Exploratory data analysis
sum(is.na(data)) # 0

table(data$Year) # 1995 - 2015
length(table(data$Year)) # 17
Year <- sort(unique(data$Year))

length(table(data$Country)) # 206
Country <- sort(unique(data$Country))


table(data$Source)
length(table(data$Source)) # 28
Source <- sort(unique(data$Source))

table(data$SourceType)
length(table(data$SourceType)) # 2
SourceType <- sort(unique(data$SourceType))


## Helper functions

#' Aggregate dataset by Year
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minRate
#' @param maxRate
#' @param Country
#' @return data.table
#'
groupByYearRate <- function(dt, minYear, maxYear, minRate,
                             maxRate, Country) {
  result <- dt %>% filter(Year >= minYear, Year <= maxYear,
                          Rate >= minRate, Rate <= maxRate,
                          Country %in% Country) 
  return(result)
}

#' Aggregate dataset by Country
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minRate
#' @param maxRate
#' @param Country
#' @return result data.table
#' 
groupByCountry <- function(dt, minYear, maxYear, 
                         minRate, maxRate, Country) {
  # use pipelining
 
  dt <- groupByYearRate(dt, minYear, maxYear, minRate,
                         maxRate, Country) 
  # print(dim(result))
  result <- datatable(dt, options = list(iDisplayLength = 50))
  return(result)
}

#' Aggregate dataset by Year to get total count of Country
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minRate
#' @param maxRate
#' @param Country
#' @return data.table 2 columns
#'
groupByYearAgg <- function(dt, minYear, maxYear, minRate,
                           maxRate, Country) {
  dt <- groupByYearRate(dt, minYear, maxYear, minRate,
                         maxRate, Country)
  result <- dt %>% 
    group_by(Year)  %>% 
    summarise(count = n()) %>%
    arrange(Year)
  return(result)
}

#' Aggregate dataset by Year to get total count of average
#' number of Rate
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minRate
#' @param maxRate
#' @param Country
#' @return data.table 2 columns
#'
groupByRateAvg <- function(dt,  minYear, maxYear, minRate,
                            maxRate, Country) {
  dt <- groupByYearRate(dt, minYear, maxYear, minRate,
                         maxRate, Country)
  result <- dt %>% 
    group_by(Year) %>% 
    
    summarise(avg = mean(Rate)) %>%
    arrange(Year)
  return(result)      
}

#' Average Rate for each Country
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minRate
#' @param maxRate
#' @param Country
#' @return data.table 2 columns
#'
groupByRateCountryAvg <- function(dt,  minYear, maxYear, minRate,
                                 maxRate, Country) {
  dt <- groupByYearRate(dt, minYear, maxYear, minRate,
                         maxRate, Country)
  result <- dt %>% 
    group_by(Country) %>%
    summarise(avgRate = mean(Rate)) %>%
    arrange(Country)
  return(result)
}

#' Plot number of Country by Year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel Year
#' @param yAxisLabel number of Country
#' @return CountryByYear plot
plotCountryCountByYear <- function(dt, dom = "CountryByYear", 
                                  xAxisLabel = "Year",
                                  yAxisLabel = "Number of Country") {
  CountryByYear <- nPlot(
    count ~ Year,
    data = dt,
    type = "multiBarChart",
    dom = dom, width = 650
  )
  CountryByYear$chart(margin = list(left = 100))
  CountryByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  CountryByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  CountryByYear
}

#' Plot number of Rate by Year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel Year
#' @param yAxisLabel Rate
#' @return plotRateByYear plot
plotRateByYear <- function(dt, dom = "RateByYear", 
                             xAxisLabel = "Year", 
                             yAxisLabel = "Rate") {
  RateByYear <- nPlot(
    Rate ~ Year,
    data = dt,
    type = "scatterChart",
    dom = dom, width = 650
  )
  RateByYear$chart(margin = list(left = 100), 
                     showDistX = TRUE,
                     showDistY = TRUE)
  RateByYear$chart(color = c('green', 'orange', 'blue'))
  RateByYear$chart(tooltipContent = "#! function(key, x, y, e){ 
                     return '<h5><b>Country</b>: ' + e.point.Country + '<br>'
                     + '<b>Source</b>: ' + e.point.Source  
                     + '</h5>'
                     
} !#") # data[data$Rate==y&data$Year==x, ]$name
  RateByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  RateByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  #     RateByYear$chart(useInteractiveGuideline = TRUE)
  RateByYear
  }

#' Plot number of average Rate by Year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel Year
#' @param yAxisLabel Average Rate
#' @return CountryByYear plot
plotRateByYearAvg <- function(dt, dom = "RateByYearAvg", 
                                xAxisLabel = "Year",
                                yAxisLabel = "Average Rate") {
  
  RateByYearAvg <- nPlot(
    avg ~ Year,
    data = dt,
    type = "multiBarChart",
    group = 'Country',
    stacked = TRUE,
    dom = dom, width = 650
  )
  RateByYearAvg$chart(margin = list(left = 100))
  RateByYearAvg$chart(color = c('orange', 'blue', 'green'))
  RateByYearAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  RateByYearAvg$xAxis(axisLabel = xAxisLabel, width = 70)
  RateByYearAvg
  
}

#' Plot number of average Rate by Country
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel Country
#' @param yAxisLabel Average Rate
#' @return RateByCountryAvg plot
plotRateByCountryAvg <- function(dt, dom = "RateByCountryAvg", 
                                 xAxisLabel = "Country", 
                                 yAxisLabel = "Average Rate") {
  RateByCountryAvg <- nPlot(
    avgRate ~ Country,
    data = dt,
    type = "multiBarChart",
    dom = dom, width = 650
  )
  RateByCountryAvg$chart(margin = list(left = 100))
  RateByCountryAvg$chart(color = c('pink', 'blue', 'green'))
  RateByCountryAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  RateByCountryAvg$xAxis(axisLabel = xAxisLabel, width = 200,
                         rotateLabels = -20, height = 200)
  RateByCountryAvg
  
}