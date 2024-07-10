# Europe COVID-19 Statistics Dashboard
# Author: Nick Brinkhof
# Date: 2024-07-10
# Description: This Shiny app displays the latest COVID-19 statistics for European countries, including total cases, total deaths, and total recoveries. The data is sourced from Worldometer.


# Load required packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(rvest)
library(dplyr)
library(DT)

# Scrape COVID-19 data
url <- "https://www.worldometers.info/coronavirus/"
webpage <- read_html(url)
data <- webpage %>%
  html_node(xpath = '//*[@id="main_table_countries_today"]') %>%
  html_table()

# List of European countries
european_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium",
                        "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
                        "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", 
                        "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", 
                        "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
                        "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
                        "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", 
                        "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City")

# Filter data for European countries
data <- data %>%
  filter(`Country,Other` %in% european_countries) %>%
  select(Country = `Country,Other`, TotalCases = `TotalCases`, TotalDeaths = `TotalDeaths`, TotalRecovered = `TotalRecovered`)

# Convert the scraped data to numeric
data <- data %>%
  mutate(
    TotalCases = as.numeric(gsub(",", "", TotalCases)),
    TotalDeaths = as.numeric(gsub(",", "", TotalDeaths)),
    TotalRecovered = as.numeric(gsub(",", "", TotalRecovered))
  )

# Define coordinates for European countries (for simplicity, i use approximate center points of each country)
coordinates <- data.frame(
  Country = european_countries,
  Latitude = c(41.1533, 42.5063, 40.0691, 47.5162, 40.1431, 53.7098, 50.8503, 43.9159, 42.7339, 45.1, 35.1264, 49.8175,
               56.2639, 58.5953, 61.9241, 46.6034, 42.3154, 51.1657, 39.0742, 47.1625, 64.9631, 53.1424, 41.8719, 48.0196, 
               42.6026, 56.8796, 47.166, 55.1694, 49.8153, 35.9375, 47.4116, 43.7333, 42.7087, 52.1326, 41.6086, 60.472, 
               51.9194, 39.3999, 45.9432, 61.524, 43.9424, 44.0165, 48.669, 46.1512, 40.4637, 60.1282, 46.8182, 38.9637, 
               48.3794, 55.3781, 41.9029),
  Longitude = c(20.1683, 1.5218, 45.0382, 14.5501, 47.5769, 27.9534, 4.3517, 17.6791, 25.4858, 15.2, 33.4299, 15.4729, 
                9.5018, 25.0136, 25.7482, 1.8883, 43.3569, 10.4515, 21.8243, 19.5033, -19.0208, -7.6921, 12.5674, 66.9237, 
                20.902, 24.6032, 9.5554, 23.8813, 6.1296, 14.3754, 28.3699, 7.4189, 19.3744, 5.2913, 21.7453, 8.4689, 
                19.1451, -8.2245, 24.9668, 105.3188, 12.4578, 20.4573, 19.699, 14.9955, -3.7492, 18.6435, 8.2275, 35.2433, 
                31.1656, -3.436, 12.4534)
)

# Merge coordinates into the data
data <- merge(data, coordinates, by = "Country")

# Define UI
header <- dashboardHeader(title = "Europe COVID-19 Dashboard", titleWidth = 340)

sidebar <- dashboardSidebar(
  width = 340,
  sliderInput('casesRange', 'Select Total Cases:', 
              min = min(data$TotalCases, na.rm = TRUE), 
              max = max(data$TotalCases, na.rm = TRUE), 
              value = c(min(data$TotalCases, na.rm = TRUE), max(data$TotalCases, na.rm = TRUE))),
  selectInput('countrySelect', 'Select Country:', choices = c("All", european_countries), selected = "All")
)

body <- dashboardBody(
  leafletOutput('map', width = '100%', height = 'calc(100vh - 80px)'),
  absolutePanel(top = 10, left = 20, id = 'controls', class = 'panel panel-default', fixed = TRUE, draggable = TRUE, height = 'auto',
                tags$style(type = "text/css", "#controls {background-color: rgba(255,255,255,0.9); padding: 20px;}")),
  DT::dataTableOutput('dataTable')
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session) {
  filteredData <- reactive({
    data <- data %>%
      filter(TotalCases >= input$casesRange[1], TotalCases <= input$casesRange[2])
    if (input$countrySelect != "All") {
      data <- data %>%
        filter(Country == input$countrySelect)
    }
    data
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 10, lat = 50, zoom = 4) %>%
      addTiles() %>%
      addCircleMarkers(data = filteredData(),
                       ~Longitude, ~Latitude,
                       weight = 1, radius = ~log(TotalCases + 1) * 2,
                       popup = ~paste(Country, "<br>Total Cases: ", TotalCases, "<br>Total Deaths: ", TotalDeaths, "<br>Total Recovered: ", TotalRecovered))
  })
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
}

# Run the application 
shinyApp(ui, server)




