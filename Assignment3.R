install.packages("rvest")
install.packages("tidyverse")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("leaflet")

library(rvest)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)

####################
##### SCRAPING #####
####################

# Scraping date, highest temp, lowest temp and rain probability on a daily base
# The parameter "url" is coming from the server
get_weather_data <- function(url) {
  
  webpage <- read_html(url)
  
  date <- webpage %>%
    html_nodes(".swg-row-wrapper.bg--blue-gradient") %>%
    html_nodes(".swg-col-period.swg-row") %>%
    html_text(trim = TRUE)
  
  temp_high <- webpage %>%
    html_nodes(".swg-row-wrapper.bg--blue-gradient") %>%
    html_nodes(".swg-col-temperature.swg-row") %>%
    html_nodes(".swg-text-large") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("°", "") %>%
    str_replace_all("\\s", "")
  
  temp_low <- webpage %>%
    html_nodes(".swg-row-wrapper.bg--blue-gradient") %>%
    html_nodes(".swg-col-temperature.swg-row") %>%
    html_nodes(".swg-text-small") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("/", "") %>%
    str_replace_all("°", "") %>%
    str_replace_all("\\s", "")
  
  rain_proba <- webpage %>%
    html_nodes(".swg-row-wrapper.bg--blue-gradient") %>%
    html_nodes(".swg-col-wv1.swg-row") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("%", "") %>%
    str_replace_all("\\s", "")
  
  return(data.frame(Date = date, temp_high = temp_high, temp_low = temp_low, rain_proba = rain_proba))
}

###########################
##### SHINY DASHBOARD #####
###########################

# Definition of UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather Dashboard"),
  dashboardSidebar(
    selectInput("city", "Choose a city:", 
                choices = c("Bern", "Rome", "Paris")), # City input
    checkboxInput("temp_high", "Show highest temperature", TRUE), # Highest temp checkbox
    checkboxInput("temp_low", "Show lowest temperature", TRUE), # Lowest temp checkbox
    checkboxInput("rain", "Show probability of rain", FALSE) # Rain probability checkbox
  ),
  dashboardBody(
    box(plotOutput("weatherPlot")),
    box(leafletOutput("map"))
  )
)

# Definition of server
server <- function(input, output) {
  
  # Reactive expressions which loads the data of the city everytime the city is selected in the input
  weather_data <- reactive({
    
    # URLs for each city weather data
    urls <- c(
      Bern = "https://ch.wetter.com/wetter_aktuell/wettervorhersage/7_tagesvorhersage/schweiz/bern/CH0CH0324.html",
      Rome = "https://ch.wetter.com/wetter_aktuell/wettervorhersage/7_tagesvorhersage/italien/rom/IT0LA0153.html",
      Paris = "https://ch.wetter.com/wetter_aktuell/wettervorhersage/7_tagesvorhersage/frankreich/paris/FR0IF0356.html"
    )
    
    # Call the function on line 19 to scrape data
    data <- get_weather_data(urls[input$city])
    
    # Columns to numeric
    data$temp_high <- as.numeric(data$temp_high)
    data$temp_low <- as.numeric(data$temp_low)
    data$rain_proba <- as.numeric(data$rain_proba)
    
    return(data)
  })
  
  ### Reactive plot ###
  output$weatherPlot <- renderPlot({
    
    # Get the data for the selected city
    weather_data <- weather_data()
    
    # Empty plot
    plot(1, type="n", xlim=c(1,7), ylim=c(0, max(max(weather_data$temp_high), max(weather_data$temp_low), max(weather_data$rain_proba))),
         xlab="Day", ylab="", xaxt="n")
    axis(side=1, at=1:7, labels=weather_data$Date)
    
    # Plotting data
    # If highest temperature is selected
    if (input$temp_high) {
      lines(weather_data$temp_high, type = 'o', col = 'red')
      points(weather_data$temp_high, type = 'o', col = 'red')
    }
    
    # If lowest temperature is selected
    if (input$temp_low) {
      lines(weather_data$temp_low, type = 'o', col = 'blue')
      points(weather_data$temp_low, type = 'o', col = 'blue')
    }
    
    # If rain probability is selected
    if (input$rain) {
      lines(weather_data$rain_proba, type = 'o', col = 'green')
      points(weather_data$rain_proba, type = 'o', col = 'green')
    }
    
    legend("topright", legend = c("Temp High", "Temp Low", "Rain Probability"), 
           col = c("red", "blue", "green"), pch = 1)
  })
  
  ### Leaflet map ###
  output$map <- renderLeaflet({
    
    # Coordinates for each city
    locations <- list(
      Bern = c(46.9480, 7.4474),
      Rome = c(41.9028, 12.4964),
      Paris = c(48.8566, 2.3522)
    )
    
    # Get the data for the selected city
    data_for_city <- weather_data()
    
    # Popup in the map when clicked on city symbol for todays weather
    weather_info <- paste(
      input$city,
      "Today:",
      paste("Temp:", data_for_city$temp_high[1], "°/", data_for_city$temp_low[1], "° C"),
      paste("Rain:", data_for_city$rain_proba[1], "%"),
      sep = "<br/>"
    )
    
    # Map with selected city in the center
    leaflet() %>%
      setView(lng = locations[[input$city]][2], lat = locations[[input$city]][1], zoom = 10) %>%
      addTiles() %>%
      addMarkers(lng = locations[[input$city]][2], lat = locations[[input$city]][1], popup = weather_info) # weather_info: todays weather
  })
}

# Run the dashboard
shinyApp(ui, server)
