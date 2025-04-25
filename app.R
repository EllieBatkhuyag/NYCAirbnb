library(ggplot2)
library(htmltools)
library(sf)
library(leaflet)
library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)
library(rsconnect)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(leaflet.extras)
library(bslib)
library(utils)

#df <- read.csv("nyc_airbnb_clean.csv")
#crime_df <- read.csv("nyc_crime_clean.csv")

load("final.RData")
df <- nyc_airbnb_clean
crime_df <- nyc_crime_clean

ui <- navbarPage(
  title = tags$div(
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/6/69/Airbnb_Logo_B%C3%A9lo.svg",
      alt = "Airbnb Logo",
      class = "navbar-brand",
      style = "display: block; margin: 0 auto; width: 200px; height: 80px;"
    )
  ),
  
  windowTitle = "NYC Airbnb Explorer", 
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      body{
        background-image: url('https://upload.wikimedia.org/wikipedia/commons/3/3e/Lower_Manhattan_from_Brooklyn_May_2015_panorama.jpg');
        background-size: cover;
        background-attachment: fixed;
        background-position: center;
      } .well {
        background-color: #2c3e50; 
        padding: 15px;
      } .control-label {
        color: #fe595d; 
      } .checkbox label {
        color: #fe595d; 
      }")
               )
  ),
  
  tabPanel(
    titlePanel("Overview"),
    
      tags$div(
        style = "background-color: rgba(44,61,80,255); color: #ffffff; padding: 20px; border-radius: 10px; margin: 20px auto; max-width: 800px;",
        HTML("<h2 style='color: #fe595d;'>NYC Airbnb Explorer</h2>"),
        strong("We are here to ease your trip to The City That Never Sleeps!"),
        p("New York City is a metropolian city that attracts millions of tourists each year. 
        From its world-class landmarks to its vibrant neighborhoods and diverse cultures, NYC 
        offers a unique experience for everyone. Airbnb, Inc is an American company that 
        operates an online marketplace for lodging, primarily homestays for vacation rentals, 
        and tourism activities."),
        br(),
        strong("About this web"),
        p("This interactive application is designed to help you explore the Airbnb listings 
        across New York City 2024, providing insights into accommodations that suit your needs. 
        Whether you’re looking for a cozy townhouse in Brooklyn, eye-catching stores in Soho or budget-friendly 
        option in Queens, this tool allows you to visualize and compare Airbnb to make planning easier."),
        br(),
        strong("Safety first!"),
        p("The city's dedicated police force is always on alert, providing a strong sense of security. 
        However, it's still important to stay cautious and be prepared. Researching your destination 
        and familiarizing yourself with potential risks will help ensure a safe and enjoyable trip.
        The crime rate in New York City is relatively low, and most incidents occur outside of the 
        popular tourist areas in Manhattan. Overall, like any major city, there are a few things 
        tourists should be mindful of to stay safe. In this search tool, we provide data on crimes 
        that took place in the year of 2024."),
        br(),
        p("At last, to further assist you to make a great memory, we provide you some fun activities
        that might catch your interest."),
        p("Enjoy exploring the concrete jungle where dreams are made of.")
    ),
    
    mainPanel(
      tabsetPanel(tabPanel("Activities",
                           p(style = "color: white; font-size: 15px", 
                             "1. Immersive Gamebox Lower East Side - ",
                             tags$a(href="https://www.immersivegamebox.com/en-GB/venues/lny-essex-crossing-lower-east-side-new-york",
                                    "Link")
                             ),
                           p(style = "color: white; font-size: 15px", 
                             "2. Comedy Cellar - ",
                             tags$a(href="https://www.comedycellar.com/",
                                    "Link")
                           ),
                           p(style = "color: white; font-size: 15px", 
                             "3. The Rink at Rockefeller Center - ",
                             tags$a(href="https://www.rockefellercenter.com/contact/rink-private-events/",
                                    "Link")
                           ),
                           p(style = "color: white; font-size: 15px", 
                             "4. Broadway Show - ",
                             tags$a(href="https://www.broadway.com/?gad_source=1&gclid=Cj0KCQiAu8W6BhC-ARIsACEQoDBrQcjD2Xlv4WN0qxX5B5dLFgYlOw6owFb5W9NtIUFR_te_NCMEQjsaAq3wEALw_wcB",
                                    "Link")
                           ),
                           p(style = "color: white; font-size: 15px", 
                             "5. RiseNY - ",
                             tags$a(href="https://www.riseny.co/?gad_source=1&gclid=Cj0KCQiAu8W6BhC-ARIsACEQoDBWa31LZqT_QOT10vuNURixg7WmkJuRaWTT3_4SHcmj3BilygcSd1oaAveqEALw_wcB",
                                    "Link")
                           ),
                           p(style = "color: white; font-size: 15px", 
                             "6. Museum Of Ice Cream NY - ",
                             tags$a(href="https://www.museumoficecream.com/new-york-city/",
                                    "Link")
                           )
                           ),
                  tabPanel("References",
                           p(style = "color: white; font-size: 13px", 
                             "1. Airbnb Open Data. https://www.kaggle.com/datasets/arianazmoudeh/airbnbopendata. 
                             Accessed 5 Dec. 2024."),
                           p(style = "color: white; font-size: 13px",
                             "2. NYC Crime | NYC Open Data. https://data.cityofnewyork.us/Public-Safety/NYC-crime/qb7u-rbmr/about_data. 
                             Accessed 5 Dec. 2024."),
                           p(style = "color: white; font-size: 13px", 
                             "3. New York City Tourism: How Safe Is NYC for Tourists? | The New York Pass®. https://newyorkpass.com/en/things-to-do/how-safe-is-nyc-for-tourists. 
                             Accessed 5 Dec. 2024."),
                           p(style = "color: white; font-size: 13px", 
                             "4. The 10 best fun activities & games in New York City (updated 2024). (n.d.).
                             https://www.tripadvisor.com/Attractions-g60763-Activities-c56-New_York_City_New_York.html ")
                           )
                  )
    )
    
  ), #tabpanel 1, intro 
  
  tabPanel(
    titlePanel("Airbnb Search"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "city",
          label = "Select City:",
          choices = unique(df$city),
          selected = unique(df$city)[1]),
        
        selectInput(
          inputId = "accommodation_type",
          label = "Select Accommodation Type:",
          choices = unique(df$property_type),
          selected = unique(df$property_type)[1]),
      
        selectInput(
          inputId = "room_type",
          label = "Select Room type:",
          choices = unique(df$room_type),
          selected = unique(df$room_type)[1]),
      
        sliderInput(
          inputId = "bedrooms",
          label = "Number of Bedrooms:",
          min = 1,
          max = 21,
          value = 1,
          step = 1),
      
        sliderInput(
          inputId = "bathrooms",
          label = "Number of Bathrooms:",
          min = 1.0,
          max = 6.5,
          value = 1.0,
          step = 0.5),
      ),
      
      mainPanel(
        leafletOutput(outputId = "map", height = "750px")
      )#main panel
      
    )#sidebar map
    
  ), #tabpanel 2, map
  
  tabPanel(
    titlePanel("Price Distribution"),

    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "price_dist",
          label = "Select Feature for Price distribution:",
          choices = c("Neighbourhood", 
                      "Number of Reviews",
                      "City"),
          selected = "Neighbourhood"),
        checkboxInput(
          inputId = "city_meanPrice",
          label = "Show Mean Price by City?",
          value = FALSE)
        ),
      
      mainPanel(
        plotlyOutput(outputId = "boxplot", height = "400px"),
        plotlyOutput(outputId = "cityPrice", height = "350px")
      )
    )
      
  ), #tabpanel 3, boxplot
  
  tabPanel(
    titlePanel("NYC Crime Data"),
    sidebarLayout(
      sidebarPanel(
        dateInput(
          inputId = "crime_map",
          label = "Choose a date:",
          value = "2024-01-01",
          min = "2024-01-01",
          max = "2024-12-31",
        )
      ),
      
      mainPanel(
        leafletOutput(outputId = "crimeMap", height = "400px"),
        plotOutput(outputId = "crimeBar", height = "350px") 
      )
    )
    
  ) #tabpanel 4, plot
  
) #ui

server <- function(input, output, session) {
  
  filtered_df <- reactive({
    df %>%
      filter(
        city == input$city,
        property_type == input$accommodation_type,
        room_type == input$room_type,
        bedrooms == input$bedrooms, 
        bathrooms == input$bathrooms
      ) %>%
      drop_na()
  })
  
  output$map <- renderLeaflet({
    data <- filtered_df()
    
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Two") %>%
      setView(lng = -74.0060, lat = 40.7128, zoom = 11) %>%
      addMiniMap(width = 150, height = 150) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,  
        color = "#ff5a5f", 
        stroke = FALSE,  
        fillOpacity = 0.9,  
        label = ~paste0(
          "Number of reviews: ", number_of_reviews,
          "<br> Accomodates: ", accommodates, " people",
          "<br> Cancel policy: ", cancellation_policy),
        popup = ~paste0(
          "Number of reviews: ", number_of_reviews,
          "\nAccomodates: ", accommodates, "people",
          "\nCancel policy: ", cancellation_policy)
      )    
  })
  
  output$boxplot <- renderPlotly({
    data <- filtered_df()
    
    if (input$price_dist == "Neighbourhood") {
      p <- ggplot(data, aes(x = neighbourhood, y = price)) +
        geom_boxplot() +
        labs(title = "Price Distribution by Neighborhood of prior selections", x = "Neighborhood", y = "Price ($)") +
        coord_flip() +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 6))
    } else if (input$price_dist == "Number of Reviews") {
      p <- ggplot(data, aes(x = as.factor(number_of_reviews), y = price)) +
        geom_boxplot() +
        labs(title = "Price Distribution by Number of Reviews of prior selections", x = "Number of Reviews", y = "Price ($)") +
        coord_flip() +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 6))
    } else if (input$price_dist == "City") {
      p <- ggplot(df, aes(x = as.factor(city), y = price)) +
        geom_boxplot() +
        labs(title = "Price Distribution by City of prior selections", x = " ", y = "Price ($)") +
        coord_flip() +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 6))
    }
    ggplotly(p)
  })
  
  output$cityPrice <- renderPlotly({
    
    if(input$city_meanPrice) {
      
      mean_df <- df %>%
        group_by(city) %>%
        summarise(mean_price = mean(price, na.rm = TRUE)) %>%
        arrange(desc(mean_price)) %>%
        drop_na()
      
      mean_df$city <- factor(mean_df$city, levels = mean_df$city)
      
      a <- ggplot(mean_df) +
        geom_bar(aes(x = city, y = mean_price), stat = "identity") +
        labs(title = "Mean Price by City", x = "City", y = "Price ($)") +
        coord_flip() +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 5))
      
      ggplotly(a)
      
    } else {
      return(NULL)
    }
    
  })
  
  output$crimeMap <- renderLeaflet({
    
    crime <- crime_df %>%
        filter(CMPLNT_FR_DT == as.Date(input$crime_map)) %>% 
        drop_na()
    
    if (nrow(crime) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -74.0060, lat = 40.7128, zoom = 10)
    } else {
      leaflet(crime) %>%
        addTiles() %>%
        setView(lng = -74.0060, lat = 40.7128, zoom = 10) %>%
        addCircleMarkers(lng = ~Longitude,
                         lat = ~Latitude,
                         color = ~case_when(
                           LAW_CAT_CD == "FELONY" ~ "orange",
                           LAW_CAT_CD == "MISDEMEANOR" ~ "red4",
                           LAW_CAT_CD == "VIOLATION" ~ "darkgreen",
                           TRUE ~ "gray"),
                         popup = ~paste0("<strong>Offense:</strong> ", OFNS_DESC),
                         label = ~BORO_NM,
                         radius = 3,
                         fillOpacity = 0.8)
      }
  })
  
  output$crimeBar <- renderPlot({
    
    crime <- crime_df %>%
      filter(CMPLNT_FR_DT == as.Date(input$crime_map)) %>%
      drop_na()
    
    if (nrow(crime) == 0) {
      return(NULL)
    } else {
      crime_count <- crime %>%
        group_by(LAW_CAT_CD, OFNS_DESC) %>%
        summarise(n = n()) %>%
        drop_na() %>%
        ungroup()
      
      
      ggplot(crime_count) +
        geom_bar(aes(x = OFNS_DESC, y = n, fill = LAW_CAT_CD), stat = "identity") +
        coord_flip() +
        facet_wrap(~LAW_CAT_CD, scales = "free_y") +
        scale_fill_manual(values = c("FELONY" = "orange", 
                                     "MISDEMEANOR" = "red4", 
                                     "VIOLATION" = "darkgreen")) +
        labs(x = "", y = "") +
        theme_minimal() +
        theme(strip.background = element_rect(fill = "gray"),
              legend.position = "none",
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank())
    }
  })
}

shinyApp(ui = ui, server = server)