# reading libraries
library(shiny)
library(readr)
library(tidyverse)
library(dplyr)
library(plotly)
library(here)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(leaflet)
library(shinyHeatmaply)
library(RColorBrewer)

# main dataset
ACT_Road_Crash_Data <- read.csv("Data/ACT_Road_Crash_Data.csv")

# datasets for canberra map

year <- separate( ACT_Road_Crash_Data, "CRASH_DATE", c("Year", "Month", "Day"), sep = "/")%>%
  rename (year = "Day", day = "Year")%>%
  select(year, LATITUDE, LONGITUDE, SUBURB_LOCATION, LIGHTING_CONDITION, WEATHER_CONDITION, ROAD_CONDITION, CRASH_TIME, CRASH_SEVERITY)

# datasets for heat map 

year <- separate( ACT_Road_Crash_Data, "CRASH_DATE", c("Year", "Month", "Day"), sep = "/")%>%
  rename (year = "Day", day = "Year")
frequency <- separate(ACT_Road_Crash_Data, "CRASH_TIME", c("Hour", "Minute", "Sec"), sep = ":")%>%
  mutate(Hour= as.numeric(Hour))

hello <- merge(year,frequency, by = "CRASH_ID")%>%
  group_by(Hour, year)%>%
  count(Hour)%>%
  rename(`Number of road accidents` = "n")

# ui interface

ui <- fluidPage(style = "background-color:#F3FCEF;",
                title = "Road Accidents in Canberra",
                tabsetPanel(tabPanel("about", icon = icon("question"),
                                        fluidRow(
                                          column(12, h1('FIT5147: Data Exploration and Visualisation',  align = "center", style = "font-size: 30px;"),
                                                 h2('Data Visualization Project',  align = "center", style = "font-size: 30px;"),
                                                 h3("Ratul Wadhwa (32055587)", align = "center", style = "font-size: 18px;"),
                                                 h6(textOutput('text_out')),
                                                 tags$head(tags$style("#text_out{color: #006400;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
                                                 uiOutput("about")))),
                    
                    tabPanel("chart",
                             icon = icon("line-chart"),
                             align = "center", style = "background-color:#F3FCEF;", plotlyOutput("chart", width = "90%", height = "700"),
                             sliderInput("Hour",
                                         "Hour",
                                         min = 00,
                                         max = 23,
                                         value = c(00, 23),
                                         sep = "",
                                         width = "100%"),
                             fluidRow("The above chart shows the overall count of road accidents through out the years (2012-2021). 
                                      We can analyze that the count of road accidents around 8 AM and 5 PM is higher than rest of the hours in a day and the faded color from 2012 to 2021 indicates that the number of accidents decreased over the years. 
                                      The slider bar for hour can be adjusted to see the trend over the years for a specific hour/hours. 
                                      The tooltip shows the exact count of accidents in a particular year and hour of the day.",
                                      style = "font-size:20px; text-align:justify; color:#006400;")),
                    
                    tabPanel("map",
                             icon = icon("map"),
                             sidebarPanel(
                               selectizeInput("SUBURB_LOCATION", "Select Suburbs",
                                              choices = unique(year$SUBURB_LOCATION),
                                              multiple = TRUE),
                               selectizeInput("ROAD_CONDITION", "Select Road Conditions",
                                              choices = unique(year$ROAD_CONDITION),
                                              multiple = TRUE),
                               selectizeInput("WEATHER_CONDITION", "Select Weather Conditions",
                                              choices = unique(year$WEATHER_CONDITION),
                                              multiple = TRUE),
                               selectizeInput("LIGHTING_CONDITION", "Select Lighting Conditions",
                                              choices = unique(year$LIGHTING_CONDITION),
                                              multiple = TRUE)
                             
                             ),
                             mainPanel(
                               leafletOutput("map",height = "700"),
                               sliderInput("year",
                                           "Year",
                                           min = 2012,
                                           max = 2021,
                                           value = c(2012, 2021),
                                           sep = "",
                                           width = "100%")),
                             fluidRow("The above maps shows the spatial locations of the road accidents through out the years (2012-2021). 
                                      The slider can be used to adjust the years, to see the trend and locations of accidents in a particular year/years.
                                      The side bar prodives the details of road, weather and lighting conditions. The suburbs drop down filters the accidents in that particular suburb.
                                      It was observed maximum number of accidents occurred in City around 3202 followed by Belconnen and Gungahi at 2756 and 2723 respectively.",
                                      style = "font-size:20px; text-align:justify; color:#006400;"))
                    )
)

server <- function(input, output, session) {
  
  output$text_out <- renderText({
    paste("The Australian Government takes road safety quite seriously. Due to the severe road security rules and regulations, the death rate or injured in a road crash  has decreased in Australia.
          Hence, the above facts motivated me to find out more and validate if crashes have decreased over the years. Also, as an enthusiastic motorist, I need to be informed about the road accident statistics 
          and the trends that develop and evolve. Therefore, the interactive analysis on the road crash frequency shows the overall frequency of road accidents over the years in Canberaa and through out the day.
          The map on the map tab shows the accidents prone areas and the factors in which they occured.", input$text_input)
  })
    
  
  output$map <- renderLeaflet({   
    
    color <- colorFactor(
      palette = brewer.pal(10,"Spectral"),
      domain = year$year
    )
      
      if (!is.null(input$SUBURB_LOCATION)){
        year <- year %>% 
          filter(SUBURB_LOCATION %in% input$SUBURB_LOCATION)
      }
  
    if (!is.null(input$ROAD_CONDITION)){
      year <- year %>% 
        filter(ROAD_CONDITION %in% input$ROAD_CONDITION)
    }
    
    if (!is.null(input$WEATHER_CONDITION)){
      year <- year %>% 
        filter(WEATHER_CONDITION %in% input$WEATHER_CONDITION)
    }
    
    if (!is.null(input$LIGHTING_CONDITION)){
      year <- year %>% 
        filter(LIGHTING_CONDITION %in% input$LIGHTING_CONDITION)
    }
    
      year <- year %>% 
        filter(year >= input$year[1] & year <= input$year[2])
      
      m <- leaflet(year) %>%
         addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addCircles(lng= year$LONGITUDE, lat= year$LATITUDE, color= color(year$year),
                     popup = paste("Suburb:", year$SUBURB_LOCATION, "<br>",
                                   "Year:", year$year, "<br>",
                                   "CRASH_TIME:", year$CRASH_TIME, "<br>",
                                   "CRASH_SEVERITY:", year$CRASH_SEVERITY, "<br>"),
                     
                     label  = paste("Suburb:", year$SUBURB_LOCATION, 
                                    ", Year:", year$year,
                                    ", Road Condition:", year$ROAD_CONDITION, 
                                    ", Weather Condition:", year$WEATHER_CONDITION, 
                                    ", Lighting Condition:", year$LIGHTING_CONDITION)) %>%
        addLegend(pal = color , values = ~year , opacity = 1)
    
    })

  
    # Define outputs here heatmap
    output$chart <- renderPlotly({

     hello <- hello %>%
        filter(Hour >= input$Hour[1] & Hour <= input$Hour[2])

      ggplot(hello, aes(y = year , x= Hour, fill= `Number of road accidents`)) +
      geom_tile()+
      scale_fill_gradient(low="light blue", high="red")+
      theme(axis.text.x = element_text(angle=40, hjust=1))+
      theme_bw()+
        theme(legend.position = "none")+
        ggtitle("Overall Frequency of Road Accidents") +
        theme(plot.title = element_text(hjust = 0.5))+
           xlab("Hour") + ylab("Year")
        
      
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
