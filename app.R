library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(RSocrata)

# leitura dos dados
years_ago <- today() - years(1)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

# selecionando algumas variáveis
db_crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "injuries", "none"),
    crash_date,
    weather_condition,
    lighting_condition,
    roadway_surface_cond,
    most_severe_injury,
    prim_contributory_cause,
    crash_day_of_week,
    crash_hour,
    latitude,
    longitude
  ) %>%
  na.omit()

# adicionando nova coluna com os dias da semana
db_crash <- db_crash %>%
  mutate(day_week = case_when(
    crash_day_of_week == 1 ~ 'Sunday',
    crash_day_of_week == 2 ~ 'Monday',
    crash_day_of_week == 3 ~ 'Tuesday',
    crash_day_of_week == 4 ~ 'Wednesday',
    crash_day_of_week == 5 ~ 'Thursday',
    crash_day_of_week == 6 ~ 'Friday',
    crash_day_of_week == 7 ~ 'Saturday'
  ))

# Filtrando só os casos que tiveram lesões
db_injuries <- db_crash %>%
  filter(injuries == "injuries" )

# Convertendo palavras para letras minúsculas
db_injuries <- db_injuries %>% 
  mutate_if(is.character, tolower)


#ui

ui <- fluidPage(
  titlePanel(p("Traffic Crashes in the City of Chicago", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      
      # seleção tempo
      selectizeInput('weather_condition','Weather Condition', choices = sort(unique(db_injuries$weather_condition)), width = 380,
                     selected = 'blowing snow' ,multiple = T),
      
      #seleção dia da semana
      selectizeInput('day_week','Days of Week', width = 380,
                     choices = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                     selected = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                     multiple = T),
      
      #seleção hora
      sliderInput("slider", "Time of Day",
                  min=0, max=23, width = 380, value = range(db_injuries$crash_hour))
    ),
    mainPanel(
      leafletOutput(outputId = "mymap"),
      
    )
  )
)


#server
server <- function(input, output, session){
  
  output$mymap <- renderLeaflet({
    
    leaflet(db_injuries %>%
              dplyr::filter(
                weather_condition %in% input$weather_condition &
                  #day_week %in% input$day_week &
                  crash_hour >= input$slider[1] & crash_hour <= input$slider[2]
              )) %>%
      addTiles(group = 'OSM') %>%
      addMarkers(lat= ~latitude,
                 lng= ~longitude,
                 #popup = ~content,
                 clusterOptions = markerClusterOptions())
    
  })
}

shinyApp(ui, server)