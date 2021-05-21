library(shiny)
library(leaflet)
library(tidyverse)
library(mmbtools)

mmb_load_fonts()
options(scipen = 999)

# Set up UI layout
ui <- fluidPage(
  titlePanel("Borehole Water Points in Africa: 2016 - 2019"),
      selectInput(
        inputId = "type", 
        label = "Select Water Tech Type:", 
        choices = c("Hand Pump", "Hydram", "Mechanized Pump", "Rope and Bucket", "Tapstand"), 
        selected = "Hand Pump",
        multiple = FALSE
      ), # closes selectInput
    
  mainPanel(
  # Display plots on the same row
  fluidRow(
    leafletOutput("map", width = "1050px", height = "500px"),
    ) # closes fluidRow
  ) # closes mainPanel
) # closes fluidPage

# Set up server logic
server <- shinyServer(function(input, output) {
  # Create filter
  filter_data <- reactive({
   # filter data
   #ifelse(input$type == "All", borehole$water_tech %in% c("Hand Pump", "Mechanized Pump"), borehole$water_tech)
   filter(borehole, borehole$water_tech == input$type)
})
  # Generate interactive scatter plot
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 34.5085, lat = 8.7832, zoom = 3) %>%
      addTiles(tilesURL) %>%
      addCircles(data = filter_data(), lng = ~lon_deg, lat = ~lat_deg, color = ~pal(water_tech), opacity = 1)
  })
})


# DATA IMPORT
# read in data and keep only relevant columns
water <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv') %>%
    select(-c(row_id, status_id, installer, pay))
# filter out empty values
water <- water %>%
  filter (!is.na(water_source) & !is.na(water_tech) & !is.na(facility_type)) %>%
  mutate(water_source = case_when(water_source %in% c("Protected Shallow Well", "Protected Spring") ~ "Protected Spring/Well",
                                  water_source %in% c("Undefined Shallow Well", "Undefined Spring") ~ "Undefined Spring/Well",
                                  water_source %in% c("Unprotected Shallow Well", "Unprotected Spring") ~ "Unprotected Spring/Well",
                                  TRUE ~ water_source),
  water_tech = case_when(water_tech %in% c("Hand Pump - Afridev", "Hand Pump - Consallen", "Hand Pump - India Mark",
                                           "Hand Pump - Indus", "Hand Pump - Inkar", "Hand Pump - Kardia",
                                           "Hand Pump - Nira", "Hand Pump - PB Mark II", "Hand Pump - SWN 80",
                                           "Hand Pump - Vergnet") ~ "Hand Pump",
                         water_tech %in% c("Mechanized Pump - Diesel", "Mechanized Pump - Solar") ~ "Mechanized Pump",
                         TRUE ~ water_tech)
  )
# create a vector of countries not in Africa
countries <- c("Peru", "Timor-Leste")
# filter out those countries
water <- water %>%
  filter(!(country_name %in% countries) & !is.na(install_year) & lat_deg < 56) %>%
  select(-status)
borehole <- water %>%
  filter(water_source == "Borehole" & !is.na(country_name) & install_year %in% c(2016, 2017, 2018, 2019)) %>%
  group_by(country_name, water_tech) %>%
  mutate(count = n()) %>%
  ungroup()
tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
# define color scheme for water_tech variable
pal <- colorFactor(palette = c("#003685", "#78BE21", "#FFC845", "#5D295F", "#008EAA"),
                   domain = borehole$water_tech)



shinyApp(ui = ui, server = server)