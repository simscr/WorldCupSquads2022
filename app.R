library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)

Players_df <- read_rds("Players_df.rds")

#####

iconWidth <- 25
iconHeight <- 25
shadowWidth <- 25
shadowHeight <- 25


flagIcon_cs <- leaflet::iconList(
  "Argentina" = makeIcon(
    iconUrl = "Country_flags/Argentina.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Australia" = makeIcon(
    iconUrl = "Country_flags/Australia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Belgium" = makeIcon(
    iconUrl = "Country_flags/Belgium.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Brazil" = makeIcon(
    iconUrl = "Country_flags/Brazil.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Cameroon" = makeIcon(
    iconUrl = "Country_flags/Cameroon.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Canada" = makeIcon(
    iconUrl = "Country_flags/Canada.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Costa Rica" = makeIcon(
    iconUrl = "Country_flags/Costa_Rica.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Croatia" = makeIcon(
    iconUrl = "Country_flags/Croatia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Denmark" = makeIcon(
    iconUrl = "Country_flags/Denmark.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Ecuador" = makeIcon(
    iconUrl = "Country_flags/Ecuador.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "England" = makeIcon(
    iconUrl = "Country_flags/England.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "France" = makeIcon(
    iconUrl = "Country_flags/France.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Germany" = makeIcon(
    iconUrl = "Country_flags/Germany.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Ghana" = makeIcon(
    iconUrl = "Country_flags/Ghana.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Iran" = makeIcon(
    iconUrl = "Country_flags/Iran.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Japan" = makeIcon(
    iconUrl = "Country_flags/Japan.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Mexico" = makeIcon(
    iconUrl = "Country_flags/Mexico.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Morocco" = makeIcon(
    iconUrl = "Country_flags/Morocco.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Netherlands" = makeIcon(
    iconUrl = "Country_flags/Netherlands.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Poland" = makeIcon(
    iconUrl = "Country_flags/Poland.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Portugal" = makeIcon(
    iconUrl = "Country_flags/Portugal.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Qatar" = makeIcon(
    iconUrl = "Country_flags/Qatar.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Saudi Arabia" = makeIcon(
    iconUrl = "Country_flags/Saudi_Arabia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Senegal" = makeIcon(
    iconUrl = "Country_flags/Senegal.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Serbia" = makeIcon(
    iconUrl = "Country_flags/Serbia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "South Korea" = makeIcon(
    iconUrl = "Country_flags/South_Korea.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Spain" = makeIcon(
    iconUrl = "Country_flags/Spain.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Switzerland" = makeIcon(
    iconUrl = "Country_flags/Switzerland.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Tunisia" = makeIcon(
    iconUrl = "Country_flags/Tunisia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "United States" = makeIcon(
    iconUrl = "Country_flags/United_States.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Uruguay" = makeIcon(
    iconUrl = "Country_flags/Uruguay.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Wales" = makeIcon(
    iconUrl = "Country_flags/Wales.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  )
)
#####


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10,
                pickerInput("countries", label = "Select a Country:",
                            choices = list("All countries", 
                                           `Group A` = sort(c("Netherlands", "Senegal", "Ecuador", "Qatar")),
                                           `Group B` = sort(c("England", "United States", "Iran", "Wales")),
                                           `Group C` = sort(c("Argentina", "Poland", "Mexico", "Saudi Arabia")),
                                           `Group D` = sort(c("France", "Australia", "Tunisia", "Denmark")),
                                           `Group E` = sort(c("Japan", "Spain", "Germany", "Costa Rica")),
                                           `Group F` = sort(c("Morocco", "Croatia", "Belgium", "Canada")),
                                           `Group G` = sort(c("Brazil", "Switzerland", "Cameroon", "Serbia")),
                                           `Group H` = sort(c("Portugal", "South Korea", "Uruguay", "Ghana"))),
                            options = list(

                              `live-search` = TRUE)
                )
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    if (input$countries == "All countries") {
      Players_df
    } else {
      filter(Players_df, Countries == input$countries)
    }
  })
  
  filteredIcon <- reactive({
    if (input$countries == "All countries") {
      flagIcon_cs
    } else {
      flagIcon_cs$iconUrl <- rep(paste0("Country_flags/", str_replace_all(input$countries, " ", "_"), ".png"), 23)
    }
    flagIcon_cs
  })
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(~lon, ~lat, 
                 icon = ~ flagIcon_cs[Countries], 
                 label = ~Player, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup_text)
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(~lon, ~lat, 
                 icon = ~ flagIcon_cs[Countries], 
                 label = ~Player, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup_text)
  })
}

shinyApp(ui, server)
