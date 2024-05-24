library(shiny)
library(shinydashboard)
library(sf)
library(leaflet)
library(leafem)
library(DT)
library(htmltools)
library(RColorBrewer)
library(dplyr)

##### UI #####
ui <- dashboardPage(
  dashboardHeader(title = "Quality of life - Germany", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarid",
      menuItem(
        "Indicator map",
        tabName = "indicator_map",
        icon = icon("map")
      ),
      menuItem(
        "Score map",
        tabName = "score_map",
        icon = icon("map")
      ),
      menuItem(
        "Summary table",
        tabName = "summary_table",
        icon = icon("table")
      ),
      conditionalPanel(
        'input.sidebarid == "indicator_map"',
        selectInput("dropdown_indicator", "Select indicator:", choices = "Population")
      ),
      conditionalPanel(
        'input.sidebarid == "score_map"',
        sliderInput("population_weight", "Select population weight:", min = 1, max = 5, value = 3),
        sliderInput("area_weight", "Select area weight:", min = 1, max = 5, value = 3),
        sliderInput("density_weight", "Select density weight:", min = 1, max = 5, value = 3)
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "indicator_map",
        h2("Indicator map"),
        fluidRow(
          leafletOutput("indicator_map")
        )
      ),
      tabItem(
        tabName = "score_map",
        h2("Score map"),
        fluidRow(
          column(9, leafletOutput("score_map")),
          column(3, DT::DTOutput("top_10_table"))
        )
      ),
      tabItem(
        tabName = "summary_table",
        h2("Summary table"),
        fluidRow(
          DT::DTOutput("summary_table")
        ),
        fluidRow(
          downloadButton("download_summary_table", "Download summary table")
        )
      )
    )
  )
)



##### SERVER #####
server <- function(input, output) {
  
  # set parameters for score calculation
  n_tile <- 5
  n_indicators <- 3
  n_points_per_indicator <- 5
  max_points_per_indicator <- 5
  max_total_points <- n_indicators * n_points_per_indicator * max_points_per_indicator
  
  # load and transform indicators
  indicators_sf <- read_sf("indicators.shp")
  indicators_sf <- st_transform(indicators_sf, 4326)
  
  # get choices for indicator dropdown
  choices <- names(indicators_sf)[4:(length(names(indicators_sf))-1)]
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "dropdown_indicator",
    choices = choices,
    selected = choices[1]
  )
  
  
  
  ### indicator map
  
  rctv_indicator_map <- reactive({
    
    selected_indicator <- input$dropdown_indicator
    
    if (selected_indicator == "Population") {
      
      bins <- c(0, 100000, 250000, 500000, 1000000, 4000000)
      
    } else if (selected_indicator == "Area") {
      
      bins <- c(0, 500, 1000, 2000, 4000, 6000)
      
    } else if (selected_indicator == "Density") {
      
      bins <- c(0, 100, 250, 500, 1000, 5000)
      
    }
    
    palette <- colorBin(
      palette = "YlOrBr",
      domain = indicators_sf[[selected_indicator]],
      na.color = "transparent",
      bins = bins
    )
    
    tooltip <- paste(
      "District: ", indicators_sf$District, "<br/>",
      "Type: ", indicators_sf$Type, "<br/>",
      "Population: ", indicators_sf$Population, "<br/>",
      "Area: ", indicators_sf$Area, "<br/>",
      "Density: ", indicators_sf$Density,
      sep = "") %>% 
      lapply(htmltools::HTML)
    
    leaflet(indicators_sf) %>% 
      addTiles() %>% 
      addScaleBar(position = "topleft") %>% 
      addProviderTiles("Esri.WorldTopoMap",
                       group = "TopoMap") %>% 
      addProviderTiles("Esri.WorldImagery",
                       group = "Aerial") %>% 
      addLayersControl(position = "topleft",
                       baseGroups = c("StreetMap", "Aerial"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      leafem::addMouseCoordinates() %>% 
      addPolygons(fillColor = ~palette(get(selected_indicator)),
                  stroke = FALSE,
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  label = tooltip,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
                  ) %>% 
      addLegend(pal = palette,
                values = ~get(selected_indicator),
                opacity = 0.9,
                title = selected_indicator,
                position = "topright")
    
  })
  
  output$indicator_map <- renderLeaflet({ rctv_indicator_map() })
  
  
  
  ### score calculation
  rctv_indicators_sf <- reactive({
    
    population_weight <- input$population_weight
    area_weight <- input$area_weight
    density_weight <- input$density_weight
    
    indicators_sf <- indicators_sf %>% 
      mutate(population_quantile = ntile(Population, n_tile),
             area_quantile = ntile(Area, n_tile),
             density_quantile = ntile(Density, n_tile)) %>% 
      mutate(Points = population_quantile * population_weight
             + area_quantile * area_weight
             + density_quantile * density_weight) %>% 
      mutate(Score = round(Points / max_total_points, 2)) %>% 
      arrange(desc(Score))
    
  })
  
  
  
  ### score map
  
  rctv_score_map <- reactive({
    
    bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    
    palette <- colorBin(
      palette = "YlOrBr",
      domain = rctv_indicators_sf()$Score,
      na.color = "transparent",
      bins = bins
    )
    
    tooltip <- paste(
      "District: ", rctv_indicators_sf()$District, "<br/>",
      "Type: ", rctv_indicators_sf()$Type, "<br/>",
      "Population: ", rctv_indicators_sf()$Population, "<br/>",
      "Area: ", rctv_indicators_sf()$Area, "<br/>",
      "Density: ", rctv_indicators_sf()$Density, "<br/>",
      "Score: ", rctv_indicators_sf()$Score,
      sep = "") %>% 
      lapply(htmltools::HTML)
    
    leaflet(rctv_indicators_sf()) %>% 
      addTiles() %>% 
      addScaleBar(position = "topleft") %>% 
      addProviderTiles("Esri.WorldTopoMap",
                       group = "TopoMap") %>% 
      addProviderTiles("Esri.WorldImagery",
                       group = "Aerial") %>% 
      addLayersControl(position = "topleft",
                       baseGroups = c("StreetMap", "Aerial"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      leafem::addMouseCoordinates() %>% 
      addPolygons(fillColor = ~palette(Score),
                  stroke = FALSE,
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  label = tooltip,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>% 
      addLegend(pal = palette,
                values = ~Score,
                opacity = 0.9,
                title = "Score",
                position = "topright")
    
  })
  
  output$score_map <- renderLeaflet({ rctv_score_map() })
  
  
  
  ### top 10 table
  
  rctv_top_10_table <- reactive({
    
    top_10_table <- rctv_indicators_sf() %>% 
      select(District,
             Score) %>% 
      head(10) %>% 
      st_drop_geometry()
    
  })
  
  output$top_10_table <- DT::renderDT({
    
    DT::datatable(rctv_top_10_table(), selection = "single", options = list(stateSave = TRUE))
    
  })
  
  
  
  ### highlight selected row
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  observeEvent(input$top_10_table_rows_selected, {
    
    # get selected row
    row_selected <- rctv_indicators_sf()[input$top_10_table_rows_selected, ]
    
    tooltip <- paste(
      "District: ", row_selected$District, "<br/>",
      "Type: ", row_selected$Type, "<br/>",
      "Population: ", row_selected$Population, "<br/>",
      "Area: ", row_selected$Area, "<br/>",
      "Density: ", row_selected$Density, "<br/>",
      "Score: ", row_selected$Score,
      sep = "") %>% 
      lapply(htmltools::HTML)
    
    proxy <- leafletProxy("score_map")
    
    # remove previously selected row
    if (!is.null(prev_row())) {
      
      proxy %>% 
        removeShape("row_selected")
      
    }
    
    # set new value to reactiveVal
    prev_row(row_selected)
    
    # highlight selected row
    proxy %>% 
      addPolygons(data = row_selected,
                  fillColor = "green",
                  stroke = FALSE,
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  label = tooltip,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto"),
                  layerId = "row_selected"
      )
    
  })
  
  
  
  ### summary table
  
  rctv_summary_table <- reactive({
    
    summary_table <- rctv_indicators_sf() %>% 
      select(-population_quantile,
             -area_quantile,
             -density_quantile) %>% 
      st_drop_geometry()
    
  })
  
  output$summary_table <- DT::renderDT({ rctv_summary_table() })
  
  output$download_summary_table <- downloadHandler(
    
    filename = function(){paste0("summary_table.csv")},
    content = function(fname){write.csv(rctv_summary_table(), file = fname, row.names = FALSE)}
    
  )
  
}

shinyApp(ui, server)