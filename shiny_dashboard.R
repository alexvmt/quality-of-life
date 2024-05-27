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
        sliderInput("density_weight", "Select density (people/km²) weight:", min = 1, max = 5, value = 3),
        sliderInput("income_weight", "Select income per capita (€) weight:", min = 1, max = 5, value = 3),
        sliderInput("age_weight", "Select average age weight:", min = 1, max = 5, value = 3)
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
    
    palette <- colorQuantile(
      palette = "YlOrBr",
      domain = indicators_sf[[selected_indicator]],
      n = 5,
      na.color = "transparent"
    )
    
    colors <- brewer.pal(n = 5, name = "YlOrBr")
    
    labels = c(
      paste0(0, " - ", round(quantile(indicators_sf[[selected_indicator]], 0.2), 0)),
      paste0(round(quantile(indicators_sf[[selected_indicator]], 0.2), 0) + 1, " - ", round(quantile(indicators_sf[[selected_indicator]], 0.4), 0)),
      paste0(round(quantile(indicators_sf[[selected_indicator]], 0.4), 0) + 1, " - ", round(quantile(indicators_sf[[selected_indicator]], 0.6), 0)),
      paste0(round(quantile(indicators_sf[[selected_indicator]], 0.6), 0) + 1, " - ", round(quantile(indicators_sf[[selected_indicator]], 0.8), 0)),
      paste0(round(quantile(indicators_sf[[selected_indicator]], 0.8), 0) + 1, " - ", quantile(indicators_sf[[selected_indicator]], 1))
    )
    
    tooltip <- paste(
      "District: ", indicators_sf$District, "<br/>",
      "Type: ", indicators_sf$Type, "<br/>",
      "Population: ", indicators_sf$Population, "<br/>",
      "Area: ", indicators_sf$Area, "<br/>",
      "Density: ", indicators_sf$Density, "<br/>",
      "Income: ", indicators_sf$Income, "<br/>",
      "Age: ", indicators_sf$Age,
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
      addLegend(position = "topright",
                values = ~get(selected_indicator),
                colors = colors,
                opacity = 0.9,
                labels = labels,
                title = selected_indicator)
    
  })
  
  output$indicator_map <- renderLeaflet({ rctv_indicator_map() })
  
  
  
  ### score calculation
  rctv_indicators_sf <- reactive({
    
    density_weight <- input$density_weight
    income_weight <- input$income_weight
    age_weight <- input$age_weight
    
    indicators_sf <- indicators_sf %>% 
      mutate(density_quantile = ntile(Density, n_tile),
             income_quantile = ntile(Income, n_tile),
             age_quantile = ntile(Age, n_tile)) %>% 
      mutate(Points = density_quantile * density_weight
             + income_quantile * income_weight
             + age_quantile * age_weight) %>% 
      mutate(Score = round(Points / max_total_points, 2)) %>% 
      arrange(desc(Score))
    
  })
  
  
  
  ### score map
  
  rctv_score_map <- reactive({
    
    bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    
    palette <- colorBin(
      palette = "YlOrBr",
      domain = rctv_indicators_sf()$Score,
      bins = bins,
      na.color = "transparent"
    )
    
    tooltip <- paste(
      "District: ", rctv_indicators_sf()$District, "<br/>",
      "Type: ", rctv_indicators_sf()$Type, "<br/>",
      "Population: ", rctv_indicators_sf()$Population, "<br/>",
      "Area: ", rctv_indicators_sf()$Area, "<br/>",
      "Density: ", rctv_indicators_sf()$Density, "<br/>",
      "Income: ", rctv_indicators_sf()$Income, "<br/>",
      "Age: ", rctv_indicators_sf()$Age, "<br/>",
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
      addLegend(position = "topright",
                pal = palette,
                values = ~Score,
                opacity = 0.9,
                title = "Score")
    
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
      "Income: ", row_selected$Income, "<br/>",
      "Age: ", row_selected$Age, "<br/>",
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
      select(-density_quantile,
             -income_quantile,
             -age_quantile) %>% 
      st_drop_geometry()
    
  })
  
  output$summary_table <- DT::renderDT({ rctv_summary_table() })
  
  output$download_summary_table <- downloadHandler(
    
    filename = function(){paste0("summary_table.csv")},
    content = function(fname){write.csv(rctv_summary_table(), file = fname, row.names = FALSE)}
    
  )
  
}

shinyApp(ui, server)