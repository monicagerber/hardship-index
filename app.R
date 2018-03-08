
library(leaflet)
library(shiny)
library(rgdal)
library(tidyverse)
library(DT)

# data by census tract

load("data-derived/acs_15_5yr_hardship.RData")
acs_15_5yr <- rename(acs_15_5yr, GEOID = GEO.id2)
tracts <- readOGR(dsn = "data-shapefiles/tl_2015_25_tract/tl_2015_25_tract.shp")

sp_data <- sp::merge(tracts, acs_15_5yr, by = "GEOID")


# shiny

ui <- fluidPage(
    
    # App title ----
    titlePanel("Economic Hardship in Massachusetts"),
    
    # Sidebar panel for inputs ----
    sidebarLayout(
        sidebarPanel(
        h3("What is the hardship index?"),
        p("The Hardship Index is a measure of economic hardship. It utilizes six indicators to provide 
          a more comprehensive view of economic hardship than single indicators.
          The calculation is based on the “Intercity Hardship Index.” 
          by Richard P. Nathan and Charles F. Adams, Jr. in “Understanding Urban Hardship,” 
          Political Science Quarterly 91 (Spring 1976): 47-62."),
        p("The economic hardship score is the median of six variables that have been standardized on
          a scale of 0 to 100. The six variables include:"),
        tags$ol(
            tags$li("Unemployment (over the age of 16 years),"), 
            tags$li("Education (over 25 years of age without a high school diploma)"), 
            tags$li("Per capita income level"),
            tags$li("Poverty (below the federal poverty level)"),
            tags$li("Crowded housing (housing units with more than one person per room)"),
            tags$li("Dependency (population under 18 or over 64 years of age)")),
        p("Higher hardship index scores indicate worse economic conditions."),
        h3("The hardship index in MA"),
        p("The Economic Hardship Index for MA utilizes the American Community Survey 2015 5-year 
          estimates. It includes indicators for Census Track Blocks in Massachusetts. Census tracts
          with small populations (under 500) were excluded from the analysis."),
        h3("More information"),
        p("This Shiny app is still under development. Monica Gerber conducted the analysis and
          developed the app."),
        p("Please contact Monica Gerber with any questions or comments (mwgerber@mgh.harvard.edu)."),
        p("Code is available on Github: https://github.com/monicagerber/hardship-index")
        width = 3),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Map", leafletOutput("mymap", height = 800)),
                        tabPanel("Data", DT::dataTableOutput("table")))
            
            )

    ))

server <- function(input, output, session) {

# hardship index polygons        
pal <- colorQuantile(
        palette = "Blues",
        domain = sp_data$ma_percentile,
        n = 10
    )
    
labels <- sprintf(
        "<strong>%s</strong><br/>Hardship Index: %g<br/>Hardship Index Percentile for MA: %d",
        sp_data@data$geo_display, sp_data@data$hardship_index, sp_data@data$ma_percentile
    ) %>% lapply(htmltools::HTML)    
    
#income polygons
pal_inc <- colorQuantile(
    palette = "Greens",
    domain = sp_data$income,
    n = 9
)
labels_inc <- sprintf(
    "<strong>%s</strong><br/>Per Capita Income: %g",
    sp_data@data$geo_display, sp_data@data$income_est
) %>% lapply(htmltools::HTML)


output$table <- DT::renderDataTable(displaytable,
                                options = list(
                                    pageLength = 5,
                                    processing=FALSE,
                                    initComplete = I("function(settings, json) {alert('Done.');}")
                                ))


output$mymap <- renderLeaflet({
    
    
    leaflet::leaflet(sp_data) %>%
        
        addTiles() %>%
        
        addPolygons(fillColor = ~pal(ma_percentile), weight = 2, opacity = .8, 
                    color = "white", dashArray = "3", fillOpacity = 0.8,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"),
                    group = "Hardship Index") %>%
                
        setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%
        
        addLayersControl(
                overlayGroups = c("Hardship Index"),
                options = layersControlOptions(collapsed = FALSE)) %>%
        
        addLegend("bottomright", pal = pal, values = ~ma_percentile,
                  title = "Hardship Index Percentile",
                  #labFormat = labelFormat(prefix = "Percentile "),
                  opacity = 1
        )
})
}    

shinyApp(ui, server)
