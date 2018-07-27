
library(leaflet)
library(shiny)
library(rgdal)
library(tidyverse)
library(DT)
library(highcharter)

# data by census tract

load("data-derived/acs_15_5yr_hardship.RData")



# shiny

ui <- fluidPage(
    
    # App title ----
    titlePanel("Economic Hardship in Massachusetts"),
    
    # Sidebar panel  ----
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
        p("Code is available on ", a(href="https://github.com/monicagerber/hardship-index", "Github.")),
        width = 3),
        # Main Panel with tabs ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Map", 
                                 leafletOutput("map", height = 400),
                                 h6("Click a census tract for more information."),
                                 highchartOutput('raceplot')),
                        tabPanel("Data", 
                                 fluidRow(
                                     column(1,
                                            numericInput("minScore", "Min score", min=0, max=1, value=0)
                                     ),
                                     column(1,
                                            numericInput("maxScore", "Max score", min=0, max=1, value=1)
                                     )
                                 ),
                                 hr(),
                                 DT::dataTableOutput("table"),
                                 downloadLink('downloadCSV', label = 'Download all data to .csv file')
                        )

            
    ))))

server <- function(input, output, session) {

# data table output 
output$table <- DT::renderDataTable({
    
    df <- displaytable %>%
        filter(hardship_index >= input$minScore,
               hardship_index <= input$maxScore)
    
    DT::datatable(df, 
                  options = list(pageLength = 10))
    })

# download link for data
output$downloadCSV <- downloadHandler(
    filename = 'ma_hardship_data.csv', 
    content = function(file) {
        write_csv(display_table, file)
    }
)

# palette for polygons    
pal <- colorNumeric('Blues', NULL)

# labels for polygons    
labels <- sprintf(
    "<strong>%s</strong>
     <br/>Hardship Index: %g",
    sp_data@data$geo_display, 
    sp_data@data$hardship_index) %>% 
    lapply(htmltools::HTML)    

# map output
output$map <- renderLeaflet({
    
    leaflet::leaflet(sp_data) %>%
        
        addTiles() %>%
        
        clearShapes() %>%
        
        addPolygons(fillColor = ~pal(hardship_index), fillOpacity = 0.8,
                    weight = 2, opacity = .8,
                    color = "white", dashArray = "3",
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                     # label = labels,
                     # labelOptions = labelOptions(
                     #     style = list("font-weight" = "normal", padding = "3px 8px"),
                     #     textsize = "15px",
                     #     direction = "auto"),
                    group = "Hardship Index",
                    layerId = sp_data@data$GEOID) %>%
                
        setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%
        
         addLayersControl(
                 overlayGroups = c("Hardship Index"),
                 options = layersControlOptions(collapsed = FALSE)) %>%
        
        addLegend("bottomright", pal = pal, values = ~hardship_index,
                  title = "Hardship Index",
                  opacity = 1
        )
})


# click event for map (used to generate bar chart)
click_tract <- eventReactive(input$map_shape_click, {
    
    x <- input$map_shape_click
    
    y <- x$id
    
    return(y)
})

# adds outline to map if polygon is clicked
observe({

    req(click_tract()) # do this if click_tract() is not null

    # add the clicked tract to the map in aqua, and remove when a new one is clicked
    map <- leafletProxy('map') %>%
        removeShape('htract') %>%
        addPolygons(data = sp_data[sp_data$GEOID == click_tract(), ], fill = FALSE,
                    color = '#00FFFF', opacity = 1, layerId = 'htract')

})

# data for bar chart
tract_data <- reactive({

    # Fetch data for the clicked tract
    return(sp_data@data[sp_data@data$GEOID == click_tract(), ])

})

# bar chart for race/ethnicity
output$raceplot <- renderHighchart({

    chart <- highchart() %>%
        hc_chart(type = 'column') %>%
        hc_legend(enabled = FALSE) %>%
        hc_xAxis(categories = c('White', 'Black', 'Hispanic', 'Asian or NHPI', "Native American",
                                'Other', 'Biracial'), title = list(text = 'Race/ethnicity')) %>%
        hc_yAxis(title = list(text = 'Population')) %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
        hc_add_series(name = 'Population, 2015', data = c(tract_data()$white,
                                                          tract_data()$black,
                                                          tract_data()$hispanic,
                                                          tract_data()$asian_nhpi,
                                                          tract_data()$american_indian,
                                                          tract_data()$other,
                                                          tract_data()$biracial)) %>%
         hc_title(text = paste0(as.character(tract_data()$geo_display)),
                  align = 'left') %>%
         hc_subtitle(text = paste0('Hardship Index: ', as.character(round(tract_data()$hardship_index, 2)),
                                   ', Population: ', as.character(tract_data()$pop_total)),
                     align = 'left') %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_colors(c('#d01010', '#d01010')) %>%
        hc_tooltip(enabled = FALSE)



    chart

})


}    

shinyApp(ui, server)


# to deploy: rsconnect::deployApp()