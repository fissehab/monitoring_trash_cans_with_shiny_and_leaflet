
library(shiny)
library(leaflet)
library(plotly)

shinyUI(fluidPage(
        tags$h3("Using PostgreSQL and shiny with a dynamic leaflet map: monitoring trash cans",
                style="text-align:center;color:#990099"),
        
     column(width=6,
      leafletOutput("leaflet_map",height = "680px")
     ),
     column(width=6,
            plotlyOutput("plotly_timeseries",height = "680px"))
))
