#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
navbarPage("Global Sampling Grid", id="nav",

           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")

                        ),

      leafletOutput("map", width="100%", height="100%"),

       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                   width = 330, height = "auto",
      # Inputs
      h4("Generate GSG"),
      # Example input country code
      selectInput("country_code", "Select countries",
                  c("World", raster::ccodes()[, 1]),
                  c("world", raster::ccodes()[, 2]),
                  multiple = TRUE),

      # Input administrative level
      selectInput("adm_level", "Administrative level",
                  c("Level 0" = "0",
                    "Level 1" = "1",
                    "Level 2" = "2")),

      # Example file input for aoi
      fileInput("aoi",
                "Upload specific aoi (.shp or .kml)",
                multiple = FALSE,
                accept = c(".shp", ".kml")),
      hr(),

      # grid distance in km
      numericInput("dist", "Grid distance", 250),

      # Button "generate"
      actionButton("go", "Generate"),

      hr(),
      textOutput("text1"),


      hr(),h4("Download GSG"),

      checkboxGroupInput("variable", "Select output format:",
                         c("shp" = "ESRI Shapefile",
                           "kml" = "KML")),
      # Button "generate"
      downloadButton("download", "Download")
)
    )
  ),
tabPanel("Data Explorer", DT::dataTableOutput('mytable'))
)