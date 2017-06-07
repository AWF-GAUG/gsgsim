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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Generate GSG"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # Inputs

      # Example input country code
      selectInput("country_code", "Select country",
                  c("World", raster::ccodes()[, 1]),
                  c("world", raster::ccodes()[, 2])),

      # Input administrative level
      selectInput("adm_level", "administrative level",
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
      actionButton("go", "generate"),

      hr(),

      # selection of output format
      selectInput("format", "File format",
                  c("KML" = ".kml",
                    "ESRI Shapefile" = ".shp",
                    "CSV" = ".csv")),


      # Button "generate"
      downloadButton("download", "Download")

     ),



    # Show a plot of the generated grid
    mainPanel(
       plotOutput("gsgPlot", width = "100%")


    )
  )
))
