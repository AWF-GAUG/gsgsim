
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(plotGoogleMaps)
library(shinyjs)
library(gsgsim)
library(leaflet)
library(leaflet.minicharts)
library(raster)
library(DT)
library(dplyr)


ui <- dashboardPage(
  dashboardHeader(title = "GSG"),

  # Sidebar with menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Generate grid", tabName = "generate", icon = icon("gears")),
      menuItem("Assessment", tabName = "assessment", icon = icon("eye")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Analyze", tabName = "analyze", icon = icon("bar-chart"))
    )
  ),


  dashboardBody(
    tabItems(
      tabItem("generate",
              fluidRow(
                useShinyjs(),
                box(id = "generate",
                    width = 6, solidHeader = TRUE,
                    title = "GSG settings",

                    # Select country code
                    selectInput(
                      "country_code",
                      "Select countries",
                      c("World", raster::ccodes()[, 1]),
                      c("world", raster::ccodes()[, 2]),
                      multiple = TRUE
                    ),

                    # File input for aoi
                    fileInput("aoi",
                              label = "Upload specific aoi as KML",
                              accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', ".prj", ".kml"),
                              multiple = TRUE
                    ),

                    # grid distance in km
                    numericInput("dist", "Grid distance in km", 250
                    ),

                    # Button "Reset"
                    actionButton("reset_input", "Reset",
                                 icon("refresh")
                    ),
                    # Button "Generate" (disabled until aoi is selected)
                    shinyjs::disabled(actionButton("go", "Generate",
                                                   icon("play")
                    )),

                    # Button "Download" (disabled until grid is generated)
                    shinyjs::disabled(downloadButton("download", "Download"))

                ),# box generate closed

                # infoBox showing resulting sample size (only after grid is generated)
                infoBoxOutput("samplesize")

                # Preview map
              ),
              fluidRow(
                box(
                  width = 12, solidHeader = TRUE,
                  title = "Preview",
                  leafletOutput("preview",height = "50vh")
                )
              )
      ), # tab generate closed

      tabItem("assessment",

				div(class='outer',
					tags$head(includeScript('gomap.js')) # includes gomap?
					), 

              fluidRow(
                box(id = "mapwindow", width = 8, height = "60vh",
                    leafletOutput('googlemap', height = "58vh")


                ),

                box(id = "pointlist", width = 3, height = "60vh",
                    title = "Point list",
                    DT::dataTableOutput('pointlist'),
										#icon('crosshair')
										actionButton('zoomToPoint', 'zoom to point'),

										br(),
										br(),

										actionButton('zoomToGrid', 'zoom to grid')
                )
              ),

              fluidRow(
                tabBox(id = "variables", width = 12,
                       title = "Assess variables",
                       selected = "Tab3",
                       tabPanel("Land Cover Components (LCC)", 
			       #verbatimTextOutput('lcc'),
			       selectInput('lcc_select', 'Select point from list', NULL)
			       ),
                       tabPanel("Land use / function attributes (LUA)", "Tab content 2"),
                       tabPanel("Landscape characteristics", "Note that when side=right, the tab order is reversed.")
                )

              )

      ), # tab assessment closed

      tabItem("explorer",
              fluidRow(
                box(width = 12,
                    title = "Data Explorer",
                    DT::dataTableOutput('datatable')
                )
              )


      ), # tab explorer closed

      tabItem("analyze",
              fluidRow(
                column(3,
                       "Select variable"),

                column(
                  8,
                  "Results",

                  # tabsetPanel
                  tabsetPanel(
                    type = "tabs",

                    tabPanel("Plot", plotOutput("plot")),

                    tabPanel("Summary", verbatimTextOutput("summary")),

                    tabPanel("Table", tableOutput("table"))
                  )
                )
              )) # tab analyze closed
    )





  )
)
