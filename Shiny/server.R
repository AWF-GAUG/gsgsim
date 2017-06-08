#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(gsgsim)
library(leaflet)
library(raster)
library(DT)

# Define server logic required to generate GSG
shinyServer(function(input, output) {


output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("OpenStreetMap")

})

observeEvent(
    # Take a dependency on input$goButton
    input$go, {

    withProgress(message = 'Work in progress', value = 0,{

      # load selected aoi as boundary
      in_bnd <- input$aoi
      if (is.null(in_bnd)) {
        bnd_path <- NULL
      } else {
        bnd_path <- in_bnd$datapath
      }

      # Increment the progress bar, and update the detail text.
      incProgress(1/3, detail = paste("Dowloading boundary (can take a while...)"), 1)

      bnd <- isolate(load_boundary(x = bnd_path,
                                   country_code = input$country_code,
                                   adm_level = 0));


      # generate GSG based on inputs from ui.R
      # isolate () to avoid dependency on input$dist (but reactive on button)
      gsg <- isolate(gen_gsg(input$dist, bnd));

      # Increment the progress bar, and update the detail text.
      incProgress(2/3, detail = paste("Generating GSG"), 2)


      # draw a map with generated GSG
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data = bnd, color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 0.4, fillOpacity = 0.5) %>%
<<<<<<< HEAD
        addCircles(data = gsg, weight = 3, radius = 40,
                  color = "#CD0000", stroke = TRUE, fillOpacity = 0.9) %>%
        fitBounds(lng1 = xmax(bnd),lat1 = ymax(bnd),
                  lng2 = ymin(bnd),lat2 = ymin(bnd))
=======
        addCircles(data=gsg, weight = 3, radius=40,
                  color="#CD0000", stroke = TRUE, fillOpacity = 0.9) %>%
        fitBounds(lng1 = xmax(gsg),lat1 = ymax(gsg),
                  lng2 = ymin(gsg),lat2 = ymin(gsg))
>>>>>>> dd8214653c5772fa8ced2bce5a005cb481725589

      # Increment the progress bar, and update the detail text.
      incProgress(3/3, detail = paste("Plotting"), 3)

      output$mytable <- DT::renderDataTable(
        DT::datatable(as.data.frame(gsg), options = list(pageLength = 25))
      )


      ### Export GSG ###
      output$download <- downloadHandler(
        filename = function() { paste0("GSG", input$dist, input$country_code, ".",input$variable) },
        content = function(file) {writeOGR(gsg, file, layer = paste0("GSG",input$dist, "_",input$variable), driver = input$variable)}
        )
    })
  })
})