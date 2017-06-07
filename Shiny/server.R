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

# Define server logic required to generate GSG
shinyServer(function(input, output) {


  output$gsgPlot <- renderPlot({


    # Take a dependency on input$goButton
    input$go

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
      gsg <-isolate(gen_gsg(input$dist, bnd));

      # Increment the progress bar, and update the detail text.
      incProgress(2/3, detail = paste("Generating GSG"), 2)


      # draw a map with generated GSG
      plot(bnd)
      plot(gsg, pch=20, col="red", add=TRUE)

      # Increment the progress bar, and update the detail text.
      incProgress(3/3, detail = paste("Plotting"), 3)


      # Download
      output$download <- downloadHandler(
      filename = function() { paste0("GSG", input$dist, input$country_code, ".kml") },
      content = function(file) {writeOGR(gsg, file, layer = paste("GSG",input$dist, "_kml", sep=""), driver = "KML")},
      contentType = "application/kml")
    })
  })

})
