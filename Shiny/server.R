#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyjs)
library(gsgsim)
library(leaflet)
library(raster)
library(DT)


options(shiny.error = browser)
# Define server logic required to generate GSG
shinyServer(function(input, output) {


output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("OpenStreetMap")

})

observeEvent(input$reset_input, {
  shinyjs::reset("")
})

observeEvent(
    # Take a dependency on input$goButton
    input$go, {

    withProgress(message = 'Work in progress', value = 0,{

      # load selected aoi as boundary
      in_bnd <- input$aoi
      if (is.null(in_bnd)) {
        getaoi <- NULL
      } else {
        switch(input$inputformat,
               'kml' = {
                 dir<-dirname(in_bnd[1,4])
                 file.rename(in_bnd[1,4], paste0(dir,"/",in_bnd[1,1]))
                 getaoi <- list.files(dir, pattern="*.kml", full.names=TRUE)

               },
               'shp' = {

                dir<-dirname(in_bnd[1,4])

                for ( i in 1:nrow(in_bnd)) {
                   file.rename(in_bnd[i,4], paste0(dir,"/",in_bnd[i,1]))}

                getaoi <- list.files(dir, pattern="*.shp", full.names=TRUE)

            })
      }



      # Increment the progress bar, and update the detail text.
      incProgress(1/3, detail = paste("Dowloading boundary (can take a while...)"), 1)


      bnd <- isolate(load_boundary(x = getaoi,
                                   country_code = input$country_code,
                                   adm_level = 0));


      # generate GSG based on inputs from ui.R
      # isolate () to avoid dependency on input$dist (but reactive on button)
      gsg <- isolate(gen_gsg(input$dist, bnd));

      output$text1 = renderText({paste0("Generated points: ",length(gsg))})

      if(length(gsg)==1)
      {
        showModal(modalDialog(
        title = "Warning",
        "No sample locations in the area of interest! Adjust grid distance!",
        easyClose = FALSE
        ))

      }else{

      center=gCentroid(gsg)
      # Increment the progress bar, and update the detail text.
      incProgress(2/3, detail = paste("Generating GSG"), 2)


      # draw a map with generated GSG
      leafletProxy("map") %>%
        clearShapes() %>%

        addPolygons(data = bnd, color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 0.4, fillOpacity = 0.5) %>%

        addCircles(data=gsg, weight = 3, radius=40,
                  color="#CD0000", stroke = TRUE, fillOpacity = 0.9) %>%


        setView(center$x, center$y, zoom=5)
        #%>%
        #setMaxBounds(lng1 = xmax(gsg),lat1 = ymax(gsg),
        #          lng2 = ymin(gsg),lat2 = ymin(gsg))


      # Increment the progress bar, and update the detail text.
      incProgress(3/3, detail = paste("Plotting"), 3)

      # Generate Data Table
      output$mytable <- DT::renderDataTable(
        DT::datatable(as.data.frame(gsg), options = list(pageLength = 25))
      )


      }
    })

      ### Export GSG ###

     observeEvent(input$format,
      if(input$format == "shp"){

      output$download <- downloadHandler(
          filename = 'GSGExport.zip',
          content = function(file) {
           if (length(Sys.glob("GSGExport.*"))>0){
             file.remove(Sys.glob("GSGExport.*"))
            }
            writeOGR(gsg, dsn="GSGExport.shp", layer="GSGExport", driver="ESRI Shapefile")
            zip(zipfile='GSGExport.zip', files=Sys.glob("GSGExport.*"))
            file.copy("GSGExport.zip", file)
            if (length(Sys.glob("GSGExport.*"))>0){
              file.remove(Sys.glob("GSGExport.*"))
            }
          })
      }
      else if (input$format == "kml"){

        output$download <- downloadHandler(
          filename = function() {
          paste0("GSG", input$dist, ".kml")
          },
          content = function(file) {
          writeOGR(gsg, file, layer = paste0("GSG",input$dist, "_KML"), driver = "KML")
          })
      }
     )
    })
    })