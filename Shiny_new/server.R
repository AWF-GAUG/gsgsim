#########################################################
# global defenition of functions to store and retrieve data (see http://shiny.rstudio.com/articles/persistent-data-storage.html)
# functions should later be moved to utilities or "global.R"

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}
##########################################################

shinyServer(function(input, output, session) {


  ## Small preview map ############
  # Initial map (update after grid is generated, see leafletProxy)
  output$preview <- renderLeaflet({
    leaflet(height = "100%") %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = 0,
              lat = 20,
              zoom = 2)
  })

  # Update preview map

  #' Function for setting map section with sampling grid or resetting it
  #' without grid.
  #' @param aoi Use an aoi as boundary, defaults to NULL
  #' @param addGrid If TRUE, sets sampling raster
  #' @return grd
  # TODO: what kind of object is the grd actually?

  setPreview <- function(aoi=NULL, addGrid=FALSE){

    # bnd initialization doesn't need isolate in this context
    bnd <- load_boundary(
	x = aoi,
	country_code = input$country_code,
	adm_level = 0
      )

    grd <- isolate(gen_gsg(input$dist, bnd))

    leafletProxy("preview", data = grd) %>%
    fitBounds(grd@bbox[1, 1] - 1, grd@bbox[2, 1] - 1, grd@bbox[1, 2] +
      1, grd@bbox[2, 2] + 1) %>%
    clearShapes()

    # Update map for assessment
    leafletProxy("googlemap", data = grd) %>%
      fitBounds(grd@bbox[1,1]-1, grd@bbox[2,1]-1, grd@bbox[1,2]+1, grd@bbox[2,2]+1) %>%
      clearShapes() %>%

      addAwesomeMarkers(data = grd)

      if(addGrid==TRUE){
	leafletProxy("preview", data = grd) %>%
	fitBounds(grd@bbox[1, 1] - 1, grd@bbox[2, 1] - 1, grd@bbox[1, 2] +
	  1, grd@bbox[2, 2] + 1) %>%
	clearShapes() %>%

	addPolygons(
	  data = bnd,
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0.3,
          fillOpacity = 0.5
        ) %>%


        addCircles(
          data = grd,
          weight = 3,
          radius = 40,
          color = "#CD0000",
          stroke = TRUE,
          fillOpacity = 0.9
        )
      }

      return(grd)
  }


  ## Grid generation #################
  # Reset grid settings
  observeEvent(input$reset_input, {

    # removed `reset` due to undesired behavior
    # (setting country to previously selected)
    #shinyjs::reset(id = "generate")

    setPreview()
  })

  ### Enable generate button only when aoi is selected
  observe({
    toggleState(id = "go", condition = !is.null(input$country_code) | !is.null(input$aoi))
  })

  observeEvent(# Take a dependency on input$goButton. All calculations start after click only


    input$go, {

      # load selected aoi as boundary
      in_bnd <- input$aoi

      # getaoi depending on file format (currently only KML is used, but shp is possible.
      # Radiobuttons for inputformat are commented in ui)
      if (is.null(in_bnd)) {
        getaoi <- NULL
      } else {
        switch(input$inputformat,
               'kml' = {
                 dir <- dirname(in_bnd[1, 4])
                 file.rename(in_bnd[1, 4], paste0(dir, "/", in_bnd[1, 1]))

                 getaoi <-
                   list.files(dir, pattern = "*.kml", full.names = TRUE)
               },

               'shp' = {
                 dir <- dirname(in_bnd[1, 4])

                 for (i in 1:nrow(in_bnd)) {
                   file.rename(in_bnd[i, 4], paste0(dir, "/", in_bnd[i, 1]))
                 }

                 getaoi <-
                   list.files(dir, pattern = "*.shp", full.names = TRUE)

               })
      }

      # Update preview map
      gsg <- setPreview(aoi=getaoi, addGrid=TRUE)

      ### Downloading the kml file #######

      ### Enable download button when gsg is generated (inside observeEvent)
      enable("download")

      ### Export GSG to KML###
      output$download <- downloadHandler(
        filename = function() {
          paste0("GSG", input$dist, ".kml")
        },
        content = function(file) {
          writeOGR(gsg,
                   file,
                   layer = paste0("GSG", input$dist, "_KML"),
                   driver = "KML")
        }
      )


      # Generate Data Table for Data explorer

      output$datatable <- DT::renderDataTable(DT::datatable(as.data.frame(gsg), options = list(pageLength = 25), selection = 'single'))

      # Generate point list for navigation in "Assessment" (a short form of the data table)
      output$pointlist <- DT::renderDataTable(DT::datatable(as.data.frame(gsg$`1:sum(idx)`, colnames("id")), options = list(pageLength = 5, searching = FALSE, filter = 'top'), selection = 'single'))

      # Retrieve id (and lat long) of selected row in datatable (to zoom to this point)
      id <- input$datatable_row_last_clicked

      # test if id is retrived
      message(id)

      # Update maps





    }) # ObserveEvent closed

  ## Map for assessment ############
  ## Initial map (update after grid is generated, see leafletProxy)
  output$googlemap <- renderLeaflet({
    setPreview()

    leaflet() %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addTiles(
        urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
        attribution = 'Google'
      )








  })










})




