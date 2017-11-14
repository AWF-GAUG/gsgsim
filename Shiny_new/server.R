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



  ## Grid generation #################

  # Reset grid settings
  observeEvent(input$reset_input, {
    shinyjs::reset(id = "generate")
  })

  ### Enable generate button only when aoi is selected
  observe({
    toggleState(id = "go", condition = !is.null(input$country_code) | !is.null(input$aoi))
  })




  observeEvent(# Take a dependency on input$goButton. All calculations start after click only


    input$go, {
      # load selected aoi as boundary
      in_bnd <- input$aoi

      # getaoi depending on file format (currently only KML is used, but shp is possible. Radiobuttons for inputformat are commented in ui)
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

      bnd <- isolate(load_boundary(
        x = getaoi,
        country_code = input$country_code,
        adm_level = 0
      ))

      # generate GSG based on inputs from ui.R
      # isolate () to avoid dependency on input$dist (but reactive on button)

      gsg <- isolate(gen_gsg(input$dist, bnd))

      # Show resulting sample size in valueBox
      output$samplesize = renderInfoBox(infoBox(
        length(gsg),
        title = "sample size",
        icon = icon("map-pin")
      ))

      # Update preview map

      leafletProxy("preview", data = gsg) %>%
        fitBounds(gsg@bbox[1, 1] - 1, gsg@bbox[2, 1] - 1, gsg@bbox[1, 2] +
                    1, gsg@bbox[2, 2] + 1) %>%
        clearShapes() %>%

        addPolygons(
          data = bnd,
          #color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0.3,
          fillOpacity = 0.5
        ) %>%


        addCircles(
          data = gsg,
          weight = 3,
          radius = 40,
          color = "#CD0000",
          stroke = TRUE,
          fillOpacity = 0.9
        )

      # Update map for assessment
      leafletProxy("googlemap", data = gsg) %>%
        fitBounds(gsg@bbox[1,1]-1, gsg@bbox[2,1]-1, gsg@bbox[1,2]+1, gsg@bbox[2,2]+1) %>%
        clearShapes() %>%

        addAwesomeMarkers(data = gsg)


      # addCircles(
      #   data = gsg,
      #   weight = 3,
      #   radius = 40,
      #   color = "#CD0000",
      #   stroke = TRUE,
      #   fillOpacity = 0.9
      # )




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


      # # Plot se_plot
      # output$se_plot <- renderPlot({
      #   se_binomial <- function(p){
      #     se <- 100*sqrt((p*(1-p))/(length(gsg)-1))/p
      #     return(se)
      #   }
      #
      #   curve(se_binomial,
      #         from=0.001,
      #         to=1,
      #         main = "Expected standard errors for grid points (no clusters)",
      #         xlab="Expected area proportion of target class",
      #         #log= "x",
      #         ylab="Relative standard error (%)"
      #   )
      # })



      # Generate Data Table for Data explorer

      output$datatable <- DT::renderDataTable(DT::datatable(as.data.frame(gsg), options = list(pageLength = 25), selection = 'single'))

      # Generate point list for navigation in "Assessment" (a short form of the data table)
      output$pointlist <- DT::renderDataTable(DT::datatable(as.data.frame(gsg$`1:sum(idx)`, colnames("id")), options = list(pageLength = 5, searching = FALSE, filter = 'top'), selection = 'single'))

      # Retrieve id (and lat long) of selected row in datatable (to zoom to this point)
      id <- input$datatable_row_last_clicked

      # test if id is retriefed
      message(id)

      # Update maps





    }) # ObserveEvent closed

  ## Map for assessment ############
  ## Initial map (update after grid is generated, see leafletProxy)
  output$googlemap <- renderLeaflet({
    leaflet() %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addTiles(
        urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
        attribution = 'Google'
      )








  })










})




