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

  # reactive values for storing attributes
  attr.tab <- reactiveValues(lcc=data.frame(id=NA, trees=NA))

  ### create standard markers
  blueMarker <- makeIcon(
    iconUrl='marker-icon.png',
    shadowUrl='marker-shadow.png'
    )


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
  #' @param markers Optional markers for googleMap update
  #' @return grd
  # TODO: what kind of object is the grd actually?

  setPreview <- function(aoi=NULL, addGrid=FALSE, markers=NULL){

    # check if markers are present and use blue marker if not
    if(is.null(markers))
      markers <- blueMarker

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

      addMarkers(data = grd, icon=markers)

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

      output$datatable <- DT::renderDataTable(
          DT::datatable(as.data.frame(gsg), options = list(pageLength = 25),
              selection = 'single')) 

      # Generate point list for navigation in "Assessment" (a short form of the 
      # data table)
      gsg.id <- gsg[['1:sum(idx)']]

      output$pointlist <- DT::renderDataTable({

				pnts <- cbind(gsg.id, as.data.frame(gsg)[,c('X','Y')])
				names(pnts) <- c('id', 'lon', 'lat') 
				message(str(pnts))
				
				d <- data.frame(zoom = paste('<a class="go-map" href="" data-lat="', 
							pnts$lat, '" data-lon="', pnts$lon, 
							'"><i class="fa fa-crosshairs"></i></a>', sep=""))
				
				# this creates a json object and returns its url
				# I think
				action <- DT::dataTableAjax(session, d)

				#DT::datatable(d, options = list(ajax = list(url = action)), 
				#	escape = FALSE)

				# this creates a data table again from the json object? 
				DT::datatable(d, options = list(ajax = list(url = action), 
							pageLength = 5, searching = FALSE, 
						selection = 'single'), escape = FALSE)

				#DT::datatable( 
				#	
				#	data.frame(ID=gsg.id), 
        #  options = list(pageLength = 5, 
        #      searching = FALSE, filter = 'top'), 
        #  selection = 'single')

			})

      # update attribute table
      attr.tab$lcc <- data.frame(id=gsg.id, 
          trees=factor('tree', levels=c('tree', 'no tree')))

            # Retrieve id (and lat long) of selected row in datatable (to zoom to this point)
      # Update maps

    }) # ObserveEvent closed
  
  # when row is selected from pointlist, update
  # variable selection
  observeEvent(input$pointlist_rows_selected, { 

    # update id for attribute selection 
    ind <- input$pointlist_row_last_clicked 
    
    updateSelectInput(session, 'lcc_select', 
      label=paste('ID:', ind),
      choices=levels(attr.tab$lcc$trees))

    # update markers (highlight selected point)
    n <- 1:nrow(attr.tab$lcc) # nrow of attribute table 

    # create marker with yellow marker for selected point 
    highlightMarker <- makeIcon(
      # if id of selected row matches attribute table row, set yellow marker
      iconUrl=ifelse(n==ind, 'marker-icon-yellow.png', 'marker-icon.png'),
      shadowUrl='marker-shadow.png'
      )

    setPreview(addGrid=TRUE, markers=highlightMarker)
    
    }) 

  # when input is selected, update reactive value tables
  observeEvent(input$lcc_select, {
      
    ind <- input$pointlist_row_last_clicked 
    attr.tab$lcc$trees[ind] <- input$lcc_select 
    
    message(print(attr.tab$lcc))

        })

	# observer handling zoom when goto button is clicked
	observe({
	   if (is.null(input$goto))
	     return() 
	   message('Goto button clicked') 
	   
	   isolate({
	     dst <- 0.5
	     lat <- input$goto$lat
	     lon <- input$goto$lon

		 message(paste('lat =', lat))
		 message(paste('lon =', lon))

	     leafletProxy("googlemap") %>% 
		 fitBounds(lon - dst, lat - dst, lon + dst, lat + dst)
	   })
	 })


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




