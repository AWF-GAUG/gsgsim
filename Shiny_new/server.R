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

	# Cleanup routine. Maybe used to close database
	# connections or detach data. It only returns
	# and 'Exit' message now with helps with debugging,
	# but I'll just leave it here (lburggr).
	cleanupOnExit <- session$onSessionEnded(function(){
		message('Exit')
})


  # Reactive values for storing the global sampling
	# grid and further attributes.
	#' @param gsg Global sampling grid
	#' @param bnd Boundary
	#' @param lcc Dummy attribute table (land cover classes)
  attr.tab <- reactiveValues(gsg=NULL, bnd=NULL,
		lcc=data.frame(id=NA, trees=NA))

  ## Small preview map ############
  # Initial map (update after grid is generated, see leafletProxy)
  output$preview <- renderLeaflet({
    leaflet(height = "100%") %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = 0,
              lat = 20,
              zoom = 2)
  })

  ## Map for assessment ############
  ## Initial map (update after grid is generated, see leafletProxy)
  output$googlemap <- renderLeaflet({

		updateGoogleMap() # see description in the next function

		message('Google Map created')

    leaflet() %>%
      addTiles(
        urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
        attribution = 'Google'
      )

	})

	# googlemap shows some strange behavior -- it is _only_ created
	# when the assessment tab is clicked. If the google map is
	# updated as usual by a simple call to leafletProxy on
	# click of the go button (which creates the grid),
	# it is overwritten by the creation of the
	# empty map on changing to the assessment tab. As a
	# workaround, updating the map is part of the following
	# reactive function which is included in the _creation_
	# of the map above.
	# It's not beautiful, as the empty map flashes up shortly
	# after clicking the assessment tab, but it works.

	updateGoogleMap <- reactive({

		# update googlemap
		if(is.null(attr.tab$gsg))
			return()

		message('Google Map updated')

		### create standard markers
		# (standard marker provided by `addAwesomeMarkers`
		# looks a bit different, which looks strange when
		# markers are updated on point selection)
		blue.marker <- makeIcon(
			iconUrl='marker-icon.png',
			shadowUrl='marker-shadow.png'
			)

		grd <- attr.tab$gsg # get grid from reactive value list
		leafletProxy("googlemap", data = grd) %>%
			fitBounds(grd@bbox[1,1]-1, grd@bbox[2,1]-1, grd@bbox[1,2]+1,
				grd@bbox[2,2]+1) %>% addMarkers(data = grd, icon=blue.marker)
	})

  ## Grid generation #################
  # Reset grid settings
  observeEvent(input$reset_input, {

		# TODO: global variables should be set to zero here
		# and tables in assessment and data exploerer removed
		# accordingly
		#attr.tab$gsg <- NULL
		#attr.tab$<- NULL

		leafletProxy('preview') %>%
			clearShapes()

		leafletProxy('googlemap') %>%
			clearMarkers()
  })

  ### Enable generate button only when aoi is selected
  observe({
    toggleState(id = "go", condition = !is.null(input$country_code) | !is.null(input$aoi))
  })

	### Generate grid by click on 'go' button
  observeEvent(# Take a dependency on input$goButton. All calculations start after click only


    input$go, {

      # load selected aoi as boundary
      in_bnd <- input$aoi

      # getaoi depending on file format (currently only KML
			# is used, but shp is possible.
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

      # bnd initialization doesn't need isolate in this context
      attr.tab$bnd <- load_boundary(
      x = getaoi,
      country_code = input$country_code,
      adm_level = 0
        )

			# generate grid and save to reactive variable
      attr.tab$gsg <- isolate(gen_gsg(input$dist, attr.tab$bnd))

      ### Downloading the kml file #######

      ### Enable download button when gsg is generated (inside observeEvent)
      enable("download")

      ### Export GSG to KML###
      output$download <- downloadHandler(
        filename = function() {
          paste0("GSG", input$dist, ".kml")
        },
        content = function(file) {
          writeOGR(grd,
                   file,
                   layer = paste0("GSG", input$dist, "_KML"),
                   driver = "KML")
        }
      )


      # Generate Data Table for Data explorer

      output$datatable <- DT::renderDataTable(
          DT::datatable(as.data.frame(attr.tab$gsg),
						options = list(pageLength = 25),
              selection = 'single'))

      # Generate point list for navigation in "Assessment"
			# (a short form of the # data table)
      gsg.id <- attr.tab$gsg[['1:sum(idx)']]

      output$pointlist <- DT::renderDataTable({

				pnts <- cbind(gsg.id, as.data.frame(attr.tab$gsg)[,c('X','Y')])
				names(pnts) <- c('id', 'lon', 'lat')

				# datatable for point list
				DT::datatable(data.frame(id=pnts$id, lon=round(pnts$lon, 3),
						lat=round(pnts$lat, 3)),
					options = list(pageLength = 5, searching = FALSE),
					escape = FALSE,
					selection='single', rownames=FALSE)

			})

			# initialize empty land cover list
			# by reading the names of files in `code_lists`
			# directory.
			attr.tab$lcc <- list()
			length(attr.tab$lcc) <- length(dir('code_lists'))
			nms <- sapply(strsplit(dir('code_lists'), '\\.'),
				function(x) x[1])
			names(attr.tab$lcc) <- nms

			# create empty (NA) factors from list elements
			# with levels according to code lists.
			# length of each vector is the number of points.
			for (i in 1:length(attr.tab$lcc)){

				# read code list
				cds <- read.csv(paste('code_lists/',
						names(attr.tab$lcc)[[i]], '.csv',
						sep=''), stringsAsFactors=FALSE, head=FALSE)[,1]

				# create factor
				attr.tab$lcc[[i]] <- factor(rep(NA, length(gsg.id)), levels=cds)
			}

			# convert list to data.frame
			attr.tab$lcc <- as.data.frame(attr.tab$lcc)

    }) # ObserveEvent closed


	### Updating maps

  # Update map for assessment
	observe({
		if(is.null(attr.tab$gsg) | is.null(attr.tab$bnd))
			return()

		grd <- attr.tab$gsg
		leafletProxy("preview", data = grd) %>%
			fitBounds(grd@bbox[1, 1] - 1, grd@bbox[2, 1] - 1,
				grd@bbox[1, 2] + 1, grd@bbox[2, 2] + 1) %>%
		clearShapes() %>%
		addPolygons(
			data = attr.tab$bnd,
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
	})

  # when row is selected from pointlist, update
  # variable selection
  observeEvent(input$pointlist_rows_selected, {

    # update id for attribute selection
    ind <- input$pointlist_row_last_clicked

    updateSelectInput(session, 'lcc_select',
      label=paste('ID:', ind),
      choices=names(attr.tab$lcc))


    # update markers (highlight selected point)
    n <- 1:nrow(attr.tab$lcc) # nrow of attribute table

    # create yellow marker for selected point
    highlightMarker <- makeIcon(
      # if id of selected row matches attribute table row, set yellow marker
      iconUrl=ifelse(n==ind, 'marker-icon-yellow.png', 'marker-icon.png'),
      shadowUrl='marker-shadow.png'
      )

		leafletProxy('googlemap') %>%
			addMarkers(data = attr.tab$gsg, icon=highlightMarker)

    })

	# update lcc factor level selection
	observeEvent(input$lcc_select, {
		if(input$lcc_select=='')
			return()

		updateSelectInput(session, 'lcc_levels_select',
			choices=levels(attr.tab$lcc[, input$lcc_select]))
	})


  # when lcc level is selected, update reactive value tables
  observe({

		if(input$lcc_select=='' | input$lcc_levels_select=='')
			return()

    ind <- input$pointlist_row_last_clicked
    attr.tab$lcc[ind, input$lcc_select] <- input$lcc_levels_select

        })



	# observer handling zoom when goto button is clicked
	observe({
	   if (input$zoomToPoint==0)
	     return()

		 isolate({
			 # get last selected row (should correspond to id)
		   ind <- input$pointlist_row_last_clicked
			 gsg <- attr.tab$gsg

			 gsg.id <- gsg[['1:sum(idx)']]

		   # get corrdinates from grid and bind to ID dataframe
		   pnts <- cbind(gsg.id, as.data.frame(gsg)[,c('X','Y')])
		   names(pnts) <- c('id', 'lon', 'lat')

		   dst <- 0.1
		   lat <- pnts$lat[ind]
		   lon <- pnts$lon[ind]

		   leafletProxy("googlemap") %>%
		  		 fitBounds(lon - dst, lat - dst, lon + dst, lat + dst)
		 })
	 })


	# observer handling zoom when goto button is clicked
	observe({
	   if (input$zoomToGrid==0)
	     return()

		 # Needs to be isolated to not be triggered
		 # in case `attr.tab$gsg` changes
		 isolate({

			 grd <- attr.tab$gsg

			 leafletProxy("googlemap", data = grd) %>%
				 fitBounds(grd@bbox[1, 1] - 0.1, grd@bbox[2, 1] - 0.1,
					 grd@bbox[1, 2] + 0.1, grd@bbox[2, 2] + 0.1)
		 })
	 })










})




