observe({
  # withProgress(message = "Generating t-SNE Plot. Please wait...", {
  groupingVariables <- inputDataReactive()$groupingVariables
  mdsResults <- inputDataReactive()$mdsResults
  # factorlevels <- inputDataReactive()$factorlevels
  

  gg_fill_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  gvList <- c(colnames(groupingVariables), "-")
  updateSelectInput(session = session, "colorGroupMDS", choices = colnames(groupingVariables), selected = colnames(groupingVariables)[2]) # try: selected = colnames(groupingVariables)[2] ([1] is sample name)
  updateSelectInput(session = session, "shapeGroupMDS", choices = gvList, selected = "-")
  
  observeEvent( # Event number 0
    {
      input$colorGroupMDS
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = F, # default = TRUE
    {
      updateSelectizeInput(
        # session = getDefaultReactiveDomain(),
        session = session,
        inputId = "selectizeMDS",
        choices = as.list(unique(groupingVariables[[input$colorGroupMDS]])),
        selected = as.list(unique(groupingVariables[[input$colorGroupMDS]])),
        server = TRUE
        # server = TRUE
      )
    }
  ) # close Event number 0
  
  
  output$colourPanelMDS <- renderUI({
    levMDS <- sort(unique(input$selectizeMDS)) # sorting so that "things" are unambigious
    colsMDS <- gg_fill_hue(length(levMDS))
    
    # New IDs "colX1" so that it partly coincide with input$selectizeMDS...
    lapply(seq_along(levMDS), function(i) {
      colourInput(inputId = paste0("colMDS_", levMDS[i]),
                  label = paste0("Choose colour for ", levMDS[i]),
                  value = colsMDS[i]
      )
    })
    
  })
  
  # dimList <- colnames(mdsTable)[-(1:nGrouping)]
  # updateSelectInput(session = session, "pickFactor1MDS", choices = dimList, selected = "X1")
  # updateSelectInput(session = session, "pickFactor2MDS", choices = dimList, selected = "X2")
  
  # print(input$paramButtonMDS)
  # cat(input$paramButtonMDS)
  
  # input$paramButtonMDS <- 1
  
  observeEvent( # Event number 1
    {
      input$paramButtonMDS
      # input$removeSamplesMDS
      # input$perplexityMDS
      # input$max_iterMDS
      # input$thetaMDS
      # input$etaMDS
      # input$perplexityMDS # or precompute?
      # lapply(seq_along(colourList), function (i) {
      #   input[[paste0("GroupColour", i)]]
      # })
      # 
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = F, # default = TRUE
    {
      
      withProgress(message = "Generating MDS Plot. Please wait...", {
        
        mdsCoords <- mdsResults %>%
          select(num_range(prefix = "C", range = 1:5))
        mdsTable <- data.frame(groupingVariables, mdsCoords, stringsAsFactors = FALSE, row.names = rownames(groupingVariables))
        
        observeEvent(
          { # Event number 2: only need axis inputs here, for the others no need for redrawing plot (?)
            input$sampleLabelsMDS
            # input$pickFactor1MDS
            # input$pickFactor2MDS
            req(input$colorGroupMDS)
            input$shapeGroupMDS
            input$plotWidthMDS
            input$plotHeightMDS
            input$textSizeMDS
            input$pointSizeMDS
            input$displayButtonMDS
            input$selectThemeMDS
            input$MDSTitle
            
          },
          ignoreInit = F, # default = FALSE
          ignoreNULL = F, # default = TRUE
          {

            colsMDS <- paste0("c(", paste0("input$colMDS_", sort(input$selectizeMDS), collapse = ", "), ")")
            # print(colsMDS)
            colsMDS <- eval(parse(text = colsMDS))
            # print(colsMDS)
            
            if (is.null(colsMDS)) {
              colsMDS <- gg_fill_hue(length(sort(unique(groupingVariables[[input$colorGroupMDS]]))))
            }
            
            # To prevent errors
            # req(length(colsMDS) == length(input$selectizeMDS))
            
            if (input$shapeGroupMDS == "-") {
              plotMDS <- mdsTable %>%
                # ggplot(aes(x = .data[[input$pickFactor1MDS]], y = .data[[input$pickFactor2MDS]], color = .data[[input$colorGroupMDS]], fill = .data[[input$colorGroupMDS]])) +
                ggplot(aes(x = .data[["C1"]], y = .data[["C2"]], color = .data[[input$colorGroupMDS]], fill = .data[[input$colorGroupMDS]])) +
                geom_point(size = as.numeric(input$pointSizeMDS)) +
                scale_color_manual(values = colsMDS)
              # geom_point(size = as.numeric(input$pointSizeMDS))
            } else {
              plotMDS <- mdsTable %>%
                # ggplot(aes(x = .data[[input$pickFactor1MDS]], y = .data[[input$pickFactor2MDS]], color = .data[[input$colorGroupMDS]], fill = .data[[input$colorGroupMDS]], shape = .data[[input$shapeGroupMDS]])) +
                ggplot(aes(x = .data[["C1"]], y = .data[["C2"]], color = .data[[input$colorGroupMDS]], fill = .data[[input$colorGroupMDS]], shape = .data[[input$shapeGroupMDS]])) +
                geom_point(size = as.numeric(input$pointSizeMDS)) +
                scale_color_manual(values = colsMDS) +
                scale_shape_manual(values = c(rep(c(21, 22, 23, 24, 25, 8, 3, 4), times = 10))[1:nlevels(as.factor(mdsTable[[input$shapeGroupMDS]]))])
              ### Note: 21-25 are simple shapes, but with filling; 8,3,4 are crosses with different amount of lines
            }
            
            if (input$sampleLabelsMDS) {
              plotMDS <- plotMDS +
                geom_text(
                  aes(label = rownames(mdsTable)),
                  size = (input$textSizeMDS / 3),
                  hjust = 0.2, vjust = -1.5, check_overlap = T,
                  show.legend = FALSE
                ) # +
              # ylim(c(min(mdsTable[[input$pickFactor2MDS]]*1.1), max(mdsTable[[input$pickFactor2MDS]]*1.2))) +
              # xlim(c(min(mdsTable[[input$pickFactor1MDS]]*1.1), max(mdsTable[[input$pickFactor1MDS]]*1.2)))
            }
            
            # if (input$displayTitleMDS) {
            #   plotMDS <- plotMDS + labs(
            #     title = input$MDSTitle
            #   )
            # }
            
            
            plotMDS <- plotMDS + labs(
              title = input$MDSTitle
            )
            
            themeMDS <- paste0("theme_", input$selectThemeMDS, "()")
            themeMDS <- eval(parse(text = themeMDS))
            
            ### themes, axis labels ,legend etc
            plotMDS <- plotMDS + labs(
              # x = paste0(input$pickFactor1MDS, " (", format(round(tabVarprop[input$pickFactor1MDS] * 100, 1), nsmall = 1), "%)"),
              # y = paste0(input$pickFactor2MDS, " (", format(round(tabVarprop[input$pickFactor2MDS] * 100, 1), nsmall = 1), "%)"),
              color = input$colorGroupMDS, shape = input$shapeGroupMDS
            ) +
              themeMDS +
              theme(
                axis.text.x = element_text(
                  colour = "grey20", size = input$textSizeMDS, angle = 0, hjust = .5,
                  vjust = .5, face = "plain"
                ),
                axis.text.y = element_text(
                  colour = "grey20", size = input$textSizeMDS, angle = 0, hjust = 1,
                  vjust = 0.5, face = "plain"
                ),
                axis.title.x = element_text(
                  colour = "grey20", size = input$textSizeMDS, angle = 0, hjust = .5,
                  vjust = 0, face = "plain"
                ),
                axis.title.y = element_text(
                  colour = "grey20", size = input$textSizeMDS, angle = 90,
                  hjust = .5, vjust = .5, face = "plain"
                ),
                legend.text = element_text(
                  colour = "grey20", size = input$textSizeMDS
                ),
                legend.title = element_text(
                  colour = "grey20", size = input$textSizeMDS
                ),
                title = element_text(colour = "grey20", size = input$textSizeMDS)
              )
            
            plotMDS <- plotMDS +
              ylim(c(min(mdsTable[["C2"]] * 1.1), max(mdsTable[["C2"]] * 1.2))) +
              xlim(c(min(mdsTable[["C1"]] * 1.1), max(mdsTable[["C1"]] * 1.2)))
            
            output$MDSStatic <- renderPlot(
              {
                plotMDS
              },
              width = as.numeric(input$plotWidthMDS),
              height = as.numeric(input$plotHeightMDS)
            )
            
            output$downloadMDS <- downloadHandler(
              filename = function() {
                paste0(Sys.Date(), "_MDS.pdf")
              },
              content = function(file) {
                ggsave(
                  filename = file, plot = plotMDS # ,
                  # width = (as.numeric(input$figWidthMDS) / 3.2),
                  # height = (as.numeric(input$figHeightMDS) / 3.2), limitsize = FALSE,
                  # units = "mm"
                )
              }
            )
            
            # cat("3")
            
            
            # }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")} # close withProgress
            # }) # close tryCatch
          }
        ) # close Event number 2
      }) # close withProgress
    }
  ) # close Event number 1
  # }) # close withProgress
}) # close observe
