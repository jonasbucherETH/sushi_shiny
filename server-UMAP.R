observe({
  withProgress(message = "Generating UMAP Plot. Please wait...", {
    dataset <- inputDataReactive()$dataset
    groupingVariables <- inputDataReactive()$groupingVariables
    datasetScaled <- inputDataReactive()$datasetScaled
    
    datasetUMAP <- datasetScaled
    
    nGrouping <- ncol(groupingVariables)
    gvList <- c(colnames(groupingVariables)[1:nGrouping], "-")
    updateSelectInput(session = session, "colorGroupUMAP", choices = colnames(groupingVariables), selected = colnames(groupingVariables)[2]) # try: selected = colnames(groupingVariables)[2] ([1] is sample name)
    updateSelectInput(session = session, "shapeGroupUMAP", choices = gvList, selected = "-")
    
    gg_fill_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    observeEvent( # Event number 0
      {
        input$colorGroupUMAP
      },
      ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
      ignoreNULL = F, # default = TRUE
      {
        updateSelectizeInput(
          # session = getDefaultReactiveDomain(),
          session = session,
          inputId = "selectizeUMAP",
          choices = as.list(unique(groupingVariables[[input$colorGroupUMAP]])),
          selected = as.list(unique(groupingVariables[[input$colorGroupUMAP]])),
          server = TRUE
          # server = TRUE
        )
      }
    ) # close Event number 0
    
    
    output$colourPanelUMAP <- renderUI({
      levUMAP <- sort(unique(input$selectizeUMAP)) # sorting so that "things" are unambigious
      colsUMAP <- gg_fill_hue(length(levUMAP))
      
      # New IDs "colX1" so that it partly coincide with input$selectizeUMAP...
      lapply(seq_along(levUMAP), function(i) {
        colourInput(inputId = paste0("colUMAP_", levUMAP[i]),
                    label = paste0("Choose colour for ", levUMAP[i]),
                    value = colsUMAP[i]
        )
      })
      
    })

    observeEvent( # Event number 1
      {
        input$excludeSamplesUMAP
        input$paramButtonUMAP
        # input$n_neighborsUMAP 
        # input$distanceMetricUMAP
        # input$sampleLabelsUMAP
        # input$pickFactor1UMAP
        # input$pickFactor2UMAP
        # input$colorGroupUMAP
        # input$shapeGroupUMAP
        # input$textSizeUMAP
        # input$pointSizeUMAP
        # input$displayTitleUMAP

      },
      ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
      ignoreNULL = F, # default = TRUE
      {
        
        umapResult <- umap(
          X = datasetUMAP,
          n_neighbors = input$n_neighborsUMAP,
          n_components = 2,
          metric = input$distanceMetricUMAP
        ) # returns matrix
        
        UMAPTable <- data.frame(groupingVariables, umapResult, stringsAsFactors = FALSE, row.names = rownames(groupingVariables))
        
        observeEvent( # Event number 2
          {
            input$sampleLabelsUMAP
            # input$pickFactor1UMAP
            # input$pickFactor2UMAP
            req(input$colorGroupUMAP)
            input$shapeGroupUMAP
            input$textSizeUMAP
            input$pointSizeUMAP
            input$plotWidthUMAP
            input$plotHeightUMAP
            # input$displayTitleUMAP
            input$displayButtonUMAP
            input$umapTitle
            input$selectThemeUMAP
            
          },
            ignoreInit = F, # default = FALSE
            ignoreNULL = F, # default = TRUE
          {
            
            colsUMAP <- paste0("c(", paste0("input$colUMAP_", sort(input$selectizeUMAP), collapse = ", "), ")")
            # print(colsUMAP)
            colsUMAP <- eval(parse(text = colsUMAP))
            # print(colsUMAP)
            
            if (is.null(colsUMAP)) {
              colsUMAP <- gg_fill_hue(length(sort(unique(groupingVariables[[input$colorGroupUMAP]]))))
            }
        
            if (input$shapeGroupUMAP == "-") {
              plotUMAP <- UMAPTable %>%
                ggplot(aes(x = X1, y = X2, color = .data[[input$colorGroupUMAP]], fill = .data[[input$colorGroupUMAP]])) +
                geom_point(size = as.numeric(input$pointSizeUMAP)) +
                scale_color_manual(values = colsUMAP)
              # geom_point(size = as.numeric(input$pointSizeUMAP))
            } else {
              plotUMAP <- UMAPTable %>%
                ggplot(aes(x = X1, y = X2, color = .data[[input$colorGroupUMAP]], fill = .data[[input$colorGroupUMAP]], shape = .data[[input$shapeGroupUMAP]])) +
                geom_point(size = as.numeric(input$pointSizeUMAP)) +
                scale_color_manual(values = colsUMAP) +
                scale_shape_manual(values = c(rep(c(21, 22, 23, 24, 25, 8, 3, 4), times = 10))[1:nlevels(as.factor(UMAPTable[[input$shapeGroupUMAP]]))])
              ### Note: 21-25 are simple shapes, but with filling; 8,3,4 are crosses with different amount of lines
            }
            
            if (input$sampleLabelsUMAP) {
              plotUMAP <- plotUMAP +
                geom_text(
                  aes(label = rownames(UMAPTable)),
                  size = (input$textSizeUMAP / 3),
                  hjust = 0.2, vjust = -1.5, check_overlap = T,
                  show.legend = FALSE
                ) 
            }
            
            plotUMAP <- plotUMAP + labs(
              title = input$umapTitle
            )
            
            themeUMAP <- paste0("theme_", input$selectThemeUMAP, "()")
            themeUMAP <- eval(parse(text = themeUMAP))
            
            ### themes, axis labels ,legend etc
            plotUMAP <- plotUMAP + labs(
              x = "X1",
              y = "X2",
              color = input$colorGroupUMAP, shape = input$shapeGroupUMAP
            ) +
              themeUMAP +
              theme(
                axis.text.x = element_text(
                  colour = "grey20", size = input$textSizeUMAP, angle = 0, hjust = .5,
                  vjust = .5, face = "plain"
                ),
                axis.text.y = element_text(
                  colour = "grey20", size = input$textSizeUMAP, angle = 0, hjust = 1,
                  vjust = 0.5, face = "plain"
                ),
                axis.title.x = element_text(
                  colour = "grey20", size = input$textSizeUMAP, angle = 0, hjust = .5,
                  vjust = 0, face = "plain"
                ),
                axis.title.y = element_text(
                  colour = "grey20", size = input$textSizeUMAP, angle = 90,
                  hjust = .5, vjust = .5, face = "plain"
                ),
                legend.text = element_text(
                  colour = "grey20", size = input$textSizeUMAP
                ),
                legend.title = element_text(
                  colour = "grey20", size = input$textSizeUMAP
                ),
                title = element_text(colour = "grey20", size = input$textSizeUMAP)
              )
            
            plotUMAP <- plotUMAP +
              ylim(c(min(UMAPTable[["X2"]] * 1.1), max(UMAPTable[["X2"]] * 1.2))) +
              xlim(c(min(UMAPTable[["X1"]] * 1.1), max(UMAPTable[["X1"]] * 1.2)))
            
            output$UMAPStatic <- renderPlot(
              {
                plotUMAP
              },
              width = as.numeric(input$plotWidthUMAP),
              height = as.numeric(input$plotHeightUMAP)
            )
          } # close Event number 2
        ) # close Event number 2
        
      } # close Event number 1
    ) # close Event number 1
    
  }) # close withProgress
}) # close observe
