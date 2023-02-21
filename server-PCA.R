observe({
  # withProgress(message = "Generating PCA Plots. Please wait...", {
    # vcfRaw <- inputDataReactive()$vcfRaw
    # dataset <- inputDataReactive()$dataset
    datasetScaled <- inputDataReactive()$datasetScaled
    groupingVariables <- inputDataReactive()$groupingVariables

    # groupingVarsCheckbox <- cbind(groupingVariables, bool = T)
    
    datasetPCA <- datasetScaled
    
    # datasetPCA <- datasetScaled

    output$sampleTablePCA <- DT::renderDataTable(
      groupingVariables,
      server = TRUE,
      rownames = FALSE,
      filter = "bottom"
    )
    
    # print the selected indices
    output$selectedSamplesPCA <- renderPrint({
      s = input$sampleTablePCA_rows_selected
      if (length(s)) {
        cat('These samples are currently selected:\n\n')
        cat(rownames(datasetPCA)[s], sep = '\n')
      }
    })
    
    gg_fill_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    gvList <- c(colnames(groupingVariables), "-")
    updateSelectInput(session = session, "colorGroupPCA", choices = colnames(groupingVariables), selected = colnames(groupingVariables)[2]) # try: selected = colnames(groupingVariables)[2] ([1] is sample name)
    updateSelectInput(session = session, "shapeGroupPCA", choices = gvList, selected = "-")
    
    pcList <- paste0("PC", c(1:5))
    updateSelectInput(session = session, "pickFactor1PCA", choices = pcList, selected = "PC1")
    updateSelectInput(session = session, "pickFactor2PCA", choices = pcList, selected = "PC2")
    
    observeEvent( # Event number 0
      {
        input$colorGroupPCA
      },
      ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
      ignoreNULL = F, # default = TRUE
      # priority = 1,
      {
        updateSelectizeInput(
          # session = getDefaultReactiveDomain(),
          session = session,
          inputId = "selectizePCA",
          choices = as.list(unique(groupingVariables[[input$colorGroupPCA]])),
          selected = as.list(unique(groupingVariables[[input$colorGroupPCA]])),
          server = TRUE
          # server = TRUE
        )
      }
    ) # close Event number 0
    
    output$colourPanelPCA <- renderUI({
      levPCA <- sort(unique(input$selectizePCA)) # sorting so that "things" are unambigious
      colsPCA <- gg_fill_hue(length(levPCA))
      
      # New IDs "colX1" so that it partly coincide with input$selectizePCA...
      lapply(seq_along(levPCA), function(i) {
        colourInput(inputId = paste0("colPCA_", levPCA[i]),
                    label = paste0("Choose colour for ", levPCA[i]),
                    value = colsPCA[i]
        )
      })
    })
    
    
    observeEvent( # Event number 1
      {
        input$removeSamplesPCA
        input$resetSelectionPCA
        input$paramButtonPCA
        
      },
      ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
      ignoreNULL = F, # default = TRUE
      {
        withProgress(message = "Generating PCA Plots. Please wait...", {
          
        removeRowsPCA <- input$sampleTablePCA_rows_selected
        if (length(removeRowsPCA)) {
          s <- c(removeRowsPCA)
          datasetPCA <- dataset[-s, ]
          groupingVariables <- groupingVariables[-s, ]
        }
        
        ##### compute PCA
        pcaResults <- dudi.pca(datasetPCA, center = input$pcaCenter, scale = input$pcaScale, scan = FALSE, nf = 5)
        
        
        nPC <- pcaResults$nf # number of (> 0) principal components
        nGrouping <- ncol(groupingVariables)
        pcaVarprop <- pcaResults$eig
        
        pcaVarprop <- tibble(PC = paste0("PC", factor(1:length(pcaVarprop))), variance = pcaVarprop) %>% 
          mutate(pct = format(variance/sum(variance)*100, digits = 2)) %>%
          # mutate(pct = variance/sum(variance)*100) %>% 
          mutate(pct_cum = cumsum(pct))
        pcaVarprop$PC <- factor(pcaVarprop$PC, levels = pcaVarprop$PC)
        
        
        pcaTable <- data.frame(groupingVariables, pcaResults$li, stringsAsFactors = FALSE, row.names = rownames(datasetPCA))
        
        tabVarprop <- pcaVarprop
        for (i in 1:nPC) {
          colnames(pcaTable)[i + nGrouping] <- paste0("PC", i)
        }
        
        # Get the gene loadings (in dudi.pca: $c1)
        pc_loadings <- pcaResults$c1
        colnames(pc_loadings) <- c("PC1", "PC2") 
        # pc_loadings <- pc_loadings %>% 
        #   as_tibble(rownames = "gene")
        pc_loadings$gene <- rownames(pc_loadings)
        top_genes <- pc_loadings %>% 
          select(gene, PC1, PC2) %>%
          pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
          group_by(PC) %>% 
          arrange(desc(abs(loading)))
        
        ##### end of PCA computation/prep
        
        
        observeEvent(
          { # Event number 2: only need axis inputs here, for the others no need for redrawing plot (?)
            input$sampleLabelsPCA
            input$pickFactor1PCA
            input$pickFactor2PCA
            req(input$colorGroupPCA)
            input$shapeGroupPCA
            input$plotWidthPCA
            input$plotHeightPCA
            input$textSizePCA
            input$pointSizePCA
            # input$displayTitlePCA
            input$displayButtonPCA
            input$pcaTitle
            input$selectThemePCA
            # input$pcaBrush
            # input$excludeSamplesPCA
          },
          ignoreInit = F, # default = FALSE
          ignoreNULL = F, # default = TRUE
          # priority = 0,
          {
            
            colsPCA <- paste0("c(", paste0("input$colPCA_", sort(input$selectizePCA), collapse = ", "), ")")
            # colsPCA <- paste0("c(", paste0("input$colPCA_", sort(unique(groupingVariables[[input$colorGroupPCA]])), collapse = ", "), ")")
            # print(colsPCA)
            colsPCA <- eval(parse(text = colsPCA))
            # colsPCA <- get(colsPCA)
            # print(colsPCA)
            
            if (is.null(colsPCA)) {
              colsPCA <- gg_fill_hue(length(sort(unique(groupingVariables[[input$colorGroupPCA]]))))
            }
            
            # req(length(colsPCA) == length(input$selectizePCA))
            
            if (input$shapeGroupPCA == "-") {
              plotPCA <- pcaTable %>%
                ggplot(aes(x = .data[[input$pickFactor1PCA]], y = .data[[input$pickFactor2PCA]], color = .data[[input$colorGroupPCA]], fill = .data[[input$colorGroupPCA]])) +
                geom_point(size = as.numeric(input$pointSizePCA)) +
                scale_color_manual(values = colsPCA)
              # scale_fill_manual(values = colsPCA)
            } else {
              plotPCA <- pcaTable %>%
                ggplot(aes(x = .data[[input$pickFactor1PCA]], y = .data[[input$pickFactor2PCA]], color = .data[[input$colorGroupPCA]], fill = .data[[input$colorGroupPCA]], shape = .data[[input$shapeGroupPCA]])) +
                geom_point(size = as.numeric(input$pointSizePCA)) +
                scale_color_manual(values = colsPCA) +
                scale_shape_manual(values = c(rep(c(21, 22, 23, 24, 25, 8, 3, 4), times = 10))[1:nlevels(as.factor(pcaTable[[input$shapeGroupPCA]]))])
              ### Note: 21-25 are simple shapes, but with filling; 8,3,4 are crosses with different amount of lines
            }
            
            if (isTRUE(input$pcaAxesProp)) {
              plotPCA <- plotPCA + coord_fixed(ratio = as.numeric(tabVarprop$pct[tabVarprop$PC == input$pickFactor2PCA])/as.numeric(tabVarprop$pct[tabVarprop$PC == input$pickFactor1PCA]))
            }
            
            
            if (input$sampleLabelsPCA) {
              plotPCA <- plotPCA +
                geom_text(
                  aes(label = rownames(pcaTable)),
                  size = (input$textSizePCA / 3),
                  hjust = 0.2, vjust = -1.5, check_overlap = T,
                  show.legend = FALSE
                ) 
            }
            
            plotPCA <- plotPCA + labs(
              title = input$pcaTitle
            )
            
            themePCA <- paste0("theme_", input$selectThemePCA, "()")
            themePCA <- eval(parse(text = themePCA))
            
            ### themes, axis labels ,legend etc
            plotPCA <- plotPCA + labs(
              x = paste0(input$pickFactor1PCA, " (", tabVarprop$pct[tabVarprop$PC == input$pickFactor1PCA], "% variance explained)"),
              y = paste0(input$pickFactor2PCA, " (", tabVarprop$pct[tabVarprop$PC == input$pickFactor2PCA], "% variance explained)"),
              color = input$colorGroupPCA, shape = input$shapeGroupPCA
            ) +
              themePCA +
              theme(
                axis.text.x = element_text(
                  colour = "grey20", size = input$textSizePCA, angle = 0, hjust = .5,
                  vjust = .5, face = "plain"
                ),
                axis.text.y = element_text(
                  colour = "grey20", size = input$textSizePCA, angle = 0, hjust = 1,
                  vjust = 0.5, face = "plain"
                ),
                axis.title.x = element_text(
                  colour = "grey20", size = input$textSizePCA, angle = 0, hjust = .5,
                  vjust = 0, face = "plain"
                ),
                axis.title.y = element_text(
                  colour = "grey20", size = input$textSizePCA, angle = 90,
                  hjust = .5, vjust = .5, face = "plain"
                ),
                legend.text = element_text(
                  colour = "grey20", size = input$textSizePCA
                ),
                legend.title = element_text(
                  colour = "grey20", size = input$textSizePCA
                ),
                title = element_text(colour = "grey20", size = input$textSizePCA)
              )
            
            
            plotPCA <- plotPCA +
              ylim(c(min(pcaTable[[input$pickFactor2PCA]] * 1.1), max(pcaTable[[input$pickFactor2PCA]] * 1.2))) +
              xlim(c(min(pcaTable[[input$pickFactor1PCA]] * 1.1), max(pcaTable[[input$pickFactor1PCA]] * 1.2)))
            
            output$pcaStatic <- output$pcaStatic2  <- renderPlot(
              {
                plotPCA
              },
              width = as.numeric(input$plotWidthPCA),
              height = as.numeric(input$plotHeightPCA)
              # , res = 96
            )
            
            # output$pcaStatic2 <- renderPlot(
            #   {
            #     plotPCA
            #   },
            #   width = "auto",
            #   height = "auto",
            # )

            # create id for data
            
            # selectedList <<- append(selectedList, which(brushedPointsPCA$selected_))
            
            # datasetPCA$selected_ <<- brushedPointsPCA$selected_
            
            # print("down")
            # print(datasetPCA$selected_)
            
            # cat(datasetPCA$selected_)
            # brushedPointsID <- which(brushedPointsPCA$selected_ == FALSE)
            
            # output$brushInfo <- renderTable({
            #   # brushedPoints(df = pcaTable, brush = input$pcaBrush, xvar = input$pickFactor1PCA, yvar = input$pickFactor2PCA)[, c(colnames(groupingVariables))]
            #   rownames(brushedPointsPCA[brushedPointsPCA$selected_ == TRUE, ])
            # },
            # rownames = F,
            # colnames = F
            # )
            
            # brushedPointsReactivePCA <- reactive(brushedPointsPCA)
            
            # output$brush_info <- renderTable({
            #   brushedPoints(df = pcaTable, brush = input$pcaBrush, xvar = input$pickFactor1PCA, yvar = input$pickFactor2PCA)[, c(colnames(groupingVariables), input$pickFactor1PCA, input$pickFactor2PCA)]
            # })
            
            output$pcaScree <- renderPlot({
              tabVarprop2 <- tabVarprop
              tabVarprop2$PC <- gsub("PC", "", tabVarprop2$PC)
              tabVarprop2$PC <- factor(tabVarprop2$PC, levels = tabVarprop2$PC)
              tabVarprop2$variance <- as.numeric(tabVarprop2$variance)
              tabVarprop2$pct <- as.numeric(tabVarprop2$pct)
              tabVarprop2$pct_cum <- as.numeric(tabVarprop2$pct_cum)
              
              if (nrow(tabVarprop2) > 10) {
                tabVarprop2 <- tabVarprop2[1:10,]
              }
              pcaScree <- tabVarprop2 %>%
                ggplot(aes(x = PC)) +
                geom_col(aes(y = pct)) +
                geom_line(aes(y = pct_cum, group = 1)) +
                geom_point(aes(y = pct_cum)) +
                labs(x = "Principal component", y = "Fraction variance explained (%)") +
                scale_y_continuous(n.breaks = 20) +
                theme_classic(base_size = as.numeric(input$textSizePCA))
              pcaScree
              
            } 
            # , width = 500, height = 400
            )
            
            output$downloadPCA <- downloadHandler(
              filename = function() {
                paste0(Sys.Date(), "_PCA.pdf")
              },
              content = function(file) {
                ggsave(
                  filename = file, plot = plotPCA # ,
                  # width = (as.numeric(input$figWidthPCA) / 3.2),
                  # height = (as.numeric(input$figHeightPCA) / 3.2), limitsize = FALSE,
                  # units = "mm"
                )
              }
            )
            
            output$pcaLoadings <- DT::renderDataTable({
              datatable(top_genes, rownames = F) %>% formatRound("loading", digits = 3)
            })
            
            
            
            
            # }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")} # close withProgress
            # }) # close tryCatch
          }
        ) # close Event number 2
      }) # close withProgress
      }
    ) # close Event number 1
  # }) # close withProgress
}) # close observe
