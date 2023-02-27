observe({
  # withProgress(message = "Generating t-SNE Plot. Please wait...", {
    distanceMatrixTSNE <- inputDataReactive()$distanceMatrixTSNE
    groupingVariables <- inputDataReactive()$groupingVariables
    datasetScaled <- inputDataReactive()$datasetScaled
    
    datasetTSNE <- datasetScaled
    # factorlevels <- inputDataReactive()$factorlevels
    
    # print(colourList)
    # cat(distanceMatrixTSNE[1:5,1:5])
    
    maxPerplexity <- floor((nrow(distanceMatrixTSNE) / 3) - 1)
    optimalPerplexity <- sqrt(nrow(distanceMatrixTSNE))
    # optimalPerplexity <- round(optimalPerplexity / 5) * 5 # round to nearest 5
    optimalPerplexity <- ceiling(optimalPerplexity) # round up to next integer
    
    updateNumericInput(
      session = session,
      inputId = "perplexityTSNE",
      value = optimalPerplexity,
      min = max(3, optimalPerplexity - 20),
      max = min(maxPerplexity, optimalPerplexity + 30),
      step = 1
    )
    
    gg_fill_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    gvList <- c(colnames(groupingVariables), "-")
    updateSelectInput(session = session, "colorGroupTSNE", choices = colnames(groupingVariables), selected = colnames(groupingVariables)[2]) # try: selected = colnames(groupingVariables)[2] ([1] is sample name)
    updateSelectInput(session = session, "shapeGroupTSNE", choices = gvList, selected = "-")
    
    ### perplexity: numeric; Perplexity parameter (should not be bigger than 3 * perplexity < nrow(X) - 1
    ### theta:	numeric; Speed/accuracy trade-off (increase for less accuracy), set to 0.0 for exact TSNE (default: 0.5)
    
    observeEvent( # Event number 0
      {
        input$colorGroupTSNE
      },
      ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
      ignoreNULL = F, # default = TRUE
      {
        updateSelectizeInput(
          # session = getDefaultReactiveDomain(),
          session = session,
          inputId = "selectizeTSNE",
          choices = as.list(unique(groupingVariables[[input$colorGroupTSNE]])),
          selected = as.list(unique(groupingVariables[[input$colorGroupTSNE]])),
          server = TRUE
          # server = TRUE
        )
      }
    ) # close Event number 0
  
    
    output$colourPanelTSNE <- renderUI({
      levTSNE <- sort(unique(input$selectizeTSNE)) # sorting so that "things" are unambigious
      colsTSNE <- gg_fill_hue(length(levTSNE))
      
      # New IDs "colX1" so that it partly coincide with input$selectizeTSNE...
      lapply(seq_along(levTSNE), function(i) {
        colourInput(inputId = paste0("colTSNE_", levTSNE[i]),
                    label = paste0("Choose colour for ", levTSNE[i]),
                    value = colsTSNE[i]
        )
      })
      
    })
    
    # dimList <- colnames(tsneTable)[-(1:nGrouping)]
    # updateSelectInput(session = session, "pickFactor1TSNE", choices = dimList, selected = "X1")
    # updateSelectInput(session = session, "pickFactor2TSNE", choices = dimList, selected = "X2")
    
    # print(input$paramButtonTSNE)
    # cat(input$paramButtonTSNE)
    
    # input$paramButtonTSNE <- 1
    
    observeEvent( # Event number 1
      {
        input$paramButtonTSNE
        # input$removeSamplesTSNE
        # input$perplexityTSNE
        # input$max_iterTSNE
        # input$thetaTSNE
        # input$etaTSNE
        # input$perplexityTSNE # or precompute?
        # lapply(seq_along(colourList), function (i) {
        #   input[[paste0("GroupColour", i)]]
        # })
        # 
      },
      ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
      ignoreNULL = F, # default = TRUE
      {
        
        withProgress(message = "Generating t-SNE Plot. Please wait...", {
        
        # ---------------------------- compute & prepare t-SNE
        nGrouping <- ncol(groupingVariables)
        
        # if (input$normalizeTSNE) {
        #   distanceMatrixTSNE <- normalize_input(distanceMatrixTSNE)
        # }

        nDimsTSNE <- 2
        # distanceMatrixTSNE <- as.matrix(distanceMatrixTSNE)
        tsneResult <- Rtsne(
          distanceMatrixTSNE,
          # perplexity=as.numeric(input$perplexityTSNE), # default = 30; good range = 5-50
          perplexity = input$perplexityTSNE, # default = 30; good range = 5-50
          initial_dims = input$initialDimsTSNE,
          check_duplicates = FALSE,
          is_distance = TRUE,
          max_iter = input$max_iterTSNE, # default= 1000
          theta = input$thetaTSNE, # default = 0.5; exact t-SNE = 0.0
          eta = input$etaTSNE, # learning rate; default = 200
          TSNE_center = input$tsneCenter,
          TSNE_scale = input$tsneScale,
          dims = nDimsTSNE
        ) # default: dims = 2
        
        tsneTable <- data.frame(groupingVariables, tsneResult$Y, stringsAsFactors = FALSE, row.names = rownames(groupingVariables))
        
        pcaResultsTSNE <- dudi.pca(datasetTSNE, center = T, scale = T, scan = FALSE, nf = 5)

        nPCTSNE <- pcaResultsTSNE$nf # number of (> 0) principal components
        nGrouping <- ncol(groupingVariables)
        pcaVarpropTSNE <- pcaResultsTSNE$eig
        
        pcaVarpropTSNE <- tibble(PC = paste0("PC", factor(1:length(pcaVarpropTSNE))), variance = pcaVarpropTSNE) %>% 
          mutate(pct = format(variance/sum(variance)*100, digits = 2)) %>%
          # mutate(pct = variance/sum(variance)*100) %>% 
          mutate(pct_cum = cumsum(pct))
        pcaVarpropTSNE$PC <- factor(pcaVarpropTSNE$PC, levels = pcaVarpropTSNE$PC)
        
        
        pcaTableTSNE <- data.frame(groupingVariables, pcaResultsTSNE$li, stringsAsFactors = FALSE, row.names = rownames(datasetTSNE))
        
        tabVarpropTSNE <- pcaVarpropTSNE
        for (i in 1:nPCTSNE) {
          colnames(pcaTableTSNE)[i + nGrouping] <- paste0("PC", i)
        }
        
        # N_perm <- 10
        # expl_var_perm <- matrix(NA, ncol = length(pcaResultsTSNE$sdev), nrow = N_perm)
        # for(k in 1:N_perm)
        # {
        #   expr_perm <- apply(expr,2,sample)
        #   PC_perm <- prcomp(t(log10(expr_perm+1)), center=TRUE, scale=FALSE)
        #   expl_var_perm[k,] <- PC_perm$sdev^2/sum(PC_perm$sdev^2)
        # }
        # plot(expl_var[1:50]~seq(1:50), ylab="EXPLAINED VARIANCE",
        #      col="darkgreen", type='o', xlab="PRINCIPAL COMPONENTS")
        # lines(colMeans(expl_var_perm)[1:50]~seq(1:50),col="red")
        # legend("topright", c("Explained by PCS", "Explained by chance"),
        #        fill=c("darkgreen","red"), inset=0.02)
        # 
        # pval <- apply(t(expl_var_perm) >= expl_var,1,sum) / N_perm
        # plot(pval[1:50]~seq(1:50),col="darkred",type='o',
        #      xlab="PRINCIPAL COMPONENTS",ylab="PVALUE")
        # optPC<-head(which(pval>=0.05),1)-1
        # mtext(paste0("OPTIMAL NUMBER OF PRINCIPAL COMPONENTS = ", optPC))


        observeEvent(
          { # Event number 2: only need axis inputs here, for the others no need for redrawing plot (?)
            input$sampleLabelsTSNE
            # input$pickFactor1TSNE
            # input$pickFactor2TSNE
            req(input$colorGroupTSNE)
            input$shapeGroupTSNE
            input$plotWidthTSNE
            input$plotHeightTSNE
            input$textSizeTSNE
            input$pointSizeTSNE
            input$displayButtonTSNE
            input$selectThemeTSNE
            
            # input$displayTitleTSNE
            input$tsneTitle

          },
          ignoreInit = F, # default = FALSE
          ignoreNULL = F, # default = TRUE
          {

            # if (length(input$selectizeTSNE)) {
            #   # print(input$selectizeTSNE)
            #   colsTSNETSNE <- paste0("c(", paste0("input$colTSNE_", sort(input$selectizeTSNE), collapse = ", "), ")")
            #   # print(colsTSNETSNE)
            #   colsTSNETSNE <- eval(parse(text = colsTSNETSNE))
            #   # print(colsTSNETSNE)
            # } else {
            #   levTSNE <- sort(unique(groupingVariables[[input$colorGroupTSNE]]))
            #   colsTSNETSNE <- gg_fill_hue(length(levTSNE))
            #   # print(colsTSNETSNE)
            # }
            
            colsTSNE <- paste0("c(", paste0("input$colTSNE_", sort(input$selectizeTSNE), collapse = ", "), ")")
            # print(colsTSNE)
            colsTSNE <- eval(parse(text = colsTSNE))
            # print(colsTSNE)
            
            if (is.null(colsTSNE)) {
              colsTSNE <- gg_fill_hue(length(sort(unique(groupingVariables[[input$colorGroupTSNE]]))))
            }

            # To prevent errors
            # req(length(colsTSNE) == length(input$selectizeTSNE))
            
            if (input$shapeGroupTSNE == "-") {
              plotTSNE <- tsneTable %>%
                # ggplot(aes(x = .data[[input$pickFactor1TSNE]], y = .data[[input$pickFactor2TSNE]], color = .data[[input$colorGroupTSNE]], fill = .data[[input$colorGroupTSNE]])) +
                ggplot(aes(x = .data[["X1"]], y = .data[["X2"]], color = .data[[input$colorGroupTSNE]], fill = .data[[input$colorGroupTSNE]])) +
                geom_point(size = as.numeric(input$pointSizeTSNE)) +
                scale_color_manual(values = colsTSNE)
                # geom_point(size = as.numeric(input$pointSizeTSNE))
            } else {
              plotTSNE <- tsneTable %>%
                # ggplot(aes(x = .data[[input$pickFactor1TSNE]], y = .data[[input$pickFactor2TSNE]], color = .data[[input$colorGroupTSNE]], fill = .data[[input$colorGroupTSNE]], shape = .data[[input$shapeGroupTSNE]])) +
                ggplot(aes(x = .data[["X1"]], y = .data[["X2"]], color = .data[[input$colorGroupTSNE]], fill = .data[[input$colorGroupTSNE]], shape = .data[[input$shapeGroupTSNE]])) +
                geom_point(size = as.numeric(input$pointSizeTSNE)) +
                scale_color_manual(values = colsTSNE) +
                scale_shape_manual(values = c(rep(c(21, 22, 23, 24, 25, 8, 3, 4), times = 10))[1:nlevels(as.factor(tsneTable[[input$shapeGroupTSNE]]))])
              ### Note: 21-25 are simple shapes, but with filling; 8,3,4 are crosses with different amount of lines
            }

            if (input$sampleLabelsTSNE) {
              plotTSNE <- plotTSNE +
                geom_text(
                  aes(label = rownames(tsneTable)),
                  size = (input$textSizeTSNE / 3),
                  hjust = 0.2, vjust = -1.5, check_overlap = T,
                  show.legend = FALSE
                ) # +
              # ylim(c(min(TSNETable[[input$pickFactor2TSNE]]*1.1), max(TSNETable[[input$pickFactor2TSNE]]*1.2))) +
              # xlim(c(min(TSNETable[[input$pickFactor1TSNE]]*1.1), max(TSNETable[[input$pickFactor1TSNE]]*1.2)))
            }
            
            # if (input$displayTitleTSNE) {
            #   plotTSNE <- plotTSNE + labs(
            #     title = input$tsneTitle
            #   )
            # }
            

            plotTSNE <- plotTSNE + labs(
              title = input$tsneTitle
            )

            themeTSNE <- paste0("theme_", input$selectThemeTSNE, "()")
            themeTSNE <- eval(parse(text = themeTSNE))
            
            ### themes, axis labels ,legend etc
            plotTSNE <- plotTSNE + labs(
              # x = paste0(input$pickFactor1TSNE, " (", format(round(tabVarprop[input$pickFactor1TSNE] * 100, 1), nsmall = 1), "%)"),
              # y = paste0(input$pickFactor2TSNE, " (", format(round(tabVarprop[input$pickFactor2TSNE] * 100, 1), nsmall = 1), "%)"),
              color = input$colorGroupTSNE, shape = input$shapeGroupTSNE
            ) +
              themeTSNE +
              theme(
                axis.text.x = element_text(
                  colour = "grey20", size = input$textSizeTSNE, angle = 0, hjust = .5,
                  vjust = .5, face = "plain"
                ),
                axis.text.y = element_text(
                  colour = "grey20", size = input$textSizeTSNE, angle = 0, hjust = 1,
                  vjust = 0.5, face = "plain"
                ),
                axis.title.x = element_text(
                  colour = "grey20", size = input$textSizeTSNE, angle = 0, hjust = .5,
                  vjust = 0, face = "plain"
                ),
                axis.title.y = element_text(
                  colour = "grey20", size = input$textSizeTSNE, angle = 90,
                  hjust = .5, vjust = .5, face = "plain"
                ),
                legend.text = element_text(
                  colour = "grey20", size = input$textSizeTSNE
                ),
                legend.title = element_text(
                  colour = "grey20", size = input$textSizeTSNE
                ),
                title = element_text(colour = "grey20", size = input$textSizeTSNE)
              )
            
            plotTSNE <- plotTSNE +
              ylim(c(min(tsneTable[["X2"]] * 1.1), max(tsneTable[["X2"]] * 1.2))) +
              xlim(c(min(tsneTable[["X1"]] * 1.1), max(tsneTable[["X1"]] * 1.2)))
            
            output$tsneStatic <- output$tsneStatic2 <-  renderPlot(
              {
                plotTSNE
              },
              width = as.numeric(input$plotWidthTSNE),
              height = as.numeric(input$plotHeightTSNE)
            )
            
            output$tsneScree <- renderPlot({
              tabVarprop3 <- tabVarpropTSNE
              tabVarprop3$PC <- gsub("PC", "", tabVarprop3$PC)
              tabVarprop3$PC <- factor(tabVarprop3$PC, levels = tabVarprop3$PC)
              tabVarprop3$variance <- as.numeric(tabVarprop3$variance)
              tabVarprop3$pct <- as.numeric(tabVarprop3$pct)
              tabVarprop3$pct_cum <- as.numeric(tabVarprop3$pct_cum)

              if (nrow(tabVarprop3) > 100) {
                tabVarprop3 <- tabVarprop3[1:100,]
              }

              TSNEScree <- tabVarprop3 %>%
                ggplot(aes(x = PC)) +
                geom_col(aes(y = pct)) +
                geom_line(aes(y = pct_cum, group = 1)) +
                geom_point(aes(y = pct_cum)) +
                geom_hline(yintercept = 95, colour = "red") +
                geom_text(aes(0, 95, label = "95% cumulative variance", vjust = -1, hjust = -0.5, col = "red"), show.legend = F) +
                labs(x = "Principal component", y = "Fraction variance explained (%)") +
                scale_y_continuous(n.breaks = 20, limits = c(0, 100), expand = expansion(mult = 0, add = 0)) +
                theme_classic(base_size = as.numeric(input$textSizeTSNE))
              TSNEScree

            }
            )
            
            # output$tsneStatic2 <- renderPlot(
            #   {
            #     plotTSNE
            #   },
            #   width = "auto",
            #   height = "auto",
            # )
            
            output$downloadTSNE <- downloadHandler(
              filename = function() {
                paste0(Sys.Date(), "_tSNE.pdf")
              },
              content = function(file) {
                ggsave(
                  filename = file, plot = plotTSNE # ,
                  # width = (as.numeric(input$figWidthTSNE) / 3.2),
                  # height = (as.numeric(input$figHeightTSNE) / 3.2), limitsize = FALSE,
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
