tabItem(
  tabName = "tab-UMAP",
  fluidRow( ### NOTE: 1 row has width = 12
    
    column(
      width = 2, 
      box(
        title = "Parameters",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        # inputs
        
        
        numericInput(
          inputId = "n_neighborsUMAP",
          label = "The size of local neighborhood 
          used for manifold approximation",
          min = 2, max = 100,
          value = 15, step = 1
        ),
        
        selectInput(
          inputId = "distanceMetricUMAP",
          label = "Type of distance metric to use to find nearest neighbors",
          choices = c("euclidean","cosine","manhattan",
                      "hamming","correlation","categorical")
        ),
        
        br(), br(),
        actionBttn(
          inputId = "paramButtonUMAP",
          label = "Apply parameters",
          # icon = icon("check"),
          style = "simple",
          color = "primary",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        ),

        # selectInput(
        #   inputId = "excludeSamplesUMAP",
        #   label = "Select samples to exclude",
        #   choices = "",
        #   selected = ""
        # 
        # ),
      ) # close box 
    ), # close  column
    
    column(
      width = 7,
      box(
        title = "UMAP Plots",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        downloadButton(
          outputId = "downloadUMAP",
          label = "Download UMAP Plot (PDF)"
        ),
        br(), br(),
        plotOutput(
          outputId = "UMAPStatic",
          height = "80vh",
          width = "100%",
          inline = F
        )
      ) # close box 
    ), # close plot(s) column
    
    column(
      width = 3,
      box(
        title = "Display options",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        textInput(
          inputId = "umapTitle",
          label = "Title of plot",
          value = ""
        ),
        checkboxInput(
          inputId = "sampleLabelsUMAP",
          label = "Display sample labels",
          value = TRUE
        ),
        
        ### no choices, selected = "" as default
        selectInput(
          inputId = "colorGroupUMAP",
          label = "Select groups to color by",
          choices = "",
          selected = ""
        ),
        selectInput(
          inputId = "shapeGroupUMAP",
          label = "Select groups for shapes",
          choices = "",
          selected = ""
        ),
        # selectInput(
        #   inputId = "pickFactor1UMAP",
        #   label = "Select Dimension for x-axis",
        #   choices = "X1",
        #   selected = "X1"
        # ),
        # selectInput(
        #   inputId = "pickFactor2UMAP",
        #   label = "Select Dimension for y-axis",
        #   choices = "X2",
        #   selected = "X2"
        # ),
        
        # sliderInput("UMAPPlotWidth", "Width of plot", min = 100, max = 2000, value = 800, step = 10),
        # sliderInput("UMAPPlotHeight", "Height of plot", min = 100, max = 2000, value = 600, step = 10),
        
        fluidRow(
          column(
            numericInput(
              inputId = "pointSizeUMAP",
              label = "Point size", min = 1, max = 6,
              value = 3, step = 0.5,
              # width = "100px"
            ),
            width = 6
          ),
          column(
            numericInput(
              inputId = "textSizeUMAP",
              label = "Font Size", min = 4, max = 30,
              value = 12, step = 0.5,
              # width = "100px"
            ),
            width = 6
          )
        ),
        
      ), # close box
    # ), # close column
    
      box(
        title = "Custom colours",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        actionBttn(
          inputId = "displayButtonUMAP",
          label = "Apply chosen colours",
          icon = icon("text-size"),
          style = "simple",
          color = "primary",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        ),
        br(), br(),
        # actionButton("reset", "Default colours", icon = icon("undo"))
        
        #### ----------------------------
        selectizeInput(
          inputId = "selectizeUMAP",
          label = "Select:",
          choices = "",
          selected = "",
          multiple = TRUE
        ),
        
        # selectizeInput("select", "Select:", 
        #                choices = as.list(unique(groupingVariables$Population)), 
        #                # selected = "X1", 
        #                multiple = TRUE),
        
        uiOutput('colourPanelUMAP'),
      )
    )
  ) # close fluidRow
) # close tabItem
