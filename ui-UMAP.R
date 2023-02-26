tabItem(
  tabName = "tab-UMAP",
  fluidRow( ### NOTE: 1 row has width = 12
    
    column(
      width = 2, 
      box(
        title = "Info",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        tags$p(
          "Uniform manifold approximation and projection (UMAP) is a novel
          alternative machine learning algorithm to t-SNE, with the benefits
          of improved runtime and the potential to highlight local and subtle
          structures of the data. The output is based on a graph topology and
          highly dependent on the chosen parameters, particularly on the number
          of neighbours considered and the minimum distance between points.

          Note that the distances between points and clusters in the plot
          cannot be interpreted as being proportional to the distance in the
          high-dimensional input data."
        ),
        tags$p("Parameters"),
        tags$ul(
          tags$li("Minimum distance: The effective minimum distance between
          embedded points: Small values will result in tight clusters,
          larger values will result in a more even dispersal of points."),
          tags$li("Number of neighbours: With larger values more neighbours are
          considered for each point, which result in more global views, while
          smaller values result in more local structures being preserved."), 
          tags$li("With the exact method (fnn) for finding nearest neighbors,
               it is only possible to use euclidean as a distance metric
               to find nearest neighbors"), 
        )
      ),
      box(
        title = "Parameters",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        numericInput(
          inputId = "n_neighborsUMAP",
          label = "Number of neighbours",
          min = 2, max = 100,
          value = 15, step = 1
        ),
        numericInput(
          inputId = "minDistUMAP",
          label = "Minimum distance",
          min = 0.01, max = 1,
          value = 0.01, step = 0.01
        ),
        tags$b("Method for finding nearest neighbors"),
        selectInput(
          inputId = "nnMethodUMAP",
          label = "By default, for n < 4'096 the exact nearest neighbors are found",
          choices = c("exact (fnn)", "approximate (annoy)"),
          selected = "exact"
        ),
        tags$b("Type of distance metric to use to find nearest neighbors"),
        selectInput(
          inputId = "distanceMetricUMAP",
          label = "",
          choices = c("euclidean","cosine","manhattan",
                      "hamming","correlation","categorical")
        ),
        checkboxInput(
          inputId = "scaleUMAP",
          label = "Scale the data before running UMAP?",
          value = FALSE
        ),
        br(), br(),
        actionBttn(
          inputId = "paramButtonUMAP",
          label = "Apply parameters and recalculate",
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
        title = "Uniform Manifold Approximation and Projection",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        downloadBttn(
          outputId = "downloadUMAP",
          label = "Download UMAP Plot (PDF)",
          style = "unite",
          color = "primary",
          size = "sm",
          block = FALSE,
          no_outline = TRUE,
          icon = shiny::icon("download")
        ),
        # downloadButton(
        #   outputId = "downloadUMAP",
        #   label = "Download UMAP Plot (PDF)"
        # ),
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
        selectInput(
          inputId = "selectThemeUMAP",
          label = "Select theme",
          choices = c("bw", "light", "minimal", "classic"),
          selected = "bw"
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
        sliderInput(
          inputId = "plotWidthUMAP",
          label = "Width of plot",
          min = 100, max = 2000,
          value = 800, step = 10,
          ticks = FALSE
        ),
        sliderInput(
          inputId = "plotHeightUMAP",
          label = "Height of plot",
          min = 100, max = 2000,
          value = 600, step = 10,
          ticks = FALSE
        ),

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
          # icon = icon("text-size"),
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
