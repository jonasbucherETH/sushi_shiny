tabItem(
  tabName = "tab-tSNE",
  fluidRow( ### NOTE: 1 row has width = 12
    
    column(
      width = 2, 
      box(
        title = "t-SNE Parameters",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        # inputs
        # actionButton(
        #   inputId = "goButton",
        #   label = "Apply parameters",
        #   icon = NULL
        # ),
        # checkboxInput(
        #   inputId = "normalizeTSNE",
        #   label = "Normalize distance matrix (recommended for Euclidean distances)",
        #   value = TRUE
        # ),
        numericInput(
          inputId = "perplexityTSNE",
          label = "Perplexity parameter",
          min = 5, max = 5,
          value = 5, step = 5
        ),
        numericInput(
          inputId = "max_iterTSNE",
          label = "Maximum number of iterations",
          min = 500, max = 3000,
          value = 1000, step = 500
        ),        
        numericInput(
          inputId = "thetaTSNE",
          label = HTML("&theta;: Speed/accuracy trade-off.
                       Set to 0.0 for exact TSNE"),
          # label = HTML("&eta;:"),
          min = 0, max = 1,
          value = 0.5, step = 0.1
        ),        
        numericInput(
          inputId = "etaTSNE",
          label = HTML("&eta;: learning rate"),
          min = 50, max = 800,
          value = 200, step = 50
        ),
        br(), br(),
        actionBttn(
          inputId = "paramButtonTSNE",
          label = "Apply parameters",
          # icon = icon("check"),
          style = "simple",
          color = "primary",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        ),
        # selectInput(
        #   inputId = "removeSamplesTSNE",
        #   label = "Select samples to exclude",
        #   choices = "",
        #   selected = ""
        # ),
        
      ) # close box 
    ), # close t-SNE parameters column
    
    column(
      width = 7,
      box(
        title = "TSNE Plots",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        downloadButton(
          outputId = "downloadTSNE",
          label = "Download t-SNE Plot (PDF)"
        ),
        br(), br(),
        plotOutput(
          outputId = "tsneStatic",
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
          inputId = "tsneTitle",
          label = "Title of plot",
          value = ""
        ),

        checkboxInput(
          inputId = "sampleLabelsTSNE",
          label = "Display sample labels",
          value = TRUE
        ),
        selectInput(
          inputId = "selectThemeTSNE",
          label = "Select theme",
          choices = c("bw", "light", "minimal", "classic"),
          selected = "bw"
        ),
        ### no choices, selected = "" as default
        selectInput(
          inputId = "colorGroupTSNE",
          label = "Select groups to color by",
          choices = "",
          selected = ""
        ),
        selectInput(
          inputId = "shapeGroupTSNE",
          label = "Select groups for shapes",
          choices = "",
          selected = ""
        ),
        sliderInput(
          inputId = "plotWidthTSNE",
          label = "Width of plot",
          min = 100, max = 2000,
          value = 800, step = 10,
          ticks = FALSE
        ),
        sliderInput(
          inputId = "plotHeightTSNE",
          label = "Height of plot",
          min = 100, max = 2000,
          value = 600, step = 10,
          ticks = FALSE
        ),
        
        fluidRow(
          column(
            numericInput(
              inputId = "pointSizeTSNE",
              label = "Point size", min = 1, max = 6,
              value = 3, step = 0.5,
              # width = "100px"
            ),
            width = 6
          ),
          column(
            numericInput(
              inputId = "textSizeTSNE",
              label = "Font Size", min = 4, max = 30,
              value = 12, step = 0.5,
              # width = "100px"
            ),
            width = 6
          )
        ),

      ), # close box
      
    # ), # close column
    
  # ), # close fluidRow 1
  # fluidRow(
  

      box(
        title = "Custom colours",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        actionBttn(
          inputId = "displayButtonTSNE",
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
          inputId = "selectizeTSNE",
          label = "Select:",
          choices = "",
          selected = "",
          multiple = TRUE
        ),
        
        # selectizeInput("select", "Select:", 
        #                choices = as.list(unique(groupingVariables$Population)), 
        #                # selected = "X1", 
        #                multiple = TRUE),
        
        uiOutput('colourPanelTSNE'),
      )
    )
  )
) # close tabItem
