tabItem(
  tabName = "tab-MDS",
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
          "MDS..."
        ),
        tags$p("...")
      ),
      box(
        title = "Parameters",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        # inputs
        # actionButton(
        #   inputId = "goButton",
        #   label = "Apply parameters",
        #   icon = NULL
        # ),
        # checkboxInput(
        #   inputId = "normalizeMDS",
        #   label = "Normalize distance matrix (recommended for Euclidean distances)",
        #   value = TRUE
        # ),
        br(), br(),
        actionBttn(
          inputId = "paramButtonMDS",
          label = "Apply parameters",
          # icon = icon("check"),
          style = "simple",
          color = "primary",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        ),
        # selectInput(
        #   inputId = "removeSamplesMDS",
        #   label = "Select samples to exclude",
        #   choices = "",
        #   selected = ""
        # ),
        
      ) # close box 
    ), # close t-SNE parameters column
    
    column(
      width = 7,
      box(
        title = "Multidimensional scaling",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        downloadButton(
          outputId = "downloadMDS",
          label = "Download MDS Plot (PDF)"
        ),
        br(), br(),
        plotOutput(
          outputId = "MDSStatic",
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
          inputId = "MDSTitle",
          label = "Title of plot",
          value = ""
        ),
        
        checkboxInput(
          inputId = "sampleLabelsMDS",
          label = "Display sample labels",
          value = TRUE
        ),
        selectInput(
          inputId = "selectThemeMDS",
          label = "Select theme",
          choices = c("bw", "light", "minimal", "classic"),
          selected = "bw"
        ),
        ### no choices, selected = "" as default
        selectInput(
          inputId = "colorGroupMDS",
          label = "Select groups to color by",
          choices = "",
          selected = ""
        ),
        selectInput(
          inputId = "shapeGroupMDS",
          label = "Select groups for shapes",
          choices = "",
          selected = ""
        ),
        sliderInput(
          inputId = "plotWidthMDS",
          label = "Width of plot",
          min = 100, max = 2000,
          value = 800, step = 10,
          ticks = FALSE
        ),
        sliderInput(
          inputId = "plotHeightMDS",
          label = "Height of plot",
          min = 100, max = 2000,
          value = 600, step = 10,
          ticks = FALSE
        ),
        
        fluidRow(
          column(
            numericInput(
              inputId = "pointSizeMDS",
              label = "Point size", min = 1, max = 6,
              value = 3, step = 0.5,
              # width = "100px"
            ),
            width = 6
          ),
          column(
            numericInput(
              inputId = "textSizeMDS",
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
          inputId = "displayButtonMDS",
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
          inputId = "selectizeMDS",
          label = "Select:",
          choices = "",
          selected = "",
          multiple = TRUE
        ),
        
        # selectizeInput("select", "Select:", 
        #                choices = as.list(unique(groupingVariables$Population)), 
        #                # selected = "X1", 
        #                multiple = TRUE),
        
        uiOutput('colourPanelMDS'),
      )
    )
  )
) # close tabItem
