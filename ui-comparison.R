tabItem(
  tabName = "tab-comparison",
  fluidRow( ### NOTE: 1 row has width = 12
    
    column(
      width = 6, 
      box(
        title = "PCA",
        width = NULL,
        height = "50%",
        solidHeader = TRUE,
        status = "primary",
        plotOutput(
          outputId = "pcaStatic2",
          height = "50vh",
          width = "100%",
          inline = FALSE
        )
      ),
      box(
        title = "UMAP",
        width = NULL,
        height = "50%",
        solidHeader = TRUE,
        status = "primary",
        plotOutput(
          outputId = "UMAPStatic2",
          height = "50vh",
          width = "100%",
          inline = FALSE
        )
      )
    ),
    
    column(
      width = 6, 
      box(
        title = "t-SNE",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        plotOutput(
          outputId = "tsneStatic2",
          height = "50vh",
          width = "100%",
          inline = FALSE
        )
      ),
      box(
        title = "MDS",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        plotOutput(
          outputId = "MDSStatic2",
          height = "50vh",
          width = "100%",
          inline = FALSE
        )
      )
    )
  )
) # close tabItem
