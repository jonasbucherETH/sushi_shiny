library("shiny")
library("shinymanager")
library("shinyjs")
library("tidyverse")
library("ggpubr")
library("plotly")
library("DESeq2")
library("RColorBrewer")
library("ComplexHeatmap")
library("tximport")
library("vsn")
library("clusterProfiler")
library("DT")
library("colourpicker")
library("writexl")
library("circlize")
library("GO.db")
library("shinydashboard")
library("shinyBS")
library("pixiedust")
library("ezRun")
library("kableExtra")
library("ggrepel")
library("gplots")
library("sortable")
library("waiter")
library("shinycssloaders")
library("ggprism")
library("ggbeeswarm")
library("rstatix")
library("gridExtra")
library("shinytitle")
library("shinylogs")

# JB libraries
library("magrittr")
library("adegenet")
library("ade4")
library("gdsfmt")
library("SNPRelate")
library("ggplot2")
library("shinythemes")
# library("shinytest")
library("vcfR")
library("styler")
library("Rtsne")
library("uwot")
# library("shinydust")
library("shinyWidgets")
library("readr")
# library("testthat")

# console.error = function () {
#   require("system").stderr.write(Array.prototype.join.call(arguments, ' ') + '\n');
# };

reactiveConsole(TRUE)

# For secure login:
# library(digest)
# digest("password1", algo = "md5")
# credentials <- data.frame(
#   user = c("user1", "user2"),
#   password = c(
#     "password1", "password2"),
#   admin = c(FALSE, TRUE),
#   comment = "Login Page.",
#   stringsAsFactors = FALSE
# )

########
# spinner <- tagList(
#   spin_chasing_dots(),
#   span("Loading stuff...", style="color:white;")
# )

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "PopGen",
    # tags$li(
    #   a(
    #     href = 'mailto:sequencing@fgcz.ethz.ch?subject=exploreDEG-shiny-app-feedback',
    #     "Request Features/Report Bugs"),
    #   class = "dropdown"
    # ),
    tags$li(
      a(href = 'http://www.fgcz.ch',
        target = "_blank",
        img(src = 'fgcz_logo.png', title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"),
      class = "dropdown"),
    tags$li(
      a(href = 'http://www.ethz.ch/en.html',
        target = "_blank",
        img(src = 'eth_logo.png', title = "FGCZ", height = "22px"),
        style = "padding-top:13px; padding-bottom:10px;"),
      class = "dropdown"),
    tags$li(
      a(href = 'http://www.uzh.ch/en.html',
        target = "_blank",
        img(src = 'University_of_Zurich_Logo.png', title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"),
      class = "dropdown")
  ),
  dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "PCA",
        tabName = "tab-PCA",
        icon = icon("ranking-star")
      ),
      menuItem(
        text = "t-SNE",
        tabName = "tab-TSNE",
        icon = icon("square-share-nodes")
        # square-poll-vertical
        # icon = icon("table")
      ),
      menuItem(
        text = "UMAP",
        tabName = "tab-UMAP",
        icon = icon("network-wired")
      ),
      menuItem(
        text = "MDS",
        tabName = "tab-MDS",
        icon = icon("cube")
        # icon = icon("cubes")
        # icon = icon("codepen")
        # icon = icon("border-none")
      ),
      menuItem(
        text = "comparison",
        tabName = "tab-comparison",
        icon = icon("codepen")
        # icon = icon("cubes")
        # icon = icon("codepen")
        # icon = icon("border-none")
      )
    )
  ),
  dashboardBody(
    # use_tracking(),
    # tags$head(tags$link(rel = "shortcut icon", href = "sushi.png")),
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css")),
    # use_waiter(),
    tabItems(
      source("ui-PCA.R", local = TRUE)$value,
      source("ui-tSNE.R", local = TRUE)$value,
      source("ui-UMAP.R", local = TRUE)$value,
      source("ui-MDS.R", local = TRUE)$value,
      source("ui-comparison.R", local = TRUE)$value
      

      # source("~/git/ezRun/R/PCAMDS_shiny/ui-PCA.R", local = F)$value
    )
  )
)

server <- function(input, output, session) {
  # track_usage(storage_mode = store_rds(path = "/scratch/shiny_logs/"), app_name="PCAMDS_shiny")

  # inputDataReactive <- reactive({source(file="server-inputData.R", local=T)})
  # reactive({source(file="server-inputData.R", local=T)})
  # inputDataReactive <- reactive({source(file="server-inputData.R", local=T)})
  # cat("after input source")

  source("server-inputData.R", local = TRUE)
  source("server-PCA.R", local = TRUE)
  source("server-tSNE.R", local = TRUE)
  source("server-UMAP.R", local = TRUE)
  source("server-MDS.R", local = TRUE)
  
  
  
}

shinyApp(ui = ui, server = server)
