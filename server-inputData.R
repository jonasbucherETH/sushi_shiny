inputDataReactive <- reactive({
# inputDataReactive <- {
  # waiter <- waiter::Waiter$new()
  # waiter$show()
  # on.exit(waiter$hide())
  # 
  # # Start from here:
  # queryList = parseQueryString(session$clientData$url_search)
  # if (is.list(queryList)){
  #   dataUrl <- queryList$data
  # } else {
  #   dataUrl <- NULL
  # }
  # urlDataRoot = c("/srv/gstore/projects", "/srv/GT/analysis/course_sushi/public/gstore/projects")
  # 
  # if (!is.null(dataUrl)) {
  #   dataDir <- file.path(urlDataRoot, dataUrl)
  #   dataDir <- dataDir[file.exists(dataDir)][1]
  #   if (!file.exists(dataDir)){
  #     # ezMail(paste("invalid dataDir: ", dataDir), subject="PopGen_Structure failed", to="gxtx_data_mngt@fgcz.ethz.ch")
  #     stop(paste("invalid dataDir", dataDir))
  #   }
  # } else {
  #   # dataDir <- "/srv/gstore/projects/p23793/o23960_EdgeR_RIVA-Ibru-6h--over--RIVA-DMSO_2022-09-02--16-54-00/RIVA-Ibru-6h--over--RIVA-DMSO"
  #   # put path to test dataset
  #   dataDir <- "/srv/gstore/projects/"
  # 
  # 
  #   # dataDir <- "/srv/gstore/projects/p24934/o27003_DESeq2_HP20--over--CTRLi_HP_2022-03-29--16-13-05/HP20--over--CTRLi_HP"
  #   # dataDir <- "/srv/gstore/projects/p23793/o29198_DESeq2_WT_antrum_Hp--over--WT_antrum_control_2022-09-29--12-37-08/WT_antrum_Hp--over--WT_antrum_control"
  #   # dataDir <- "/srv/gstore/projects/p3572/o29366_DESeq2_Huh7--over--JHH-5_2022-10-20--10-49-09/Huh7--over--JHH-5"
  #   # showModal(
  #   #   modalDialog(
  #   #     title = "No dataset link provided",
  #   #     paste0("There is no dataset link provided. Please add the path to your dataset to the URL."),
  #   #     easyClose = TRUE,
  #   #     footer = NULL
  #   #   )
  #   # )
  #   # stop()
  # }
  # rdsFilePath <- file.path(dataDir, "deResult.rds")
  # oraHTML <- file.path(dataDir, list.files(dataDir)[grep("ORA_results.xlsx", list.files(dataDir))])
  # gseaHTML <- file.path(dataDir, list.files(dataDir)[grep("GSEA_results.xlsx", list.files(dataDir))])
  # degHTML <- file.path(dataDir, list.files(dataDir)[grep("xlsx", list.files(dataDir))]) %>% .[!grepl("ORA|GSEA", .)]
  # if (file.exists(file.path(dirname(dataDir), "input_dataset.tsv"))) {
  #   inputData <- read_tsv(file.path(dirname(dataDir), "input_dataset.tsv"))
  #   colnames(inputData) <- gsub(" \\[.*", "", colnames(inputData))
  # }
  
  ### ------------- Load data for SUSHI -----------------------------------
  
  # pca <- readRDS("PCA.rds")
  # groupingVariables <- readRDS("grouping_vars.rds")
  # vcfRaw <- read.vcfR("vcf.rds")
  # dataset <- vcfR2genind(vcfRaw)
  # datasetScaled <- scaleGen(dataset, NA.method="mean")
  # 
  # mds <- read.csv("plink.mds", sep="")
  # 
  # distanceMatrixTSNE <- read_tsv("plink.dist", col_names = F)
  
  ### ------------- Load data for testing -----------------------------------
  # colnames(vcfRaw@gt)
  
  ## PCA
  vcfRaw <- read.vcfR("data/ragi_highcov_sa0001_1k.vcf.gz")
  # vcfR@gt has sample IDs as colnames (col 1 = FORMAT)
  vcfGenind <- vcfR2genind(vcfRaw)
  # dataset <- genind2df(vcfGenind, pop = NULL)
  dataset <- genind2df(vcfGenind)
  
  groupingVariables <- read.delim("data/populations.txt") # read.table or read.delim (used before) -> delim should be fine
  rownames(groupingVariables) <- groupingVariables[,1]
  factors <- colnames(groupingVariables)
  groupingVariables[42, 2] <- "DipPop"

  datasetScaled <- scaleGen(
    vcfGenind,
    center = FALSE,
    scale = FALSE,
    NA.method="mean"
  )

  mdsResults <- read.csv("data/plink_3101.mds", sep="")

  # ## t-SNE
  distanceMatrixTSNE <- read_tsv("data/plink_3101.dist", col_names = F)
  
  ## UMAP
  
  # vcfA <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/A_Eall.vcf.gz")
  # vcfB <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/B_Eall.vcf.gz")
  # vcfA_corrected <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/A_Eall.pruned.phased.sample_name_corrected.landraces.vcf.gz")
  # vcfB_corrected <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/B_Eall.pruned.phased.sample_name_corrected.landraces.vcf.gz")
  # dataset <- vcfR2genind(vcfA_corrected)
  # pop(dataset) <- c(groupingVariables[,2])
  
  # factorLevels <- NULL
  # for (i in seq_along(datasetScaled[factors])){
  #   factorLevels[[i]] <- paste0(
  #     colnames(datasetScaled[factors[[i]]]),
  #     ": ",
  #     levels(as.factor(datasetScaled[, factors[i]])))
  # }
  # factorLevels <- unlist(factorLevels)
  # 
  # 
  if (length(factors) == 1) {
    factors <- as.numeric(c(factors, factors))
  }

  colourList <- list()
  for (i in factors) {
    for (l in levels(as.factor(groupingVariables[, i]))) {
      colourList[l] <- NA
    }
  }

  return(list(
    # "pca" = pca,
    # "n_pcs" = n_pcs,
    # "n_grouping" = n_grouping,
    # "pc_list" = pc_list,
    # "pca_varprop" = pca_varprop,
    # "pca_tab" = pca_tab,
    # "tab_varprop" = tab_varprop,
    "vcfRaw" = vcfRaw,
    "dataset" = dataset,
    "datasetScaled" = datasetScaled,
    "groupingVariables" = groupingVariables,
    "distanceMatrixTSNE" = distanceMatrixTSNE,
    "mdsResults" = mdsResults
    # "colourList" = colourList
    # "factorLevels" = factorLevels
    # "mds" = mds,
    # "distanceMatrix" = distanceMatrix
    
    )
  )
  
# }
})
  