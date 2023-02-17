inputDataReactive <- reactive({
# inputDataReactive <- {
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())

  # Start from here:
  queryList = parseQueryString(session$clientData$url_search)
  if (is.list(queryList)){
    dataUrl <- queryList$data
  } else {
    dataUrl <- NULL
  }
  urlDataRoot = c("/srv/gstore/projects", "/srv/GT/analysis/course_sushi/public/gstore/projects")

  if (!is.null(dataUrl)) {
    dataDir <- file.path(urlDataRoot, dataUrl)
    dataDir <- dataDir[file.exists(dataDir)][1]
    if (!file.exists(dataDir)){
      # ezMail(paste("invalid dataDir: ", dataDir), subject="PopGen_Structure failed", to="gxtx_data_mngt@fgcz.ethz.ch")
      stop(paste("invalid dataDir", dataDir))
    }
  } else {
    # dataDir <- "/srv/gstore/projects/p23793/o23960_EdgeR_RIVA-Ibru-6h--over--RIVA-DMSO_2022-09-02--16-54-00/RIVA-Ibru-6h--over--RIVA-DMSO"
    # put path to test dataset
    dataDir <- "/srv/gstore/projects/p1535/test_vcf_dataset"
  }
  
  resultsDir <- file.path(urlDataRoot, "/srv/gstore/projects/p1535/PCAMDS_jonas_test13_2023-02-13--10-41-33")
  
  vcfRawFilePath <- file.path(dataDir, "ragi_highcov_sa0001_1k.vcf.gz")
  groupingVariablesFilePath <- file.path(dataDir, "populations.txt")
  mdsResultsFilePath <- file.path(resultsDir, "pca_mds/plink.mds")
  distanceMatrixTSNEFilePath <- file.path(resultsDir, "pca_mds/plink.dist")

  
  if (file.exists(file.path(dirname(dataDir), "input_dataset.tsv"))) {
    inputData <- read_tsv(file.path(dirname(dataDir), "input_dataset.tsv"))
    colnames(inputData) <- gsub(" \\[.*", "", colnames(inputData))
  }
  
  ### ------------- Load data for SUSHI -----------------------------------
  vcfRaw <- read.vcfR(vcfRawFilePath)
  groupingVariables <- read.delim(groupingVariablesFilePath)
  mdsResults <- read.csv(mdsResultsFilePath, sep="")
  distanceMatrixTSNE <- read_tsv(distanceMatrixTSNEFilePath, col_names = F)
  
  ### ------------- Load data for testing -----------------------------------
  # colnames(vcfRaw@gt)
  
  ## PCA
  # vcfRaw <- read.vcfR("data/ragi_highcov_sa0001_1k.vcf.gz")
  # # vcfR@gt has sample IDs as colnames (col 1 = FORMAT)
  # vcfGenind <- vcfR2genind(vcfRaw)
  # # dataset <- genind2df(vcfGenind, pop = NULL)
  # dataset <- genind2df(vcfGenind)
  # 
  # groupingVariables <- read.delim("data/populations.txt") # read.table or read.delim (used before) -> delim should be fine
  # rownames(groupingVariables) <- groupingVariables[,1]
  # factors <- colnames(groupingVariables)
  # groupingVariables[42, 2] <- "DipPop"
  # 
  # datasetScaled <- scaleGen(
  #   vcfGenind,
  #   center = FALSE,
  #   scale = FALSE,
  #   NA.method="mean"
  # )
  # 
  # mdsResults <- read.csv("data/plink_3101.mds", sep="")
  # 
  # # ## t-SNE
  # distanceMatrixTSNE <- read_tsv("data/plink_3101.dist", col_names = F)
  
  ## UMAP
  
  # vcfA <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/A_Eall.vcf.gz")
  # vcfB <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/B_Eall.vcf.gz")
  # vcfA_corrected <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/A_Eall.pruned.phased.sample_name_corrected.landraces.vcf.gz")
  # vcfB_corrected <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/B_Eall.pruned.phased.sample_name_corrected.landraces.vcf.gz")
  # dataset <- vcfR2genind(vcfA_corrected)
  # pop(dataset) <- c(groupingVariables[,2])
  
  ### ------------- Prepare data -----------------------------------
  vcfGenind <- vcfR2genind(vcfRaw)
  # dataset <- genind2df(vcfGenind, pop = NULL)
  dataset <- genind2df(vcfGenind)
  rownames(groupingVariables) <- groupingVariables[,1]
  factors <- colnames(groupingVariables)
  groupingVariables[42, 2] <- "DipPop"
  datasetScaled <- scaleGen(
    vcfGenind,
    center = FALSE,
    scale = FALSE,
    NA.method="mean"
  )
  
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
  