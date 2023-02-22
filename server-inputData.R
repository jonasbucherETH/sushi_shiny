inputDataReactive <- reactive({
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
    # dataDir <- "/srv/gstore/projects/p1535/popgen_shiny_JB_test2_2023-02-20--16-26-07/shiny_test2"
    dataDir <- "/srv/gstore/projects/p1535/popgen_shiny_JB_test2_2023-02-20--16-26-07/shiny_test2"

  }

  if (file.exists(file.path(dirname(dataDir), "input_dataset.tsv"))) {
    inputData <- read_tsv(file.path(dirname(dataDir), "input_dataset.tsv"))
    colnames(inputData) <- gsub(" \\[.*", "", colnames(inputData))
  }

  # groupingVariablesFilePath <- file.path(dataDir, list.files(dataDir)[grep("grouping", list.files(dataDir))])
  # print(groupingVariablesFilePath)
  
  # mdsResultsFilePath <- file.path(dataDir, "pca_mds/plink.mds")
  mdsResultsFilePath <- file.path(dataDir, list.files(dataDir)[grep("plink.mds", list.files(dataDir))])
  print(mdsResultsFilePath)
  
  # distanceMatrixTSNEFilePath <- file.path(dataDir, "plink.dist")
  distanceMatrixTSNEFilePath <- file.path(dataDir, list.files(dataDir)[grep("plink.dist$", list.files(dataDir))])
  print(distanceMatrixTSNEFilePath)

  
  vcfRawFilePath <- file.path(urlDataRoot[1], inputData$`Filtered VCF`) # [2] works as well
  # vcfRawFilePath <- file.path("~/git/sushi_shiny/data/ragi_highcov_sa0001_1k.vcf.gz")
  print(vcfRawFilePath)
  
  groupingVariablesFilePath <- file.path(urlDataRoot[1], inputData$`Grouping File`)
  print(groupingVariablesFilePath)
  
  # datasetScaledFilePath <- file.path(dataDir, list.files(dataDir)[grep("datasetScaled.rds", list.files(dataDir))])
  # datasetScaledFilePath <- file.path(dataDir, list.files(dataDir)[grep("datasetScaled.rds", list.files(dataDir))])
  # print(datasetScaledFilePath)
  
  ### ------------- Load data for SUSHI -----------------------------------
  vcfRaw <- read.vcfR(vcfRawFilePath)
  # datasetScaled <- readRDS(datasetScaledFilePath)
  # vcfRaw <- read.vcfR(vcfRawFilePath)
  vcfGenind <- vcfR2genind(vcfRaw)
  datasetScaled <- scaleGen(vcfGenind,
                            center = FALSE,
                            scale = FALSE,
                            NA.method="mean")
  # groupingVariables <- readRDS(groupingVariablesFilePath)
  groupingVariables <- read.delim(groupingVariablesFilePath)
  mdsResults <- read.csv(mdsResultsFilePath, sep="")
  distanceMatrixTSNE <- read_tsv(distanceMatrixTSNEFilePath, col_names = F)
  
  ### ------------- Load data for testing -----------------------------------
  # vcfRaw <- read.vcfR("data/ragi_highcov_sa0001_1k.vcf.gz")
  # groupingVariables <- read.delim("data/populations.txt") # read.table or read.delim (used before) -> delim should be fine
  # mdsResults <- read.csv("data/plink_3101.mds", sep="")
  # distanceMatrixTSNE <- read_tsv("data/plink_3101.dist", col_names = F)
  
  
  # vcfA <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/A_Eall.vcf.gz")
  # vcfB <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/B_Eall.vcf.gz")
  # vcfA_corrected <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/A_Eall.pruned.phased.sample_name_corrected.landraces.vcf.gz")
  # vcfB_corrected <- read.vcfR("~/git/ezRun/R/PCAMDS_shiny/data/sample_dataset4jonas_20230206/B_Eall.pruned.phased.sample_name_corrected.landraces.vcf.gz")
  # dataset <- vcfR2genind(vcfA_corrected)
  # pop(dataset) <- c(groupingVariables[,2])
  
  ### ------------- Prepare data -----------------------------------
  # vcfGenind <- vcfR2genind(vcfRaw)
  # dataset <- genind2df(vcfGenind)
  # dataset <- genind2df(vcfGenind, pop = NULL)
  # dataset <- genind2df(vcfGenind)
  rownames(groupingVariables) <- groupingVariables[,1]
  factors <- colnames(groupingVariables)
  # groupingVariables[42, 2] <- "DipPop"
  # datasetScaled <- scaleGen(
  #   vcfGenind,
  #   center = FALSE,
  #   scale = FALSE,
  #   NA.method="mean"
  # )
  
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
    # "vcfRaw" = vcfRaw,
    # "dataset" = dataset,
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
  