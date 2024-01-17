dev <- FALSE

if(dev) {
  library(openxlsx2)

  meta_data <- read_xlsx(file = "./data/Database/SampleMasterfile.xlsx",
                         sheet = 1,
                         skip_empty_rows = TRUE)


  experiments <- sort(unique(meta_data$experimentId))

  # remove all NLA stuff
  experiments <- experiments[!grepl(pattern = "^NLA_.*",
                                    x = experiments)]

  export <- data.frame(matrix(ncol = 6,
                              nrow = length(experiments)))
  colnames(export) <- c("experimentId", "experimentTitle", "genoType",
                        "cellType", "parentCellLine", "cellLine", "treatment")

  for(a in 1:length(experiments)) {
    genoType <- unique(meta_data$genoType[meta_data$experimentId == experiments[a]])
    genoType <- genoType[!is.na(genoType)]
    genoType <- genoType[genoType != "NA"]
    genoType <- paste(genoType, collapse = ", ")

    cellType <- unique(meta_data$cellType[meta_data$experimentId == experiments[a]])
    cellType <- cellType[!is.na(cellType)]
    cellType <- cellType[cellType != "NA"]
    cellType <- paste(cellType, collapse = ", ")

    parentCellLine <- unique(meta_data$parentalCellLine[meta_data$experimentId == experiments[a]])
    parentCellLine <- parentCellLine[!is.na(parentCellLine)]
    parentCellLine <- parentCellLine[parentCellLine != "NA"]
    parentCellLine <- paste(parentCellLine, collapse = ", ")

    cellLine <- unique(meta_data$cellLineName[meta_data$experimentId == experiments[a]])
    cellLine <- cellLine[!is.na(cellLine)]
    cellLine <- cellLine[cellLine != "NA"]
    cellLine <- paste(cellLine, collapse = ", ")

    treatment <- unique(meta_data$treatment[meta_data$experimentId == experiments[a]])
    treatment <- treatment[!is.na(treatment)]
    treatment <- treatment[treatment != "NA"]
    treatment <- paste(treatment, collapse = ", ")

    export$experimentId[a] <- experiments[a]
    export$experimentTitle[a] <- paste(genoType, cellType, collapse = " ")
    export$genoType[a] <- genoType
    export$cellType[a] <- cellType
    export$parentCellLine[a] <- parentCellLine
    export$cellLine[a] <- cellLine
    export$treatment[a] <- treatment

  }

  write.table(x = export,
              file = "./neurolipid_meta.csv",
              quote = TRUE,
              sep = ",",
              row.names = FALSE)
}