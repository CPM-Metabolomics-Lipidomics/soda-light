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

  export <- data.frame(matrix(ncol = 10,
                              nrow = length(experiments)))
  colnames(export) <- c("experimentId", "experimentTitle", "genoType",
                        "cellType", "parentCellLine", "drugTreatment", "sex",
                        "method", "contributingLab", "harvestDate")

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

    treatment <- unique(meta_data$treatment[meta_data$experimentId == experiments[a]])
    treatment <- treatment[!is.na(treatment)]
    treatment <- treatment[treatment != "NA"]
    treatment <- paste(treatment, collapse = ", ")

    sex <- unique(meta_data$sex[meta_data$experimentId == experiments[a]])
    sex <- sex[!is.na(sex)]
    sex <- sex[sex != "NA"]
    sex <- paste(sex, collapse = ", ")

    ## Method column not present
    # method <- unique(meta_data$method[meta_data$experimentId == experiments[a]])
    # method <- method[!is.na(method)]
    # method <- method[method != "NA"]
    # method <- paste(method, collapse = ", ")

    contributingLab <- unique(meta_data$lab[meta_data$experimentId == experiments[a]])
    contributingLab <- contributingLab[!is.na(contributingLab)]
    contributingLab <- contributingLab[contributingLab != "NA"]
    contributingLab <- paste(contributingLab, collapse = ", ")

    harvestDate <- unique(meta_data$harvestDate[meta_data$experimentId == experiments[a]])
    harvestDate <- harvestDate[!is.na(harvestDate)]
    harvestDate <- harvestDate[harvestDate != "NA"]
    harvestDate <- paste(harvestDate, collapse = ", ")

    export$experimentId[a] <- experiments[a]
    export$experimentTitle[a] <- paste(genoType, cellType, collapse = " ")
    export$genoType[a] <- genoType
    export$cellType[a] <- cellType
    export$parentCellLine[a] <- parentCellLine
    export$drugTreatment[a] <- treatment
    export$sex[a] <- sex
    export$method[a] <- "SLA" # method
    export$contributingLab[a] <- contributingLab
    export$harvestDate[a] <- harvestDate

  }

  ## Need to do some cleaning before sending to Menno
  # Remove datasets with multiple harvest dates
  keep <- !grepl(pattern = ",",
                 x = export$harvestDate)
  export <- export[keep, ]
  # Remove experiment VDK_230406_01
  export <- export[export$experimentId != "VDK_230406_01", ]

  write.table(x = export,
              file = "./neurolipid_meta.csv",
              quote = TRUE,
              sep = ",",
              row.names = FALSE)
}