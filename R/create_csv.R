dev <- FALSE

if(dev) {
  library(openxlsx2)

  meta_data <- read_xlsx(file = "./data/Database/SampleMasterfile.xlsx",
                         sheet = 1,
                         skip_empty_rows = TRUE)

  # remove QC and blanks
  meta_data <- meta_data[!grepl(x = meta_data$sampleType,
                                pattern = "(blank|quality)",
                                ignore.case = TRUE), ]

  # get all experiments / comparisons
  experiments <- sort(unique(meta_data$experimentId))

  # remove all NLA stuff
  # experiments <- experiments[!grepl(pattern = "^NLA_.*",
  #                                   x = experiments)]

  export <- data.frame(matrix(ncol = 11,
                              nrow = length(experiments)))
  colnames(export) <- c("experimentId", "experimentTitle", "genoType",
                        "cellType", "parentCellLine", "drugTreatment", "sex",
                        "method", "contributingLab", "harvestDate", "doi")

  for(a in 1:length(experiments)) {
    experimentTitle <- unique(meta_data$experimentTitle[meta_data$experimentId == experiments[a]])
    experimentTitle <- experimentTitle[!is.na(experimentTitle)]
    experimentTitle <- experimentTitle[experimentTitle != "NA"]
    experimentTitle <- paste(experimentTitle, collapse = ", ")

    genoType <- unique(meta_data$genoType[meta_data$experimentId == experiments[a]])
    genoType <- genoType[!is.na(genoType)]
    genoType <- genoType[genoType != "NA"]
    genoType <- paste(genoType, collapse = ", ")

    # remove blanks and QC from cellType
    sampleType <- unique(meta_data$sampleType[meta_data$experimentId == experiments[a]])
    sampleType <- sampleType[!is.na(sampleType)]
    sampleType <- sampleType[sampleType != "NA"]
    sampleType <- paste(sampleType, collapse = ", ")

    parentCellLine <- unique(meta_data$parentCellLineBrainregion[meta_data$experimentId == experiments[a]])
    parentCellLine <- parentCellLine[!is.na(parentCellLine)]
    parentCellLine <- parentCellLine[parentCellLine != "NA"]
    parentCellLine <- paste(parentCellLine, collapse = ", ")

    treatmentDiagnosis <- unique(meta_data$treatmentDiagnosis[meta_data$experimentId == experiments[a]])
    treatmentDiagnosis <- treatmentDiagnosis[!is.na(treatmentDiagnosis)]
    treatmentDiagnosis <- treatmentDiagnosis[treatmentDiagnosis != "NA"]
    treatmentDiagnosis <- paste(treatmentDiagnosis, collapse = ", ")

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

    doi <- unique(meta_data$doi[meta_data$experimentId == experiments[a]])
    doi <- doi[!is.na(doi)]
    doi <- doi[doi != "NA"]
    doi <- paste(doi, collapse = ", ")

    export$experimentId[a] <- experiments[a]
    export$experimentTitle[a] <- experimentTitle
    export$genoType[a] <- genoType
    export$cellType[a] <- sampleType
    export$parentCellLine[a] <- parentCellLine
    export$drugTreatment[a] <- treatmentDiagnosis
    export$sex[a] <- sex
    export$method[a] <- "SLA" # method
    export$contributingLab[a] <- contributingLab
    export$harvestDate[a] <- harvestDate
    export$doi[a] <- doi

  }

  ## Need to do some cleaning before sending to Menno
  # Remove datasets with multiple harvest dates
  # keep <- !grepl(pattern = ",",
  #                x = export$harvestDate)
  # export <- export[keep, ]
  # # Remove experiment VDK_230406_01
  # export <- export[export$experimentId != "VDK_230406_01", ]

  write.table(x = export,
              file = "./neurolipid_meta.csv",
              quote = TRUE,
              sep = ",",
              row.names = FALSE)
}