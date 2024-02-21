dev <- FALSE

if(dev) {
  library(ggplot2)
  library(tidyr)

  work_dir <- "/home/ricoderks/Documents/LUMC/Projects/soda-light/"
  feature_table <- read.csv(file = file.path(work_dir, "240220_features.csv"),
                            row.names = 1,
                            header = TRUE)
  data_table <- read.csv(file = file.path(work_dir, "240220_data.csv"),
                         row.names = 1,
                         header = TRUE,
                         check.names = FALSE)
  meta_table <- read.csv(file = file.path(work_dir, "240220_meta.csv"),
                         row.names = 1,
                         header = TRUE,
                         check.names = FALSE)


  # initialize some stuff
  # the feature table doesn't contain a column lipids fix here
  feature_table$lipid <- rownames(feature_table)

  # get the unique lipid classes
  lipid_classes <- unique(feature_table$lipid_class)
  # for now remove TG and PA
  lipid_classes <- lipid_classes[!(lipid_classes %in% c("PA", "TG"))]

  tot_lipids <- vector(mode = "list",
                       length = length(lipid_classes))
  names(tot_lipids) <- lipid_classes
  tot_lipids <- lapply(tot_lipids, function(x) {
    setNames(vector(mode = "list",
                    length = 3),
             c("tot_sat", "tot_unsat", "SI"))
  })



  for(a in lipid_classes) {
    # a <- "PC"
    # lipids with only one FA chain (including PA)
    if(all(feature_table$carbons_2[feature_table$lipid_class == a] == 0) | a == "TG") {
      if(a == "TG") {
        sat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                           feature_table$unsat_2 == 0]
        unsat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                             feature_table$unsat_2 != 0]
        sat_lipid_dbl <- NULL
        unsat_lipid_dbl <- NULL
      } else {
        sat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                           feature_table$unsat_1 == 0]
        unsat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                             feature_table$unsat_1 != 0]
        sat_lipid_dbl <- NULL
        unsat_lipid_dbl <- NULL
      }
    } else {
      # lipids with 2 FA chains
      # this also selects PC 14:0_14:0
      sat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                         (feature_table$unsat_1 == 0 |
                                            feature_table$unsat_2 == 0)]
      unsat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                           (feature_table$unsat_1 != 0 |
                                              feature_table$unsat_2 != 0)]
      sat_lipid_dbl <- feature_table$lipid[feature_table$lipid_class == a &
                                             feature_table$unsat_1 == 0 &
                                             feature_table$unsat_2 == 0]
      unsat_lipid_dbl <- feature_table$lipid[feature_table$lipid_class == a &
                                               feature_table$unsat_1 != 0 &
                                               feature_table$unsat_2 != 0]
    }

    ## get data per lipid class
    lipid_data <- data_table[, colnames(data_table) %in% feature_table$lipid[feature_table$lipid_class == a], drop = FALSE]
    ## saturated
    lipid_data_sat <- lipid_data[, colnames(lipid_data) %in% union(sat_lipid, sat_lipid_dbl), drop = FALSE]
    # if contains 2x saturated FA tail multiply by 2
    lipid_data_sat[, colnames(lipid_data_sat) %in% sat_lipid_dbl] <- lipid_data_sat[, colnames(lipid_data_sat) %in% sat_lipid_dbl] * 2

    ## unsaturated
    lipid_data_unsat <- lipid_data[, colnames(lipid_data) %in% union(unsat_lipid, unsat_lipid_dbl), drop = FALSE]
    # if contains 2x unsaturated FA tail multiply by 2
    lipid_data_unsat[, colnames(lipid_data_unsat) %in% unsat_lipid_dbl] <- lipid_data_unsat[, colnames(lipid_data_unsat) %in% unsat_lipid_dbl] * 2

    # calculate the SI index
    tot_lipids[[a]][["tot_sat"]] <- rowSums(lipid_data_sat, na.rm = TRUE)
    tot_lipids[[a]][["tot_unsat"]] <- rowSums(lipid_data_unsat, na.rm = TRUE)
    tot_lipids[[a]][["SI"]] <- tot_lipids[[a]][["tot_sat"]] / tot_lipids[[a]][["tot_unsat"]]

  }

  # make data.frame
  tot_lipids <- do.call("cbind.data.frame", lapply(tot_lipids, function(x) {
    x["SI"]
  }))
  names(tot_lipids) <- lipid_classes


  # plotting
  SI_data <- vector(mode = "list",
                    length = length(lipid_classes))
  names(SI_data) <- lipid_classes
  for(a in lipid_classes) {
    SI_data[[a]] <- tapply(tot_lipids[, a], meta_table$genoType, function(x) {
      data.frame("mean" = mean(x, na.rm = TRUE),
                 "sd" = sd(x, na.rm = TRUE))
    })
  }
  SI_data <- do.call("cbind.data.frame", SI_data)
  SI_data$genoType <- rownames(SI_data)
  SI_data <- SI_data |>
    pivot_longer(cols = -genoType,
                 names_to = "lipidClass",
                 values_to = "value")
  SI_data <- unnest(SI_data,
                    cols = value)

  SI_data |>
    ggplot(aes(x = genoType,
               y = mean,
               fill = genoType)) +
    geom_col() +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean + sd),
                  width = 0.2) +
    facet_wrap(. ~ lipidClass,
               scales = "free") +
    theme_minimal() +
    theme(legend.position = "none")
}