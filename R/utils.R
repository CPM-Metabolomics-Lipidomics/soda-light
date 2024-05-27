# Utility functions
base::source('./R/complex_functions/volcano.R')
base::source('./R/complex_functions/pca.R')

#--------------------------------------------------------- Switch functions ----

experiment_switch = function(selection) {
  switch(EXPR = selection,
         'Lipidomics' = 'lips',
         'Proteomics' = 'prot',
         'Transcriptomics' = 'trns'
  )
}

adjustment_switch = function(selection){
  switch(EXPR = selection,
         "None" = "minus_log10_p_value",
         "Benjamini-Hochberg" = "minus_log10_p_value_bh_adj"
  )
}

adjustment_title_switch = function(selection) {
  switch(EXPR = selection,
         "minus_log10_p_value" = "-Log10(p-value)",
         "minus_log10_p_value_bh_adj" = "-Log10(BH(p-value))"
  )
}

feature_table_cols_switch = function(col) {
  switch(EXPR = col,
         'Lipid class' = 'lipid_class',
         'SN1 number of double bonds' = 'unsat_1',
         'SN1 number of carbons' = 'carbons_1',
         'SN2 number of double bonds' = 'unsat_2',
         'SN2 number of carbons' = 'carbons_2',
         'Total number of double bonds' = 'unsat_sum',
         'Total number of carbons' = 'carbons_sum'
  )
}

r6_switch = function(exp_type, name, id, slot, data_file, experiment_id){
  switch(EXPR = exp_type,
         "Lipidomics" = Lips_exp$new(name = name, id = id, slot = slot, data_file = data_file, experiment_id = experiment_id),
         "Proteomics" = Prot_exp$new(name = name, id = id, slot = slot),
         "Transcriptomics" = Trns_exp$new(name = name, id = id, slot = slot)

  )
}

table_switch = function(table_name, r6) {
  switch(EXPR = table_name,
         'Imported metadata table' = r6$tables$imp_meta,
         'Raw metadata table' = r6$tables$raw_meta,
         'Imported data table' = r6$tables$imp_data,
         'Raw data table' = r6$tables$raw_data,
         'Feature table' = r6$tables$feature_table,
         'Blank table' = r6$tables$blank_table,
         'Class normalized table' = r6$tables$class_norm_data,
         'Total normalized table' = r6$tables$total_norm_data,
         'Z-scored table' = r6$tables$z_scored_data,
         'Z-scored class normalized table' = r6$tables$z_scored_class_norm_data,
         'Z-scored total normalized table' = r6$tables$z_scored_total_norm_data,
         'Class table' = r6$tables$class_table,
         'Class table z-scored' = r6$tables$class_table_z_scored,
         'Class table total normalized' = r6$tables$class_table_total_norm,
         'Class table z-scored total normalized' = r6$tables$class_table_z_scored_total_norm,
         'Species summary table' = r6$tables$summary_species_table,
         'Class summary table' = r6$tables$summary_class_table,
         'GSEA prot list' = r6$tables$prot_list
  )
}

table_name_switch = function(table_name) {
  switch(EXPR = table_name,
         'Imported metadata table' = 'imp_meta',
         'Raw metadata table' = 'raw_meta',
         'Imported data table' = 'imp_data',
         'Raw data table' = 'raw_data',
         'Imported feature table' = 'imp_feature_table',
         'Feature table' = 'feature_table',
         'Blank table' = 'blank_table',
         'Class normalized table' = 'class_norm_data',
         'Total normalized table' = 'total_norm_data',
         'Z-scored table' = 'z_scored_data',
         'Z-scored class normalized table' = 'z_scored_class_norm_data',
         'Z-scored total normalized table' = 'z_scored_total_norm_data',
         'Class table' = 'class_table',
         'Class table z-scored' = 'class_table_z_scored',
         'Class table total normalized' = 'class_table_total_norm',
         'Class table z-scored total normalized' = 'class_table_z_scored_total_norm',
         'Species summary table' = 'summary_species_table',
         'Class summary table' = 'summary_class_table',
         'GSEA prot list' = 'prot_list'
  )
}

method_switch = function(method) {
  switch(EXPR = method,
         'minimum' = base::min,
         'mean' = base::mean,
         'median' = stats::median,
         'maximum' = base::max
  )
}

#--------------------------------------------------------------- Plot lists ----
lipidomics_plot_list = function() {
  plot_list = c("Class distribution" = "select_class_distribution",
                "Class comparison" = "select_class_comparison",
                "Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "PCA" = "select_pca",
                # "Double bond plot" = "select_double_bond_plot",
                # "Saturation index" = "select_satindex_plot",
                "Fatty acid analysis" = "select_fa_analysis_plot",
                "Fatty acid composition" = "select_fa_composition"
  )
  return(plot_list)
}

proteomics_plot_list = function() {
  plot_list = c("Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "PCA" = "select_pca"
  )
  return(plot_list)
}

gsea_plot_list = function() {
  plot_list = c("Dot plot" = "select_dot_plot",
                "Ridge plot" = "select_ridge_plot",
                "CNET plot" = "select_cnet_plot",
                "eMap plot" = "select_emap_plot"
  )
  return(plot_list)
}

or_plot_list = function() {
  plot_list = c("Dot plot" = "select_dot_plot",
                "Bar plot" = "select_bar_plot",
                "CNET plot" = "select_cnet_plot",
                "eMap plot" = "select_emap_plot"
  )
  return(plot_list)
}

get_mofa_plot_list = function() {
  plot_list = c("Explained variance" = "select_explained_variance",
                "Factor plot" = "select_factor_plot",
                "Combined factors" = "select_combined_factors",
                "Feature weights" = "select_feature_weights",
                "Feature top weights" = "select_feature_top_weights",
                "MOFA Heatmap" = "select_mofa_heatmap",
                "Scatterplot" = "select_scatterplot"
  )
  return(plot_list)
}

get_snf_plot_list = function() {
  plot_list = c("Clusters heatmap 1 " = "select_clusters_heatmap_1",
                "Clusters heatmap 2 " = "select_clusters_heatmap_2",
                'Similarity network 1' = 'select_similarity_network_1',
                'Similarity network 2' = 'select_similarity_network_2',
                "Fusion heatmap" = "select_fusion_heatmap",
                'Similarity network fusion' = 'select_similarity_network_fusion'
  )
  return(plot_list)
}


#---------------------------------------------------------- Purge functions ----
purge_module_inputs = function(id, input_object) {
  base::invisible(
    lapply(grep(id, names(input_object), value = TRUE), function(i) {
      .subset2(input_object, "impl")$.values$remove(i)
    })
  )
}


#--------------------------------------------------------- Import functions ----

find_delim = function(path) {
  probe = paste(readLines(con = path, n = 10), collapse = "")
  sep = c("\t" = lengths(regmatches(probe, gregexpr("\t", probe))),
          "," = lengths(regmatches(probe, gregexpr(",", probe))),
          ";" = lengths(regmatches(probe, gregexpr(";", probe))))
  return(names(which.max(sep)))
}

soda_read_table = function(file_path, sep = NA) {

  if (is.na(sep)) {
    if (stringr::str_sub(file_path, -4, -1) == ".tsv") {
      sep = '\t'
    }
  }

  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table = as.data.frame(readxl::read_xlsx(file_path))
  } else {
    if (is.na(sep)) {
      sep = find_delim(path = file_path)
      data_table = read.csv(file_path,
                            header = T,
                            sep = sep,
                            check.names = FALSE)
    } else {
      data_table = read.csv(file_path,
                            header = T,
                            sep = sep,
                            check.names = FALSE)
    }

  }
  original_count = ncol(data_table)
  data_table = data_table[,!base::duplicated(colnames(data_table))]
  final_count = ncol(data_table)
  if(original_count != final_count) {
    print(paste0('Removed ', original_count - final_count, ' duplicated columns'))
  }

  return(data_table)
}

#-------------------------------------------------------- General utilities ----

is_coercible_to_numeric = function(vector) {
  numeric_values = suppressWarnings(as.numeric(vector))
  if (any(is.na(numeric_values))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

unique_na_rm = function(vector) {
  vector = vector[!is.na(vector)]
  vector = unique(vector)
  return(vector)
}

drop_rows = function(data_table, rows) {
  return(data_table[!(row.names(data_table) %in% rows),])
}

keep_rows = function(data_table, rows) {
  return(data_table[(row.names(data_table) %in% rows),])
}

drop_cols = function(data_table, cols) {
  return(data_table[,!(colnames(data_table) %in% cols)])
}

remove_empty_cols = function(table) {
  # filter out columns which are only NA
  del_cols = c()
  for (col in colnames(table)) {
    if (sum(is.na(table[,col])) == length(rownames(table))){
      del_cols = c(del_cols, col)
    }
  }
  if (!is.null(del_cols)) {
    table = table[,-which(colnames(table) %in% del_cols)]
  }
  return(table)
}

is_num_coercible = function(x) {
  all(grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$', x, perl = TRUE))
}

show_exp_info <- function(data = NULL, experiment = NULL) {
  # how are non samples annotated
  nonSamples = c("Blank", "Quality control Plasma", "Quality control Cells")
  # selection to get the correct info
  generalSelection = data$experimentId == experiment &
    !(data$cellType %in% nonSamples)

  # get all info
  infoSource = unique(data$source[generalSelection])
  infoSource = infoSource[!is.na(infoSource)]
  infoSource = paste(infoSource[!is.na(infoSource)],
                     collapse = " | ")
  infoCellType = unique(data$cellType[generalSelection])
  infoCellType = paste(infoCellType[!is.na(infoCellType)],
                       collapse = " | ")
  infoHarvestDate = unique(data$harvestDate[generalSelection])
  infoHarvestDate = paste(infoHarvestDate[!is.na(infoHarvestDate)],
                          collapse = " | ")
  infoGenoTypes = unique(data$genoType[generalSelection])
  infoGenoTypes = paste(infoGenoTypes[!is.na(infoGenoTypes)],
                        collapse = " | ")
  infoParentalCellLine = unique(data$parentalCellLine[generalSelection])
  infoParentalCellLine = paste(infoParentalCellLine[!is.na(infoParentalCellLine)],
                               collapse = " | ")
  infoCellLineName = unique(data$`cellLineName`[generalSelection])
  infoCellLineName = paste(infoCellLineName[!is.na(infoCellLineName)],
                           collapse = " | ")
  infoGender = unique(data$sex[generalSelection])
  infoGender = paste(infoGender[!is.na(infoGender)],
                     collapse = " | ")

  # generate the output
  exp_info = shiny::HTML(paste0("<h3> Information experiment: <i>", experiment, "</i></h3>"),
                         "<hr style=\"margin-left:0;max-width:50%\">",
                         "<ul>",
                         paste0("<li><b>Source :</b> ", infoSource, "</li>"),
                         paste0("<li><b>Cell type :</b> ", infoCellType, "</li>"),
                         paste0("<li><b>Harvest date :</b> ", infoHarvestDate, "</li>"),
                         paste0("<li><b>Genotype :</b> ", infoGenoTypes, "</li>"),
                         paste0("<li><b>Parental cell line :</b> ", infoParentalCellLine, "</li>"),
                         paste0("<li><b>Cell line name :</b> ", infoCellLineName, "</li>"),
                         paste0("<li><b>Gender :</b> ", infoGender, "</li>"),
                         "</ul>")

  return(exp_info)
}


#----------------------------------------------------- Print time functions ----

get_time = function() {
  return(format(Sys.time(), "%H:%M:%S"))
}

get_time_code = function() {
  return(format(Sys.time(), "%H%M%S"))
}

timestamped_name = function(file_name) {
  return(paste0(get_time_code(), '_', file_name))
}

print_t = function(in_print) {
  print(paste0(get_time(), " - ", in_print))
}

print_tm = function(m, in_print) {
  print(paste0(get_time(), " ", m, " - ", in_print))
}

#---------------------------------------------------------------- Plotboxes ----
# Plotly plotbox
get_plotly_box = function(id, label, dimensions_obj, session) {
  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "info",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    shiny::textOutput(outputId = ns(paste0(id, "_message"))),
    plotly::plotlyOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

# NetworkD3 plotbox
get_networkd3_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    networkD3::simpleNetworkOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}


# Visnet plotbox (for networks)
get_visnet_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    visNetwork::visNetworkOutput(outputId = ns(paste0(id, "_plot")),
                                 width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                 height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

# Regular plotbox (static plots)
get_plot_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    shiny::plotOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

#----------------------------------------------------- Lipidomics functions ----

get_group_median_table = function(data_table,
                                  meta_table,
                                  group_col) {
  unique_groups = unique(meta_table[, group_col])
  out_table = as.data.frame(matrix(data = NA,
                                   nrow = length(unique_groups),
                                   ncol = ncol(data_table)))
  colnames(out_table) = colnames(data_table)
  rownames(out_table) = unique_groups

  for (group in unique_groups) {
    idx = rownames(meta_table)[which(meta_table[,group_col] == group)]

    group_table = data_table[idx,]

    if (length(idx) == 1) {
      group_values = group_table
    } else {
      group_values = apply(group_table,2,median, na.rm = TRUE)
    }

    # group_values[is.na(group_values)] = 0.0
    group_values[group_values == 0] = NA
    out_table[group,] = group_values
  }
  return(out_table)
}

get_lipid_class_table = function(table){

  # Get unique lipid classes
  classes = get_lipid_classes(feature_list = colnames(table), uniques = TRUE)

  # Get a column vector to find easily which columns belong to each lipid group
  col_vector = get_lipid_classes(feature_list = colnames(table), uniques = FALSE)

  # Fill the table
  out_table = sapply(X = classes,
                     FUN = function(x) {
                       col_list = which(col_vector == x)
                       if (length(col_list) > 1) {
                         rowSums(table[,col_list], na.rm = T)
                       } else {
                         table[,col_list]
                       }
                     }
  )

  # Fix the TG, sum needs to be divided by 3, because they are measured 3 times.
  out_table[, "TG"] <- out_table[, "TG"] / 3

  return(out_table)
}

normalise_lipid_class = function(lips_table) {
  # Get classes and unique classes for the lipid features
  classes = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = FALSE)
  classes_unique = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = TRUE)

  # For each unique lipid class...
  for (lip_class in classes_unique){

    # Get columns from that class...
    cols = which(classes == lip_class)
    if (length(cols) > 1) {
      class_row_sums = rowSums(lips_table[, cols], na.rm = T)
    } else {
      class_row_sums = lips_table[, cols]
      class_row_sums[is.na(class_row_sums)] = 0
    }
    class_row_sums[class_row_sums == 0] = 1
    lips_table[, cols] = lips_table[, cols] / class_row_sums
  }
  return(lips_table)
}

z_score_normalisation = function(data_table) {

  data_table = apply(data_table, 2, function(col) {
    centered_row = col - base::mean(col, na.rm = T)
    scaled_row = centered_row / sd(centered_row, na.rm = T)
    return(scaled_row)
  })
  return(data_table)
}

impute_na = function(method, data_table, meta_table, group_col, sample_rownames, val_threshold) {
  imputation_func = method_switch(method)
  data_table = remove_empty_cols(data_table)
  groups = unique(meta_table[sample_rownames, group_col])
  for (g in groups) {
    g_rows = rownames(meta_table)[meta_table[, group_col] == g]
    g_rows = intersect(g_rows, sample_rownames)
    samples_total = length(g_rows)
    selected_features = colnames(data_table)[(colSums(!is.na(data_table[g_rows,])) / samples_total) >= val_threshold]
    for (f in selected_features){
      g_vector = data_table[g_rows,f]
      names(g_vector) = g_rows
      imputation_value = imputation_func(g_vector, na.rm = T)
      na_rownames = names(g_vector)[is.na(data_table[g_rows,f])]
      data_table[na_rownames,f] = imputation_value
    }
  }
  return(data_table)
}


get_lipid_classes = function(feature_list, uniques = TRUE){
  classes = sapply(feature_list, function(x)
    strsplit(x = x,
             split = " ",
             fixed = TRUE)[[1]][1])
  classes = as.vector(classes)
  if (uniques) {
    return(unique(classes))}
  else{
    return(classes)
  }
}

get_feature_metadata = function(data_table) {
  feature_table = data.frame(row.names = sort(colnames(data_table)))
  feature_table$lipid_class = get_lipid_classes(feature_list = rownames(feature_table),
                                                uniques = FALSE)
  # Collect carbon and unsaturation counts
  c_count_1 = c() # Main carbon count / total carbon count (TGs)
  s_count_1 = c() # Main saturation count
  c_count_2 = c() # Secondary carbon count (asyl groups or TGs)
  s_count_2 = c() # Secondary saturation (asyl groups or TGs)
  for (c in unique(feature_table$lipid_class)) {
    idx = rownames(feature_table)[feature_table$lipid_class == c]

    if (c == "TG") {
      # For triglycerides
      for (i in stringr::str_split(string = idx, pattern = " |:|-FA")) {
        c_count_1 = c(c_count_1, i[2])
        c_count_2 = c(c_count_2, i[4])
        s_count_1 = c(s_count_1, i[3])
        s_count_2 = c(s_count_2, i[5])
      }
    } else if (sum(stringr::str_detect(string = idx, pattern = "/|_")) >0) {
      # For species with asyl groups ("/" or "_")
      for (i in stringr::str_split(string = idx, pattern = " |:|_|/")) {
        c_count_1 = c(c_count_1, gsub("[^0-9]", "", i[2]))
        c_count_2 = c(c_count_2, i[4])
        s_count_1 = c(s_count_1, i[3])
        s_count_2 = c(s_count_2, i[5])
      }
    } else {
      # For the rest
      for (i in stringr::str_split(string = idx, pattern = " |:")) {
        c_count_1 = c(c_count_1, i[2])
        c_count_2 = c(c_count_2, 0)
        s_count_1 = c(s_count_1, i[3])
        s_count_2 = c(s_count_2, 0)
      }
    }
  }

  idx_tg <- feature_table$lipid_class == "TG"
  feature_table$carbons_1 = as.numeric(c_count_1)
  feature_table$carbons_2 = as.numeric(c_count_2)
  feature_table$carbons_sum[idx_tg] = feature_table$carbons_1[idx_tg]
  feature_table$carbons_sum[!idx_tg] = feature_table$carbons_1[!idx_tg] + feature_table$carbons_2[!idx_tg]
  feature_table$unsat_1 = as.numeric(s_count_1)
  feature_table$unsat_2 = as.numeric(s_count_2)
  feature_table$unsat_sum[idx_tg] = feature_table$unsat_1[idx_tg]
  feature_table$unsat_sum[!idx_tg] = feature_table$unsat_1[!idx_tg] + feature_table$unsat_2[!idx_tg]
  return(feature_table)
}

get_col_means = function(data_table) {
  means = colMeans(data_table, na.rm = TRUE)
  means[is.na(means)] = 0
  return(means)
}

blank_filter = function(data_table, blank_table, blank_multiplier, sample_threshold, saved_cols = FALSE) {
  # Find features / columns below threshold
  blank_means = get_col_means(data_table = blank_table)
  del_cols = c()
  total_samples = nrow(data_table)
  for (col in colnames(data_table)){
    threshold = blank_multiplier * blank_means[col]
    above_threshold = sum(data_table[, col] >= threshold, na.rm = T)
    if ((above_threshold/total_samples) < sample_threshold) {
      del_cols = c(del_cols, col)
    }
  }
  if (saved_cols) {
    del_cols = setdiff(colnames(data_table), del_cols)
  }
  return(del_cols)
}

# Implementation of blank filtering methods with r6 object
lips_get_del_cols = function(data_table,
                             blank_table,
                             imp_meta,
                             raw_meta,
                             idx_blanks,
                             idx_samples,
                             id_col_meta,
                             group_col,
                             batch_col,
                             blank_multiplier,
                             sample_threshold,
                             group_threshold) {
  # Blank filtering
  del_cols = c()
  all_batches = unique(imp_meta[, batch_col])
  for (b in all_batches) {
    batch_idx = which(imp_meta[, batch_col] == b)
    batch_blanks = base::intersect(batch_idx, idx_blanks)
    batch_samples = base::intersect(batch_idx, idx_samples)

    # Get rownames
    batch_blanks = rownames(imp_meta)[batch_blanks]
    batch_samples = rownames(imp_meta)[batch_samples]
    batch_samples = base::intersect(rownames(data_table), batch_samples)

    del_cols = c(del_cols, blank_filter(data_table = data_table[batch_samples,],
                                        blank_table = blank_table[as.character(batch_blanks),],
                                        blank_multiplier = blank_multiplier,
                                        sample_threshold = sample_threshold))
  }

  del_cols = unique(del_cols)
  del_cols = sort(del_cols)

  if (is.null(del_cols)) {
    return(del_cols)
  }

  # Group filtering
  saved_cols = c()
  for (g in unique(raw_meta[, group_col])) {
    group_idx = which(imp_meta[, group_col] == g)
    above_threshold = rep(0, length(del_cols))
    names(above_threshold) = del_cols
    for (b in unique(imp_meta[group_idx, batch_col])) {

      batch_idx = which(imp_meta[, batch_col] == b)
      batch_blanks = base::intersect(batch_idx, idx_blanks)
      batch_samples = base::intersect(batch_idx, group_idx)

      # Get rownames
      batch_blanks = rownames(imp_meta)[batch_blanks]
      batch_samples = rownames(imp_meta)[batch_samples]
      batch_samples = base::intersect(rownames(data_table), batch_samples)

      # get batch blank means
      blank_means = get_col_means(data_table = blank_table[batch_blanks, ])
      threshold = blank_multiplier * blank_means

      # Find features / columns below threshold
      for (col in del_cols) {
        above_threshold[col] = above_threshold[col] + sum(data_table[batch_samples,col] >= threshold[col], na.rm = T)
      }
    }
    above_threshold = above_threshold / length(group_idx) >= group_threshold
    saved_cols = c(saved_cols, names(above_threshold)[above_threshold])
  }

  saved_cols = unique(saved_cols)
  saved_cols = sort(saved_cols)

  del_cols = setdiff(del_cols, saved_cols)

  return(del_cols)
}

#------------------------------------------------------- Plotting functions ----

pca_plot_scores = function(x, y, meta_table, group_col, width, height, colour_list){
  groups = unique(meta_table[,group_col])
  fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
  i = 1
  for (grp in groups) {
    idx = rownames(meta_table)[which(meta_table[,group_col] == grp)]
    fig = fig %>% add_trace(x = x[idx], y = y[idx],
                            name = grp, color = colour_list[i],
                            type  = "scatter", mode = "markers",
                            text = idx,
                            hoverinfo = "text",
                            legendgroup=grp)
    i = i + 1
  }
  fig = fig %>% layout(shapes = list(hline(0),
                                     vline(0),
                                     circle(x, y)),
                       legend = list(title=list(text = paste0('<b>', group_col, ': </b>'))))
  return(fig)
}

pca_plot_loadings = function(x, y, feature_list, width, height, colour_list){
  fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
  fig = fig %>% add_trace(x = x, y = y,
                          type = "scatter", mode = "text", text = feature_list,
                          textposition = 'middle right', showlegend = F)

  shape_list = list(
    hline(0),
    vline(0)
  )


  for (i in 1:length(feature_list)) {
    feature = feature_list[i]
    new_line = list(
      type = "line",
      line = list(color = "pink"),
      xref = "x",
      yref = "y",
      x0 = 0,
      y0 = 0,
      x1 = x[i],
      y1 = y[i]
    )
    shape_list[[length(shape_list) + 1]] = new_line
  }

  fig = fig %>% layout(shapes = shape_list)

  return(fig)
}

hline = function(y = 0, color = "black", dash = NULL) {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash=dash)
  )
}

vline <- function(x = 0, color = "black", dash = NULL) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash=dash)
  )
}

#' @title Calculate Hoteling T2
#'
#' @description Calculate Hoteling T2 for the scores plot
#'
#' @param x numeric vector with x values
#' @param y numeric vector with y values
#' @param alpha numeric(1), confidence interval
#' @param len numeric(1), number of points to create the ellipse
#'
#' @return A list is returned to be used in a plotly graph.
#'
#' @details This is a helper function which is used to create a confidence (Hotelling T2) interval for a
#'     PCA score plot.
#'
#' @importFrom stats var qf
#'
#' @noRd
#'
#' @author Damien Olivier
circle = function(x, y, alpha = 0.95, len = 200){
  N = length(x)
  mypi = seq(0, 2 * pi, length = len)
  r1 = sqrt(stats::var(x) * stats::qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  r2 = sqrt(stats::var(y) * stats::qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  list(
    type = "circle",
    xref = "x",
    x0= -r1,
    x1 = r1,
    yref = "y",
    y0 = -r2,
    y1 = r2,
    line = "black",
    opacity = 0.2
  )
}

lipidomics_summary_plot = function(r6, data_table) {
  groups = get_lipid_classes(colnames(r6$tables$imp_data)[2:length(colnames(r6$tables$imp_data))], uniques = T)

  plot_table = data.frame(table(base::factor((get_lipid_classes(colnames(data_table)[2:length(colnames(data_table))], uniques = F)), levels = groups)))
  names(plot_table) = c("class", "raw")
  plot_table$imported = table(base::factor((get_lipid_classes(colnames(r6$tables$imp_data)[2:length(colnames(r6$tables$imp_data))], uniques = F)), levels = groups))
  plot_table$removed = plot_table$imported - plot_table$raw

  absolute_counts = as.data.frame(base::matrix(nrow = 2*nrow(plot_table)))
  absolute_counts$classes = c(plot_table$class, plot_table$class)
  absolute_counts$values = c(plot_table$removed, plot_table$raw)
  absolute_counts$status = c(rep('kept', nrow(plot_table)), rep('removed', nrow(plot_table)))
  absolute_counts$V1 = NULL

  relative_counts = absolute_counts
  relative_counts$values = round(100*(relative_counts$values/c(plot_table$imported, plot_table$imported)))

  plot_1 = ggplot(absolute_counts ,
                  aes(
                    fill=status ,
                    y=values ,
                    x=classes ,
                    label = values )) +
    ggtitle("Absolute compound count")+
    geom_bar(position="stack",
             stat="identity",
             show.legend = FALSE) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent')
    ) +
    scale_fill_manual(values = c("#D2E8F5", "#007bff"))+
    coord_flip() +
    scale_y_reverse(limits = c(max(plot_table$imported), 0))


  plot_2 = ggplot(relative_counts,
                  aes(
                    fill=status ,
                    y=values ,
                    x=classes ,
                    label = values )) +
    ggtitle("Relative compound count (%)")+
    geom_bar(position="stack",
             stat="identity",
             show.legend = FALSE) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_text(hjust = 0.4),
      axis.ticks.y=element_blank(),
      axis.ticks.x=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent')
    ) +
    scale_fill_manual(values = c("#D2E8F5", "#007bff"))+
    coord_flip()

  return(gridExtra::grid.arrange(plot_1, plot_2, ncol=2))
}


#--------------------------------------------------------------- Statistics ----

get_pca_data = function(data_table){

  pca_data = pcaMethods::pca(object = data_table,
                             nPcs = 2,
                             scale = "none",
                             cv = "q2",
                             completeObs = T)

  return(pca_data)
}

apply_discriminant_analysis = function(data_table, group_list, nlambda = 100, alpha = 0.8) {

  ncol_1 = ncol(data_table)
  data_table = data_table[,!is.na(colSums(data_table))]
  ncol_2 = ncol(data_table)
  if(ncol_2 != ncol_1) {
    print(paste0("Discriminant analysis : dropped ", ncol_1 - ncol_2, " features with no signal variation."))
  }

  count = table(group_list)
  if (any(count < 3)) {
    dead_groups = names(count)[count < 3]
    print(paste0("Warning: ", length(dead_groups), " groups with fewer than 3 samples, dropped from analysis."))
    data_table = data_table[!(group_list %in% dead_groups),]
    group_list = group_list[!(group_list %in% dead_groups)]
  }

  if(length(unique(group_list)) == 1) {
    stop("Heatmap: not enough groups (2) with at least 3 samples.")
  }

  if (length(unique(group_list) > 2)) {
    family = "multinomial"
  } else {
    family = "binomial"
  }

  coef = NULL
  attempt_count = 1
  while(is.null(coef)) {
    print(paste0("Discriminant analysis: attempt ", attempt_count))
    if (attempt_count == 5) {break}
    attempt_count = attempt_count + 1
    base::tryCatch(
      {
        coef = glmnet::cv.glmnet(data_table,
                                 group_list,
                                 nlambda = nlambda,
                                 alpha = alpha,
                                 family = family,
                                 type.multinomial = "grouped")

      },error=function(e){
      },finally={}
    )
  }

  coef = stats::coef(coef, s = "lambda.min")

  keep_cols = as.matrix(coef[[1]])

  keep_cols = rownames(keep_cols)[which(keep_cols != 0)]
  keep_cols = keep_cols[2:length(keep_cols)]
  data_table = data_table[,keep_cols]
  return(data_table)
}


get_fold_changes = function(data_table, idx_group_1, idx_group_2, used_function, impute_inf = T) {

  if (used_function == "median") {
    av_function = function(x) {return(median(x, na.rm = T))}
  } else {
    av_function = function(x) {return(mean(x, na.rm = T))}
  }


  fold_changes = apply(data_table, 2, function(column) {
    mean_group1 = av_function(column[idx_group_1])
    mean_group2 = av_function(column[idx_group_2])

    # Impute NA means with 0
    if (is.na(mean_group1)) mean_group1 = 0
    if (is.na(mean_group2)) mean_group2 = 0

    fold_change = mean_group2 / mean_group1

    return(fold_change)
  })

  if (impute_inf) {
    # Impute infinite (x/0)
    if (length(which(fold_changes == Inf)) > 0) {
      fold_changes[which(fold_changes == Inf)] = max(fold_changes[which(fold_changes != Inf)]) * 1.01
    }

    # Impute zeros (0/x)
    if (length(which(fold_changes == 0)) > 0) {
      fold_changes[which(fold_changes == 0)] = min(fold_changes[which(fold_changes > 0)]) * 0.99
    }
  }

  # if x/0 the fold change should be -Inf
  fold_changes[fold_changes == 0] <- -Inf

  # Impute NaNs (0/0)
  if (length(which(is.nan(fold_changes)) > 0)) {
    fold_changes[which(is.nan(fold_changes))] = 1
  }

  return(fold_changes)
}

get_p_val = function(data_table, idx_group_1, idx_group_2, used_function, impute_na = T) {
  # define the function for testing
  if (used_function == "Wilcoxon") {
    test_function = function(x, y){

      if(all(is.na(x)) | all(is.na(y))) {
        # if one group contains only NA's
        if(all(is.na(x))) {
          x <- y
        }
        return(stats::wilcox.test(x)$p.value)
      } else if(sum(!is.na(x)) < 2 | sum(!is.na(y)) < 2) {
        # if one of the groups doesn't contain enough data
        return(NA)
      } else if(all(x == mean(x, na.rm = T)) & all(y == mean(y, na.rm = T))) {
        return(1)
      } else {
        return(stats::wilcox.test(x, y)$p.value)
      }
    }
  } else if (used_function == "t-Test") {
    test_function = function(x, y){

      # if(all(x == mean(x, na.rm = T)) & all(y == mean(y, na.rm = T))) {
      #   return(1)
      # } else
      if(all(is.na(x)) | all(is.na(y))) {
        # if one group contains only NA's
        if(all(is.na(x))) {
          x <- y
        }
        return(stats::t.test(x)$p.value)
      } else if(sum(!is.na(x)) < 2 | sum(!is.na(y)) < 2) {
        # if one of the groups doesn't contain enough data
        return(NA)
      } else if(all(x == mean(x, na.rm = T)) & all(y == mean(y, na.rm = T))) {
        return(1)
      } else {
        return(stats::t.test(x, y)$p.value)
      }
    }

  }

  p_values = apply(data_table, 2, function(column) {
    group1 = column[idx_group_1]
    group2 = column[idx_group_2]

    test_result = test_function(group1, group2)  # Assuming equal variance

    return(test_result)
  })


  if ((length(which(is.na(p_values))) > 0) & impute_na){
    p_values[which(is.na(p_values))] = min(p_values, na.rm = T) * 0.99
  }

  return(p_values)
}



get_fc_and_pval = function(data_table, idx_group_1, idx_group_2, used_function, test){

  if (used_function == "median") {
    av_function = function(x) {return(median(x, na.rm = T))}
  } else {
    av_function = function(x) {return(mean(x, na.rm = T))}
  }

  if (test == "Wilcoxon") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T))&all(y==mean(y, na.rm = T))) {
        return(1)
      } else{
        return(stats::wilcox.test(x, y)$p.value)
      }
    }
  } else if (test == "t-Test") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T))&all(y==mean(y, na.rm = T))) {
        return(1)
      } else{
        return(stats::t.test(x, y)$p.value)
      }
    }

  }

  # Collect fold change and p-values
  fold_change = c()
  p_value = c()

  sorted_cols = sort(colnames(data_table))

  for (col in sorted_cols) {

    # If both groups contain data
    if (length(na.exclude(data_table[idx_group_1, col])) > 0 & length(na.exclude(data_table[idx_group_2, col])) > 0) {

      # If at least one of the groups contains only one value
      if ((length(na.exclude(data_table[idx_group_1, col])) == 1) | (length(na.exclude(data_table[idx_group_2, col])) == 1)) {
        fold_change = c(fold_change, av_function(data_table[idx_group_2, col]) / av_function(data_table[idx_group_1, col]))
        p_value = c(p_value, NA)
      } else {

        # If there is actual comparable data
        fold_change = c(fold_change, av_function(data_table[idx_group_2, col]) / av_function(data_table[idx_group_1, col]))
        p_value = c(p_value, test_function(data_table[idx_group_1, col], data_table[idx_group_2, col]))
      }

    } else {
      # If at least one of the groups is full NA, default values
      p_value = c(p_value, 666)
      # For fold changes, if it is the denominator
      if (length(na.exclude(data_table[idx_group_1, col])) == 0) {
        fold_change = c(fold_change, 777)
      } else {
        # If it is the numerator
        fold_change = c(fold_change, 666)
      }
    }
  }

  # Imputation of Inf for when denominator average is 0
  fold_change[fold_change == Inf] = 1.01*max(fold_change[!(fold_change == 777) & !(fold_change == 666) & !(fold_change == Inf)], na.rm = T)

  # Imputation of 0 for when numerator average is 0
  fold_change[fold_change == 0] = 0.99*min(fold_change[!(fold_change == 0)], na.rm = T)

  # Imputation of NAs for denominator FC with a value slightly above max FC
  fold_change[fold_change == 777] = 1.01*max(fold_change[!(fold_change == 777) & !(fold_change == 666) & !(fold_change == Inf)], na.rm = T)

  # Imputation of NAs for nominator FC with a value slightly below min FC
  fold_change[fold_change == 666] = 0.99*min(fold_change[!(fold_change == 0)], na.rm = T)

  # Imputation of NAs for when both numerators and denominator medians are 0
  fold_change[is.na(fold_change)] = 1

  # Imputation of NAs for p-values to be the min p-val
  p_value[p_value == 666] = 0.99*min(p_value, na.rm = T)

  # Adjust p-value
  p_value_bh_adj = p.adjust(p_value, method = "BH")

  return(list("fold_change" = fold_change,
              "p_value" = p_value,
              "p_value_bh_adj" = p_value_bh_adj))
}


#------------------------------------------------------------ heatmap stuff ----
calc_subplot_size <- function(dendrogram = c("both", "row", "column", "none"),
                              cluster_rows = NULL,
                              cluster_columns = NULL,
                              factor_height = 1) {
  subplot <- vector(mode = "list",
                    length = 2)
  names(subplot) <- c("width", "height")

  # define width and height for the color annotation bars
  width_ann <- 0.02
  height_ann <- 0.04 / factor_height

  # define width and height for the dendrograms
  width_dend <- switch(
    dendrogram,
    "both" = 0.05,
    "row" = 0.05,
    "column" = 0,
    "none" = 0
  )
  height_dend <- switch(
    dendrogram,
    "both" = 0.075 / factor_height,
    "row" = 0,
    "column" = 0.075 / factor_height,
    "none" = 0
  )

  # calculate the size of the subplots
  subplot$width <- c(1 - width_dend - (length(cluster_columns) * width_ann),
                     length(cluster_columns) * width_ann,
                     width_dend)
  subplot$height <- c(height_dend,
                      length(cluster_rows) * height_ann,
                      1 - height_dend - (length(cluster_rows) * height_ann))

  # remove any zero's
  subplot$width <- subplot$width[subplot$width != 0]
  subplot$height <- subplot$height[subplot$height != 0]

  return(subplot)
}


#------------------------------------------------------ Fatty acid analysis ----
fa_analysis_calc <- function(data_table = NULL,
                             feature_table = NULL,
                             sample_meta = NULL,
                             selected_lipidclass = NULL,
                             fa_norm = FALSE) {
  ## Features
  feature_table$lipid <- rownames(feature_table)

  # fix TG's
  idx_tg <- feature_table$lipid[feature_table$lipid_class == "TG"]
  data_table[, idx_tg] <- data_table[, idx_tg] / 3

  # get the species from the selected lipid classes
  if(selected_lipidclass == "All") {
    # all lipids, but remove PA
    sel_feat_idx <- feature_table$lipid[!(feature_table$lipid_class %in% c("PA"))]
  } else if(selected_lipidclass == "All_noTG") {
    # all lipids, but remove PA
    sel_feat_idx <- feature_table$lipid[!(feature_table$lipid_class %in% c("PA", "TG"))]
  } else {
    sel_feat_idx <- feature_table$lipid[feature_table$lipid_class %in% selected_lipidclass]
  }
  sel_feature_table <- feature_table[feature_table$lipid %in% sel_feat_idx, ]

  ## Data
  # select the correct data
  sel_data_table <- data_table[, sel_feat_idx]

  # get the unique chain lengths and unsaturation
  uniq_carbon <- sort(union(unique(sel_feature_table$carbons_1[sel_feature_table$lipid_class != "TG"]),
                            unique(sel_feature_table$carbons_2)))
  uniq_carbon <- uniq_carbon[uniq_carbon != 0]
  uniq_unsat <- sort(union(unique(sel_feature_table$unsat_1[sel_feature_table$lipid_class != "TG"]),
                           unique(sel_feature_table$unsat_2)))

  # Initialize results data.frame
  fa_chains <- expand.grid(uniq_unsat, uniq_carbon)
  fa_chains <- paste(fa_chains[, 2], fa_chains[, 1], sep = ":")
  res <- as.data.frame(matrix(ncol = length(fa_chains),
                              nrow = nrow(sel_data_table)))
  colnames(res) <- fa_chains
  rownames(res) <- rownames(sel_data_table)

  # do the calculations
  for(a in uniq_carbon) {
    for(b in uniq_unsat) {
      sel_fa_chain <- paste(a, b, sep = ":")
      sel_lipids <- sel_feature_table$lipid[(sel_feature_table$carbons_1 == a &
                                               sel_feature_table$unsat_1 == b) |
                                              (sel_feature_table$carbons_2 == a &
                                                 sel_feature_table$unsat_2 == b)]
      sel_lipids_double <- sel_feature_table$lipid[(sel_feature_table$carbons_1 == a &
                                                      sel_feature_table$unsat_1 == b) &
                                                     (sel_feature_table$carbons_2 == a &
                                                        sel_feature_table$unsat_2 == b)]

      res[, sel_fa_chain] <- `+`(
        rowSums(sel_data_table[, sel_lipids, drop = FALSE], na.rm = TRUE),
        rowSums(sel_data_table[, sel_lipids_double, drop = FALSE], na.rm = TRUE)
      )
    }
  }

  # remove empty columns
  empty_idx <- apply(res, 2, function(x) {
    all(x == 0)
  })
  res <- res[, !empty_idx]

  # normalise by total FA's
  if(fa_norm) {
    res <- res / rowSums(res, na.rm = TRUE)
  }

  return(res)
}


fa_analysis_rev_calc <- function(data_table = NULL,
                                 feature_table = NULL,
                                 sample_meta = NULL,
                                 selected_fa = NULL,
                                 fa_norm = FALSE) {
  uniq_lipid_classes <- unique(feature_table$lipid_class[!(feature_table$lipid_class %in% c("PA"))])

  ## Features
  feature_table$lipid <- rownames(feature_table)

  sel_feat_idx <- feature_table$lipid[!(feature_table$lipid_class %in% c("PA"))]
  sel_feature_table <- feature_table[feature_table$lipid %in% sel_feat_idx, ]

  ## Data
  # select the correct data
  sel_data_table <- data_table[, sel_feat_idx]

  # Initialize results data.frame
  res <- as.data.frame(matrix(ncol = length(uniq_lipid_classes),
                              nrow = nrow(sel_data_table)))
  colnames(res) <- uniq_lipid_classes
  rownames(res) <- rownames(sel_data_table)

  # do the calculations
  fa_norm_tot <- 0
  for(lipid_class in uniq_lipid_classes) {
    for(fa_tail in selected_fa) {
      split_fa <- as.numeric(unlist(strsplit(fa_tail,
                                             split = ":",
                                             fixed = TRUE)))
      sel_lipids <- sel_feature_table$lipid[sel_feature_table$lipid_class == lipid_class &
                                              ((sel_feature_table$carbons_1 == split_fa[1] &
                                                  sel_feature_table$unsat_1 == split_fa[2]) |
                                                 (sel_feature_table$carbons_2 == split_fa[1] &
                                                    sel_feature_table$unsat_2 == split_fa[2]))]
      sel_lipids_double <- sel_feature_table$lipid[sel_feature_table$lipid == lipid_class &
                                                     (sel_feature_table$carbons_1 == split_fa[1] &
                                                        sel_feature_table$unsat_1 == split_fa[2]) &
                                                     (sel_feature_table$carbons_2 == split_fa[1] &
                                                        sel_feature_table$unsat_2 == split_fa[2])]

      res[, lipid_class] <- rowSums(sel_data_table[, c(sel_lipids, sel_lipids_double), drop = FALSE], na.rm = TRUE)
    } # end selected_fa
  } # end lipid_class

  # fix the TG's
  res[, "TG"] <- res[, "TG"] / 3

  # remove empty columns
  empty_idx <- apply(res, 2, function(x) {
    all(x == 0)
  })
  res <- res[, !empty_idx]

  # get rid of the zero's
  res[res == 0] <- NA

  # normalise by total FA's
  if(fa_norm) {
    res <- res / rowSums(res, na.rm = TRUE)
  }

  return(res)
}

#--------------------------------------------------------- FA composition   ----
fa_comp_hm_calc <- function(data_table = NULL,
                            feature_table = NULL,
                            group_col = NULL,
                            selected_group = NULL,
                            sample_meta = NULL,
                            selected_lipidclass = NULL) {
  ## samples
  idx_samples <- rownames(sample_meta)[sample_meta[, group_col] == selected_group]
  hm_data <- data_table[idx_samples, , drop = FALSE]

  ## features
  feature_table$lipid <- rownames(feature_table)
  if(selected_lipidclass == "All") {
    selected_features <- feature_table
  } else {
    selected_features <- feature_table[feature_table$lipid_class == selected_lipidclass, ]
  }
  # get the unique chain lengths and unsaturation
  uniq_carbon <- c(min(selected_features$carbons_sum), max(selected_features$carbons_sum))
  uniq_unsat <- c(min(selected_features$unsat_sum), max(selected_features$unsat_sum))

  ## calculations
  # initialize result matrix
  res <- matrix(ncol = length(uniq_carbon[1]:uniq_carbon[2]),
                nrow = length(uniq_unsat[1]:uniq_unsat[2]))
  colnames(res) <- uniq_carbon[1]:uniq_carbon[2]
  rownames(res) <- uniq_unsat[1]:uniq_unsat[2]
  for(a in rownames(res)) { # unsaturation
    for(b in colnames(res)) { # carbons
      idx_lipids <- selected_features$lipid[selected_features$carbons_sum == b &
                                              selected_features$unsat_sum == a]
      if(length(idx_lipids) > 0) {
        res[a, b] <- sum(hm_data[, idx_lipids], na.rm = TRUE)
      } else {
        res[a, b] <- 0
      }
    }
  }

  # calculate the proportion
  res <- res / sum(res)

  return(res)
}


fa_comp_heatmap <- function(data = NULL,
                            hline = NULL,
                            vline = NULL,
                            color_limits = NULL,
                            color_palette = NULL,
                            y_pos_right = FALSE,
                            showlegend = FALSE) {
  # prepare data
  data_df <- as.data.frame(data)
  data_df$row <- rownames(data)

  data_df <- data_df |>
    tidyr::pivot_longer(cols = -row,
                        names_to = "col",
                        values_to = "value")
  data_df$row <- as.numeric(data_df$row)
  data_df$col <- as.numeric(data_df$col)

  # make heatmap
  fig <- plotly::plot_ly(data = data_df,
                         x = ~col,
                         y = ~row,
                         z = ~value,
                         type = "heatmap",
                         colors = color_palette,
                         hovertemplate = paste(
                           "Total carbons: %{x:d}<br>",
                           "Total double bond: %{y:d}<br>",
                           "Proportion: %{z:.3f}",
                           "<extra></extra>"
                         )) |>
    plotly::colorbar(limits = color_limits,
                     title = "Proportion") |>
    plotly::style(xgap = 3,
                  ygap = 3)

  if(!showlegend) {
    fig <- fig |>
      plotly::hide_colorbar()
  }
  fig <- fig |>
    # vertical line
    plotly::add_segments(
      x = vline,
      xend = vline,
      y = min(data_df$row) - 0.5,
      yend = max(data_df$row) + 0.5,
      inherit = FALSE,
      line = list(color = "black",
                  width = 2,
                  dash = "dot"),
      showlegend = FALSE
    ) |>
    # horizotal line
    plotly::add_segments(
      x = min(data_df$col) - 0.5,
      xend = max(data_df$col) + 0.5,
      y = hline,
      yend = hline,
      inherit = FALSE,
      line = list(color = "black",
                  width = 2,
                  dash = "dot"),
      showlegend = FALSE
    ) |>
    plotly::layout(
      font = list(
        size = 9
      ),
      xaxis = list(
        tick0 = 1,
        dtick = 1,
        zeroline = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE,
        ticklen = 3,
        title = list(
          text = "Number of carbon atoms",
          standoff = 5,
          font = list(
            size = 10
          )
        )
      )
    ) |>
    plotly::add_annotations(
      x = c(max(data_df$col), vline),
      y = c(hline, max(data_df$row)),
      text = c(sprintf("Avg. %0.1f", hline), sprintf("Avg. %0.1f", vline)),
      font = list(size = 10),
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      xanchor = c("right", "left"),
      yanchor = c("bottom", "middle")
    )

  if(y_pos_right) {
    fig <- fig |>
      plotly::layout(
        font = list(
          size = 9
        ),
        yaxis = list(
          tick0 = 1,
          dtick = 1,
          zeroline = FALSE,
          showgrid = FALSE,
          range = c(max(data_df$row) + 0.5, min(data_df$row) - 0.5),
          side = "right",
          fixedrange = TRUE,
          ticklen = 3,
          title = list(
            text = "Number of double bonds",
            standoff = 3,
            font = list(
              size = 10
            )
          )
        )
      )
  } else {
    fig <- fig |>
      plotly::layout(
        font = list(
          size = 9
        ),
        yaxis = list(
          tick0 = 1,
          dtick = 1,
          zeroline = FALSE,
          showgrid = FALSE,
          range = c(max(data_df$row) + 0.5, min(data_df$row) - 0.5),
          fixedrange = TRUE,
          ticklen = 3,
          title = list(
            text = "Number of double bonds",
            standoff = 3,
            font = list(
              size = 10
            )
          )
        )
      )
  }

  return(fig)
}

#--------------------------------------------------------- Input validation ----
iv_check_select_input <- function(value, choices, name_plot, message) {
  if(!all(value %in% choices)) {
    print_tm(name_plot, message)
  }
}

iv_check_numeric_input <- function(value, check_range, name_plot, message) {
  if(!is.numeric(as.numeric(value)) |
     as.numeric(value) < check_range[1] |
     as.numeric(value) > check_range[2]) {
    print_tm(name_plot, message)
  }
}

iv_check_numeric_range <- function(value, check_range, name_plot, message) {
  if(!all(is.numeric(as.numeric(value))) |
     as.numeric(value[1]) < check_range[1] |
     as.numeric(value[2]) > check_range[2]) {
    print_tm(name_plot, message)
  }
}

#--------------------------------------------------------- QC stuff ------------
qc_histogram <- function(data = NULL,
                         title = NULL) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = rsd)) +
    ggplot2::geom_histogram(binwidth = 0.005) +
    ggplot2::geom_vline(xintercept = 0.3,
                        color = "red",
                        linetype = 2) +
    ggplot2::labs(title = title,
                  x = "Relative standard deviation") +
    ggplot2::theme_minimal()

  ply <- plotly::ggplotly(p)

  return(ply)
}

qc_trend_plot <- function(data = NULL,
                          title = NULL) {
  # get the lipid class
  data$lipidclass <- gsub(x = data$lipid,
                          pattern = "^([a-zA-Z]*) .*",
                          replacement = "\\1")

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = ID,
                                 y = log2fc,
                                 group = lipid,
                                 color = lipidclass)) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::geom_point(size = 1,
                        alpha = 0.3) +
    ggplot2::geom_hline(yintercept = c(-1, 1),
                        color = "red",
                        linetype = 2) +
    ggplot2::labs(title = title,
                  x = "Sample ID",
                  y = "log2(fold change)") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Lipid class",
                                                  nrow = 2,
                                                  override.aes = list(alpha = 1))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  ply <- plotly::ggplotly(p)

  ply <- plotly::layout(ply,
                        xaxis = list(tickangle = 45))

  return(ply)
}

qc_prep_trend <- function(data = NULL) {
  qc_data <- data |>
    tidyr::pivot_longer(cols = -ID,
                        names_to = "lipid",
                        values_to = "value")

  ref_qc <- qc_data[qc_data$ID == sort(qc_data$ID)[1], ]
  colnames(ref_qc)[3] <- "ref_value"

  qc_data <- merge(
    x = qc_data,
    y = ref_qc[, c("lipid", "ref_value")],
    by = "lipid",
    all.x = TRUE
  )

  qc_data$log2fc <- log2(qc_data$value / qc_data$ref_value)

  return(qc_data)
}

qc_rsd_violin <- function(data = NULL,
                          title = NULL) {
  data$lipidclass <- gsub(x = data$lipid,
                          pattern = "^([a-zA-Z]*) .*",
                          replacement = "\\1")

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = lipidclass,
                                 y = rsd)) +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(size = 1,
                         alpha = 0.5,
                         width = 0.1) +
    ggplot2::geom_hline(yintercept = 0.3,
                        color = "red",
                        linetype = 2) +
    ggplot2::labs(title = title,
                  x = "Lipid class",
                  y = "Relative standard deviation") +
    ggplot2::theme_minimal()

  ply <- plotly::ggplotly(p)

  ply <- plotly::layout(ply,
                        xaxis = list(tickangle = 45))

  return(ply)
}
#--------------------------------------------------------- Example datasets ----
example_lipidomics = function(name,
                              id = NA,
                              slot = NA,
                              experiment_id = NULL) {
  # get the meta data
  # set the interesting columns
  meta_columns <- c("experimentTitle", "experimentId", "defaultColumn", "batchNumber",
                    "processDate", "experimentIdOrg", "analystId", "sampleId",
                    "referenceGroup", "sampleReferral", "harvestDate", "sampleType",
                    "genoType", "parentCellLineBrainregion", "cellLineName", "sex", "cultureConditions",
                    "treatmentDiagnosis", "notes", "lab", "Machine")
  meta_data = soda_read_table(file.path("data", "Database", "SampleMasterfile.xlsx"))
  data_files = unique(meta_data$batchNumber[meta_data$experimentId == experiment_id])
  data_files = data_files[!is.na(data_files)]
  if(length(data_files) > 0) {
    meta_data = meta_data[meta_data$batchNumber %in% data_files &
                            (meta_data$experimentId %in% experiment_id |
                               meta_data$experimentId %in% data_files), meta_columns]
    rownames(meta_data) <- paste(meta_data[, "batchNumber"], meta_data[, "analystId"], sep = "_")
    # create a new column for the blank group filtering, initialize with NA
    meta_data$group_col_blank <- NA

    # get the lipid data
    data_tables <- vector("list", length = length(data_files))
    for(a in 1:length(data_files)) {
      file_path = file.path("data", "Database", data_files[a], paste0(data_files[a], "_output_merge.xlsx"))
      data_tables[[a]] = soda_read_table(file_path = file_path)
      data_tables[[a]][, "rownames"] <- paste(data_files[a], data_tables[[a]][, "ID"], sep = "_")
    }
    lips_data = Reduce(function(x, y) merge(x, y, all = TRUE), data_tables)
    rownames(lips_data) <- lips_data[, "rownames"]
    lips_data[, "rownames"] <- NULL

    # The imported data needs to be filtered because sometimes a batch consist out of multiple experiments
    lips_data = lips_data[lips_data[, "ID"] %in% meta_data[, "analystId"], ]

    # create the r6 object
    r6 = Lips_exp$new(name = name,
                      id = id,
                      slot = slot,
                      preloaded = TRUE,
                      data_file = data_files,
                      experiment_id = experiment_id)

    r6$tables$imp_meta = meta_data
    r6$tables$imp_data = lips_data

    r6$indices$id_col_meta = 'analystId'
    r6$indices$id_col_data = 'ID'

    r6$indices$group_col = unique(meta_data$defaultColumn)[1]
    r6$indices$batch_col = 'batchNumber'
    r6$set_raw_meta()

    type_vector = r6$tables$imp_meta[, "sampleType"]
    blank_idx = grep(pattern = 'blank',
                     x = type_vector,
                     ignore.case = TRUE)
    qc_idx = grep(pattern = 'Quality',
                  x = type_vector,
                  ignore.case = TRUE)
    pool_idx = grep(pattern = 'Pool',
                    x = type_vector,
                    ignore.case = TRUE)

    sample_idx = 1:nrow(r6$tables$imp_meta)
    sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

    r6$indices$idx_blanks = blank_idx
    r6$indices$idx_qcs = qc_idx
    r6$indices$idx_pools = pool_idx
    r6$indices$idx_samples = sample_idx

    r6$indices$rownames_blanks = rownames(r6$tables$imp_meta)[blank_idx]
    r6$indices$rownames_qcs = rownames(r6$tables$imp_meta)[qc_idx]
    r6$indices$rownames_pools = rownames(r6$tables$imp_meta)[pool_idx]
    r6$indices$rownames_samples = rownames(r6$tables$imp_meta)[sample_idx]

    r6$tables$raw_meta = r6$tables$raw_meta[r6$indices$rownames_samples, ]
    # create the new groups for the blank group filtering
    r6$tables$raw_meta[, "group_col_blank"] <- tolower(
      paste(
        r6$tables$raw_meta[, "sampleType"],
        r6$tables$raw_meta[, "genoType"],
        r6$tables$raw_meta[, "treatmentDiagnosis"],
        r6$tables$raw_meta[, "parentCellLineBrainregion"],
        # r6$tables$raw_meta[, "sex"],
        r6$tables$raw_meta[, "cultureConditions"],
        sep = "_")
    )

    # extract the blanks and the qc samples
    r6$get_blank_table()
    r6$get_qc_table()

    r6$set_raw_data(apply_imputation = FALSE,
                    impute_before = FALSE,
                    apply_filtering = TRUE,
                    imputation_function = 'minimum',
                    val_threshold = 0.6,
                    blank_multiplier = 2,
                    sample_threshold = 0.8,
                    group_threshold = 0.6,
                    norm_col = '')

    r6$derive_data_tables()

    # set which variables are available for colering
    idx_meta <- apply(r6$tables$raw_meta[, r6$hardcoded_settings$meta_column], 2, function(x) {
      length(unique(x)) >= 2
    })

    r6$hardcoded_settings$meta_column <- r6$hardcoded_settings$meta_column[idx_meta]
  } else {
    r6 <- Lips_exp$new(name = "Error",
                       id = id,
                       slot = slot,
                       preloaded = TRUE,
                       data_file = data_files,
                       experiment_id = experiment_id)
  }

  return(r6)
}

example_proteomics = function(name = 'prot_example', id = NA, slot = NA, data = './examples/multiomics/proteomics_2.tsv', meta = './examples/multiomics/metadata.csv') {
  prot_data = soda_read_table(data)
  meta_data = soda_read_table(meta)

  r6 = Prot_exp$new(name = name, id = id, slot = slot, preloaded = T)

  r6$tables$imp_meta = meta_data
  r6$tables$imp_data = prot_data

  r6$indices$id_col_meta = 'ID'
  r6$indices$id_col_data = 'ID'

  r6$indices$group_col = 'Group_type'
  r6$indices$batch_col = 'Batch'
  r6$set_raw_meta()

  type_vector = r6$tables$imp_meta[, 'Sample_type']
  blank_idx = grep(pattern = 'blank',
                   x = type_vector,
                   ignore.case = TRUE)
  qc_idx = grep(pattern = 'QC',
                x = type_vector,
                ignore.case = TRUE)
  pool_idx = grep(pattern = 'Pool',
                  x = type_vector,
                  ignore.case = TRUE)

  sample_idx = 1:nrow(r6$tables$imp_meta)
  sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

  r6$indices$idx_blanks = blank_idx
  r6$indices$idx_qcs = qc_idx
  r6$indices$idx_pools = pool_idx
  r6$indices$idx_samples = sample_idx

  r6$indices$rownames_blanks = r6$tables$imp_meta[blank_idx, r6$indices$id_col_meta]
  r6$indices$rownames_qcs = r6$tables$imp_meta[qc_idx, r6$indices$id_col_meta]
  r6$indices$rownames_pools = r6$tables$imp_meta[pool_idx, r6$indices$id_col_meta]
  r6$indices$rownames_samples = r6$tables$imp_meta[sample_idx, r6$indices$id_col_meta]

  r6$tables$raw_meta = r6$tables$raw_meta[r6$indices$rownames_samples,]

  r6$get_blank_table()

  r6$set_raw_data(apply_imputation = F,
                  impute_before = F,
                  apply_filtering = F,
                  imputation_function = 'minimum',
                  val_threshold = 0.6,
                  blank_multiplier = 2,
                  sample_threshold = 0.8,
                  group_threshold = 0.8,
                  norm_col = '')

  r6$derive_data_tables()

  # r6$get_prot_list()
  # r6$get_gsea_object()

  return(r6)
}

example_transcriptomics = function(name = 'trns_example', id = NA, slot = NA, data = './examples/multiomics/transcriptomics_2_genename_test.tsv', meta = './examples/multiomics/metadata.csv') {
  trns_data = soda_read_table(data)
  meta_data = soda_read_table(meta)

  r6 = Trns_exp$new(name = name, id = id, slot = slot, preloaded = T)

  r6$tables$imp_meta = meta_data
  r6$tables$imp_data = trns_data

  r6$indices$id_col_meta = 'ID'
  r6$indices$id_col_data = 'ID'

  r6$indices$group_col = 'Group_type'
  r6$indices$batch_col = 'Batch'
  r6$set_raw_meta()

  type_vector = r6$tables$imp_meta[, 'Sample_type']
  blank_idx = grep(pattern = 'blank',
                   x = type_vector,
                   ignore.case = TRUE)
  qc_idx = grep(pattern = 'QC',
                x = type_vector,
                ignore.case = TRUE)
  pool_idx = grep(pattern = 'Pool',
                  x = type_vector,
                  ignore.case = TRUE)

  sample_idx = 1:nrow(r6$tables$imp_meta)
  sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

  r6$indices$idx_blanks = blank_idx
  r6$indices$idx_qcs = qc_idx
  r6$indices$idx_pools = pool_idx
  r6$indices$idx_samples = sample_idx

  r6$indices$rownames_blanks = r6$tables$imp_meta[blank_idx, r6$indices$id_col_meta]
  r6$indices$rownames_qcs = r6$tables$imp_meta[qc_idx, r6$indices$id_col_meta]
  r6$indices$rownames_pools = r6$tables$imp_meta[pool_idx, r6$indices$id_col_meta]
  r6$indices$rownames_samples = r6$tables$imp_meta[sample_idx, r6$indices$id_col_meta]

  r6$tables$raw_meta = r6$tables$raw_meta[r6$indices$rownames_samples,]

  r6$get_blank_table()

  r6$set_raw_data(apply_imputation = F,
                  impute_before = F,
                  apply_filtering = F,
                  imputation_function = 'minimum',
                  val_threshold = 0.6,
                  blank_multiplier = 2,
                  sample_threshold = 0.8,
                  group_threshold = 0.8,
                  norm_col = '')

  r6$derive_data_tables()

  # r6$get_prot_list()
  # r6$get_gsea_object()

  return(r6)
}
