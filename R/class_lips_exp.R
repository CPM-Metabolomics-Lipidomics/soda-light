#---------------------------------------------- Lipidomics experiment class ----
Lips_exp = R6::R6Class(
  "Lips_exp",
  public = list(
    initialize = function(name, id = NA, slot = NA, preloaded = F, data_file, experiment_id){
      self$name = name
      self$id = id
      self$slot = slot
      self$preloaded_data = preloaded
      self$data_file = data_file
      self$experiment_id = experiment_id
    },
    #--------------------------------------------------------------- Global ----
    name = NA,
    id = NA,
    slot = NA,
    type = 'Lipidomics',
    preloaded_data = F,
    data_file = NA,
    experiment_id = NA,

    #----------------------------------------------------------- Parameters ----
    params = list(
      # Class distribution parameters
      class_distribution = list(
        dataset = 'Class table total normalized',
        group_col = NULL,
        color_palette = 'Spectral',
        img_format = "png"
      ),

      # Class comparison parameters
      class_comparison = list(
        dataset = 'Class table total normalized',
        group_col = NULL,
        img_format = "png"
      ),

      # Volcano plot parameters self$params$volcano_plot$
      volcano_plot = list(
        data_table = 'Total normalized table',
        adjustment = "BH",
        group_col = NULL,
        group_1 = NULL,
        group_2 = NULL,
        feature_metadata = "lipid_class",
        displayed_plot = "all",
        p_val_threshold = 0.05,
        fc_threshold = 2,
        marker_size = 8,
        opacity = 1,
        color_palette = 'Spectral',
        selected_function = "mean",
        selected_test = "t-Test",
        img_format = "png"
      ),

      # Heatmap parameters self$params$heatmap$
      heatmap = list(
        dataset = 'Z-scored total normalized table',
        impute = TRUE,
        cluster_samples = TRUE,
        cluster_features = TRUE,
        map_sample_data = NULL,
        map_feature_data = "lipid_class",
        group_column_da = NULL,
        apply_da = FALSE,
        alpha_da = 0.8,
        factor_height = 2,
        img_format = "png"
      ),

      # PCA parameters self$params$pca$
      pca = list(
        data_table = 'z_scored_total_norm_data',
        sample_groups_col = NULL,
        sample_groups_col_shape = NULL,
        feature_groups_col = NULL,
        apply_da = FALSE,
        alpha_da = 0.8,
        pca_method = 'svd',
        nPcs = 10,
        displayed_pc_1 = 1,
        displayed_pc_2 = 2,
        completeObs = F,
        displayed_plots = 'both',
        colors_palette = 'Spectral',
        img_format = "png"
      ),

     # Fatty acid analysis parameters self$params$fa_analysis_plot$
     fa_analysis_plot = list(
       data_table = "Raw data table",
       feature_meta = NULL,
       sample_meta = "Raw meta table",
       group_col = NULL,
       pathway = NULL,
       selected_lipidclass = "All",
       color_palette = 'Spectral',
       img_format = "png"
     )
    ),
    #----------------------------------------------------Hard coded settings----
    hardcoded_settings = list(
      # general
      meta_column = c(
        "Culture conditions" = "cultureConditions",
        "Geno type" = "genoType",
        "Treatment" = "treatment",
        "Sex" = "sex"
      ),
      color_palette = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                        "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
                        "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "BrBG", "PiYG", "PRGn",
                        "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent",
                        "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
      image_format = c("png", "svg", "jpeg", "webp"),

      # plot specific
      class_distribution = list(
        datasets = list(
          "Lipid classes (absolute conc.)" = "Class table",
          "Lipid classes (normalized, % of total lipid classes)" = "Class table total normalized"
        )
      ),

      class_comparison = list(
        datasets = list(
          "Lipid classes (absolute conc.)" = "Class table",
          "Lipid classes (normalized, % of total lipid classes)" = "Class table total normalized"
        )
      ),

      volcano_plot = list(
        datasets = list(
          "Lipid species (normalized, % of total lipids within class)" = "Class normalized table",
          "Lipid species (normalized, % of total lipids)" = "Total normalized table"
        ),
        calc_func = list(
          "median",
          "mean"
        ),
        test_func = list(
          "t-Test",
          "Wilcoxon"
        ),
        adjustment_func = list(
          "None" = "None",
          "Benjamini & Hochberg" = "BH"
        ),
        display_plot = list(
          "main",
          "all",
          "left",
          "right",
          "top"
        ),
        feature_metadata = list(
          "None" = "None",
          "Lipid classes" = "lipid_class",
          "SN1 number of carbons" = "carbons_1",
          "SN2 number of carbons" = "carbons_2",
          "Total number of carbons" = "carbons_sum",
          "Total number of double bonds" = "unsat_sum"
        )
      ),

      heatmap = list(
        datasets = list(
          "Lipid species" = "Z-scored table",
          "Lipid species (normalized, % of total lipids)" = "Z-scored total normalized table",
          "Lipid classes" = "Class table z-scored",
          "Lipid classes (normalized, % of total lipid classes)" = "Class table z-scored total normalized"
        ),
        map_cols = list(
          "Lipid classes" = "lipid_class",
          "SN1 number of carbons" = "carbons_1",
          "SN2 number of carbons" = "carbons_2",
          "Total number of carbons" = "carbons_sum",
          "Total number of double bonds" = "unsat_sum"
        )
      ),
      # samples_correlation = list(
      #   datasets = list(
      #     "Raw data table",
      #     "Total normalized table",
      #     'Z-scored table',
      #     'Z-scored total normalized table'
      #   )
      # ),
      # feature_correlation = list(
      #   datasets = list(
      #     "Raw data table",
      #     "Total normalized table",
      #     'Z-scored table',
      #     'Z-scored total normalized table'
      #   )
      # ),
      pca = list(
        datasets = list(
          # "Lipid species (z-scores)" = "Z-scored table",
          "Lipid species (z-scores), (normalized, % of total lipids)" = "Z-scored total normalized table"
        ),
        method = list(
          "svd",
          "nipals",
          "rnipals",
          "bpca",
          "ppca",
          "svdImpute",
          "llsImputeAll"
        ),
        display_plot = list(
          "both",
          "scores",
          "loadings",
          "variance"
        )
      ),
      fa_analysis = list(
        pathway = list(
          "SFA",
          "MUFA",
          "PUFA(n-6)",
          "PUFA(n-3)"
        )
      )
    ),

    #--------------------------------------------------------------- Indices ----

    indices = list(
      id_col_meta = NA,
      id_col_data = NA,
      type_col = NA,
      group_col = NA,
      batch_col = NA,

      idx_blanks = NULL,
      idx_qcs = NULL,
      idx_pools = NULL,
      idx_samples = NULL,

      rownames_blanks = NULL,
      rownames_qcs = NULL,
      rownames_pools = NULL,
      rownames_samples = NULL,

      excluded_cols = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(

      imp_meta = NULL,
      raw_meta = NULL,

      imp_data = NULL,
      raw_data = NULL,

      blank_table = NULL,

      qc_cell_table = NULL,
      qc_plasma_table = NULL,

      feature_table = NULL,

      # Group summaries
      summary_species_table = NULL,
      summary_class_table = NULL,

      # Normalised
      class_norm_data = NULL,
      total_norm_data = NULL,

      # Z-scored
      z_scored_data = NULL,
      z_scored_class_norm_data = NULL,
      z_scored_total_norm_data = NULL,

      # class tables
      class_table= NULL,
      class_table_z_scored = NULL,
      class_table_total_norm = NULL,
      class_table_z_scored_total_norm = NULL,

      # Plot tables
      class_distribution_table = NULL,
      volcano_table = NULL,
      heatmap_table = NULL,
      pca_scores_table = NULL,
      pca_loadings_table = NULL,
      dbplot_table = NULL,
      satindex_table = NULL,
      fa_analysis_table = NULL


    ),

    #-------------------------------------------------------------- Local table

    table_switch_local = function(table_name) {
      switch(EXPR = table_name,
             'Imported metadata table' = self$tables$imp_meta,
             'Raw metadata table' = self$tables$raw_meta,
             'Imported data table' = self$tables$imp_data,
             'Raw data table' = self$tables$raw_data,
             'Feature table' = self$tables$feature_table,
             'Blank table' = self$tables$blank_table,
             'Class normalized table' = self$tables$class_norm_data,
             'Total normalized table' = self$tables$total_norm_data,
             'Z-scored table' = self$tables$z_scored_data,
             'Z-scored class normalized table' = self$tables$z_scored_class_norm_data,
             'Z-scored total normalized table' = self$tables$z_scored_total_norm_data,
             'Class table' = self$tables$class_table,
             'Class table z-scored' = self$tables$class_table_z_scored,
             'Class table total normalized' = self$tables$class_table_total_norm,
             'Class table z-scored total normalized' = self$tables$class_table_z_scored_total_norm,
             'Species summary table' = self$tables$summary_species_table,
             'Class summary table' = self$tables$summary_class_table,
             'GSEA prot list' = self$tables$gsea_prot_list,
             'ORA prot list' = self$tables$ora_prot_list
      )
    },

    table_check_convert = function(table) {
      if (length(table) == 1) {
        if (is.character(table)){
          table = self$table_switch_local(table)
        }
      }
      return(table)
    },

    #---------------------------------------------------------------- Plots ----
    plots = list(
      class_distribution = NULL,
      class_comparison = NULL,
      volcano_plot = NULL,
      heatmap = NULL,
      pca_plot = NULL,
      double_bond_plot = NULL,
      satindex_plot = NULL,
      fa_analysis_plot = NULL
    ),

    #---------------------------------------------------- Parameter methods ----

    param_class_distribution = function(dataset, group_col, color_palette, img_format) {
      self$params$class_distribution$dataset = dataset
      self$params$class_distribution$group_col = group_col
      self$params$class_distribution$color_palette = color_palette
      self$params$class_distribution$img_format = img_format
    },

    param_class_comparison = function(dataset, group_col, color_palette, img_format) {
      self$params$class_comparison$dataset = dataset
      self$params$class_comparison$group_col = group_col
      self$params$class_comparison$color_palette = color_palette
      self$params$class_comparison$img_format = img_format
    },

    param_volcano_plot = function(auto_refresh, data_table, adjustment, group_col, group_1, group_2, feature_metadata, displayed_plot,
                                  p_val_threshold, fc_threshold, marker_size, opacity, color_palette, selected_function, selected_test, img_format) {

      self$params$volcano_plot$auto_refresh = auto_refresh
      self$params$volcano_plot$data_table = data_table
      self$params$volcano_plot$adjustment = adjustment
      self$params$volcano_plot$group_col = group_col
      self$params$volcano_plot$group_1 = group_1
      self$params$volcano_plot$group_2 = group_2
      self$params$volcano_plot$feature_metadata = feature_metadata
      self$params$volcano_plot$displayed_plot = displayed_plot
      self$params$volcano_plot$p_val_threshold = p_val_threshold
      self$params$volcano_plot$fc_threshold = fc_threshold
      self$params$volcano_plot$marker_size = marker_size
      self$params$volcano_plot$opacity = opacity
      self$params$volcano_plot$color_palette = color_palette
      self$params$volcano_plot$selected_function = selected_function
      self$params$volcano_plot$selected_test = selected_test
      self$params$volcano_plot$img_format = img_format

    },

    param_heatmap = function(dataset, impute, cluster_samples, cluster_features, map_sample_data, map_feature_data, group_column_da, apply_da, alpha_da, color_palette, reverse_palette, factor_height, img_format) {
      self$params$heatmap$dataset = dataset
      self$params$heatmap$impute = impute
      self$params$heatmap$cluster_samples = cluster_samples
      self$params$heatmap$cluster_features = cluster_features
      self$params$heatmap$map_sample_data = map_sample_data
      self$params$heatmap$map_feature_data = map_feature_data
      self$params$heatmap$group_column_da = group_column_da
      self$params$heatmap$apply_da = apply_da
      self$params$heatmap$alpha_da = alpha_da
      self$params$heatmap$color_palette = color_palette
      self$params$heatmap$reverse_palette = reverse_palette
      self$params$heatmap$factor_height = factor_height
      self$params$heatmap$img_format = img_format
    },

    param_pca = function(auto_refresh, data_table, sample_groups_col, sample_groups_col_shape, feature_groups_col, apply_da, alpha_da, pca_method, nPcs, displayed_pc_1, displayed_pc_2, completeObs, displayed_plots, colors_palette, img_format) {
      self$params$pca$auto_refresh = auto_refresh
      self$params$pca$data_table = data_table
      self$params$pca$sample_groups_col = sample_groups_col
      self$params$pca$sample_groups_col_shape = sample_groups_col_shape
      self$params$pca$feature_groups_col = feature_groups_col
      self$params$pca$apply_da = apply_da
      self$params$pca$alpha_da = alpha_da
      self$params$pca$pca_method = pca_method
      self$params$pca$nPcs = nPcs
      self$params$pca$displayed_pc_1 = displayed_pc_1
      self$params$pca$displayed_pc_2 = displayed_pc_2
      self$params$pca$completeObs = completeObs
      self$params$pca$displayed_plots = displayed_plots
      self$params$pca$colors_palette = colors_palette
      self$params$pca$img_format = img_format

    },

    param_fa_analysis_plot = function(data_table, feature_meta, sample_meta, group_col, pathway, selected_lipidclass, color_palette, img_format) {
      self$params$fa_analysis_plot$data_table = data_table
      self$params$fa_analysis_plot$feature_meta = feature_meta
      self$params$fa_analysis_plot$sample_meta = sample_meta
      self$params$fa_analysis_plot$group_col = group_col
      self$params$fa_analysis_plot$pathway = pathway
      self$params$fa_analysis_plot$selected_lipidclass = selected_lipidclass
      self$params$fa_analysis_plot$color_palette = color_palette
      self$params$fa_analysis_plot$img_format = img_format
    },

    #-------------------------------------------------------- Table methods ----

    set_raw_meta = function(){
      if (!is.na(self$indices$id_col_meta) & !is.null(self$tables$imp_meta)){
        data_table = self$tables$imp_meta
        rownames(data_table) = paste(data_table[, "batchNumber"], data_table[, self$indices$id_col_meta], sep = "_")
        data_table[, self$indices$id_col_meta] = NULL
        self$tables$raw_meta = data_table
      }
    },

    set_raw_data = function(apply_imputation = T,
                            impute_before = T,
                            apply_filtering = T,
                            imputation_function = 'median',
                            val_threshold = 0.6,
                            blank_multiplier = 0.8,
                            sample_threshold = 0.8,
                            group_threshold = 0.6,
                            norm_col = "") {
      if (!is.na(self$indices$id_col_data) & !is.null(self$tables$imp_data) & !is.null(self$tables$raw_meta)){
        # Copy imported table
        data_table = self$tables$imp_data

        # Set ID column
        # rownames(data_table) = data_table[, self$indices$id_col_data]
        data_table[,self$indices$id_col_data] = NULL
        data_table = as.matrix(data_table)

        # Keep only rows from raw_meta
        data_table = data_table[rownames(self$tables$raw_meta),]

        # Remove columns from exclusion list
        if (!is.null(self$indices$excluded_cols)) {
          data_table = drop_cols(data_table, self$indices$excluded_cols)
        }

        # Remove empty columns
        data_table = remove_empty_cols(data_table)
        # Imputation and filtering
        if (apply_imputation & impute_before & apply_filtering) {
          # impute and filter
          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)

          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)
        } else if (apply_imputation & !impute_before & apply_filtering) {
          # impute (not before) and filter
          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)

          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)
        } else if (apply_imputation & !apply_filtering) {
          # impute alone
          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)
        } else if (!apply_imputation & apply_filtering) {
          # Filtering alone
          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)
        }

        if (norm_col != "") {
          if (is_num_coercible(self$tables$raw_meta[,norm_col]) & !base::any(is.na(self$tables$raw_meta[,norm_col]))) {
            print(paste0('Normalizing data by ', norm_col))
            data_table = data_table/as.numeric(self$tables$raw_meta[,norm_col])
          } else {
            print('Warning: Normalization skipped, selected column contains either non numeric or missing data.')
          }
        }
        self$tables$raw_data = data_table
      }
    },

    get_feature_table = function() {
      self$tables$feature_table = get_feature_metadata(data_table = self$tables$raw_data)
    },

    get_blank_table = function() {
      blank_table = self$tables$imp_data[rownames(self$tables$imp_data) %in% self$indices$rownames_blanks, ]
      blank_table[, self$indices$id_col_data] = NULL
      self$tables$blank_table = as.matrix(blank_table)
    },

    get_qc_table = function() {
      id_cells <- self$tables$imp_meta[tolower(self$tables$imp_meta$cellType) == "quality control cells", "analystId"]
      id_plasma <- self$tables$imp_meta[tolower(self$tables$imp_meta$cellType) == "quality control plasma", "analystId"]

      qc_cells <- self$tables$imp_data[self$tables$imp_data$ID %in% id_cells, ]
      qc_plasma <- self$tables$imp_data[self$tables$imp_data$ID %in% id_plasma, ]

      # qc_table[, self$indices$id_col_data] <- NULL
      self$tables$qc_cells_table <- qc_cells
      self$tables$qc_plasma_table <- qc_plasma
    },

    # Class normalisation
    normalise_class = function(){
      self$tables$class_norm_data = normalise_lipid_class(self$tables$raw_data) * 100
    },

    # Total or Row normalisation
    normalise_total = function(){
      self$tables$total_norm_data = self$tables$raw_data/rowSums(self$tables$raw_data, na.rm = T) * 100
    },

    # Z-score normalisation
    normalise_z_score = function() {
      self$tables$z_scored_data = z_score_normalisation(data_table = self$tables$raw_data)
    },

    # Class and z-score normalisation
    normalise_class_z_score = function() {
      self$tables$z_scored_class_norm_data = z_score_normalisation(data_table = self$tables$class_norm_data)
    },

    # Total and z-score normalisation
    normalise_total_z_score = function() {
      self$tables$z_scored_total_norm_data = z_score_normalisation(data_table = self$tables$total_norm_data)
    },

    # Class table
    get_class_table = function(){
      self$tables$class_table = get_lipid_class_table(self$tables$raw_data)
    },

    # Class table z-scored
    get_class_table_z_scored = function(){
      self$tables$class_table_z_scored = z_score_normalisation(data_table = self$tables$class_table)
    },

    # Class table total norm
    class_grouping_total_norm = function(){
      self$tables$class_table_total_norm = get_lipid_class_table(self$tables$total_norm_data)
    },

    # Z-score the class table (generated by the class_grouping method)
    normalise_class_table_z_score = function() {
      self$tables$class_table_z_scored_total_norm = z_score_normalisation(data_table = self$tables$class_table_total_norm)
    },

    get_group_summary_species = function() {
      self$tables$summary_species_table = get_group_median_table(data_table = self$tables$raw_data,
                                                                 meta_table = self$tables$raw_meta,
                                                                 group_col = self$indices$group_col)
    },

    get_group_summary_classes = function() {
      self$tables$summary_class_table = get_group_median_table(data_table = self$tables$class_table,
                                                               meta_table = self$tables$raw_meta,
                                                               group_col = self$indices$group_col)
    },

    derive_data_tables = function() {
      # Derive tables

      self$get_feature_table()
      self$normalise_class()
      self$normalise_total()
      self$normalise_z_score()
      self$normalise_class_z_score()
      self$normalise_total_z_score()
      self$get_class_table()
      self$get_class_table_z_scored()
      self$class_grouping_total_norm()
      self$normalise_class_table_z_score()
      self$get_group_summary_species()
      self$get_group_summary_classes()

      # Set plotting parameters
      self$param_class_distribution(dataset = 'Class table total normalized',
                                    group_col = self$indices$group_col,
                                    color_palette = 'Spectral',
                                    img_format = "png")

      self$param_class_comparison(dataset = 'Class table total normalized',
                                  group_col = self$indices$group_col,
                                  color_palette = 'Spectral',
                                  img_format = "png")

      self$param_volcano_plot(auto_refresh = TRUE,
                              data_table = 'Total normalized table',
                              adjustment = "BH",
                              group_col = self$indices$group_col,
                              group_1 = unique(self$tables$raw_meta[,self$indices$group_col])[1],
                              group_2 = unique(self$tables$raw_meta[,self$indices$group_col])[2],
                              feature_metadata = "lipid_class",
                              displayed_plot = "all",
                              p_val_threshold = 0.05,
                              fc_threshold = 2,
                              marker_size = 8,
                              opacity = 1,
                              color_palette = 'Spectral',
                              selected_function = "mean",
                              selected_test = "t-Test",
                              img_format = "png")

      self$param_heatmap(dataset = 'Z-scored total normalized table',
                         impute = TRUE,
                         cluster_samples = TRUE,
                         cluster_features = TRUE,
                         map_sample_data = self$indices$group_col,
                         map_feature_data = "lipid_class",
                         group_column_da = self$indices$group_col,
                         apply_da = FALSE,
                         alpha_da = 0.8,
                         color_palette = 'RdYlBu',
                         reverse_palette = FALSE,
                         factor_height = 2,
                         img_format = "png")

      self$param_pca(auto_refresh = TRUE,
                     data_table = 'z_scored_total_norm_data',
                     sample_groups_col = self$indices$group_col,
                     sample_groups_col_shape = "",
                     feature_groups_col = NULL,
                     apply_da = self$params$pca$apply_da,
                     alpha_da = 0.8,
                     pca_method = 'svd',
                     nPcs = 10,
                     displayed_pc_1 = 1,
                     displayed_pc_2 = 2,
                     completeObs = F,
                     displayed_plots = 'both',
                     colors_palette = 'Spectral',
                     img_format = "png")

      self$param_fa_analysis_plot(data_table = self$tables$raw_data,
                                  feature_meta = self$tables$feature_table,
                                  sample_meta = self$tables$raw_meta,
                                  group_col = self$indices$group_col,
                                  selected_lipidclass = self$indices$selected_lipidclass,
                                  pathway = NULL,
                                  color_palette = 'Spectral',
                                  img_format = "png")

    },

    #--------------------------------------------------- Plot table methods ----

    # Volcano table
    get_volcano_table = function(data_table = self$tables$raw_data,
                                 volcano_table = self$tables$feature_table,
                                 group_col = self$params$volcano_plot$group_col,
                                 used_function = self$params$volcano_plot$selected_function,
                                 test = self$params$volcano_plot$selected_test,
                                 group_1 = self$params$volcano_plot$group_1,
                                 group_2 = self$params$volcano_plot$group_2) {
      rownames_group_1 = rownames(self$tables$raw_meta)[self$tables$raw_meta[, group_col] == group_1]
      rownames_group_2 = rownames(self$tables$raw_meta)[self$tables$raw_meta[, group_col] == group_2]
      all_rownames = sort(unique(c(rownames_group_1, rownames_group_2)))

      # Filter data to keep only the two groups
      data_table = data_table[all_rownames,]

      # Get the indices for each group
      idx_group_1 = which(rownames(data_table) %in% rownames_group_1)
      idx_group_2 = which(rownames(data_table) %in% rownames_group_2)


      # Remove empty columns
      dead_features = colnames(data_table)
      data_table = remove_empty_cols(table = data_table)
      dead_features = setdiff(dead_features, colnames(data_table))

      if (length(dead_features) > 0) {
        dead_features = which(rownames(volcano_table) %in% dead_features)
        volcano_table = volcano_table[-dead_features,]
      }


      # Collect fold change and p-values
      volcano_table$fold_change = get_fold_changes(data_table = data_table,
                                                   idx_group_1 = idx_group_1,
                                                   idx_group_2 = idx_group_2,
                                                   used_function = used_function,
                                                   impute_inf = F)


      volcano_table$p_val = get_p_val(data_table = data_table,
                                      idx_group_1 = idx_group_1,
                                      idx_group_2 = idx_group_2,
                                      used_function = test,
                                      impute_na = F)
      volcano_table$q_val_bh = stats::p.adjust(volcano_table$p_val, method = "BH")

      volcano_table$minus_log10_p_value = -log10(volcano_table$p_val)
      volcano_table$log2_fold_change = log2(volcano_table$fold_change)
      volcano_table$minus_log10_p_value_bh_adj = -log10(volcano_table$q_val_bh)

      self$tables$volcano_table = volcano_table
    },


    #----------------------------------------------------- Plotting methods ----
    # Class distribution
    plot_class_distribution = function(table = self$params$class_distribution$dataset,
                                       meta_table = self$tables$raw_meta,
                                       group_col = self$params$class_distribution$group_col,
                                       color_palette = self$params$class_distribution$color_palette,
                                       width = NULL,
                                       height = NULL){
      table = self$table_check_convert(table)

      # Produce the class x group table
      samp_list = rownames(table)
      class_list = colnames(table)
      group_list = sort(unique(meta_table[, group_col]))

      plot_table = data.frame(matrix(data = 0.0,
                                     nrow = length(class_list),
                                     ncol = length(group_list)))
      rownames(plot_table) = class_list
      colnames(plot_table) = group_list

      for (c in class_list) {
        for (g in group_list){
          s = rownames(meta_table)[meta_table[,group_col] == g]
          m = mean(as.matrix(table[s, c]))
          plot_table[c,g] = m
        }
      }

      # Store the plot_table
      self$tables$class_distribution_table = plot_table

      colors = brewer.pal(as.numeric(colors_switch(color_palette)), color_palette)
      colors = colorRampPalette(colors)(length(group_list))
      colors = setNames(colors, group_list)

      # Produce the plot
      i = 1
      fig = plotly::plot_ly(colors = unname(colors), width = width, height = height)
      for (col in colnames(plot_table)) {
        fig = fig %>% add_trace(x = rownames(plot_table), y = plot_table[,col],
                                name = col, color = colors[col], type  = "bar")
        fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                             yaxis = list(title = "%", tickformat = "digits"))
        i = i + 1
      }

      self$plots$class_distribution = fig
    },

    # Class comparison
    plot_class_comparison = function(data_table = self$params$class_comparison$dataset,
                                     meta_table = self$tables$raw_meta,
                                     group_col = self$params$class_comparison$group_col,
                                     color_palette = self$params$class_comparison$color_palette,
                                     width = NULL,
                                     height = NULL){
      data_table = self$table_check_convert(data_table)

      # Get sample groups and the list of classes
      groups = sort(unique(meta_table[,group_col]))
      class_list = colnames(data_table)

      x_dim = ceiling(sqrt(length(class_list)))
      y_dim = floor(sqrt(length(class_list)))


      x_step = 1/x_dim
      y_step = 1/y_dim

      x = x_step / 2
      y = 0.97 - y_step
      i = 1

      annotations = c()
      for (c in class_list) {
        tmp_ann = list(
          x = x,
          y = y,
          text = c,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE)
        annotations[[i]] = tmp_ann
        i = i + 1
        x = x + x_step
        if (x >= 1) {
          x = x_step/2
          y = y - y_step}
      }
      annotations[[i]] = list(x = -0.045, y = 0.5, text = "%",
                              font = list(size = 12),
                              textangle = 270, showarrow = FALSE, xref='paper',
                              yref='paper')

      colors = brewer.pal(as.numeric(colors_switch(color_palette)), color_palette)
      colors = colorRampPalette(colors)(length(groups))
      colors = setNames(colors, groups)

      # Plot list will be the list of subplots
      plot_list = c()

      # Cleared groups is created for the legends
      cleared_groups = c()
      j = 1
      for (c in class_list) {
        i = 1
        subplot = plot_ly(colors = unname(colors), width = width, height = height)
        for (g in groups){
          if (g %in% cleared_groups) {
            first_bool = FALSE
          }else{
            first_bool = TRUE
            cleared_groups = c(cleared_groups, g)
          }

          # For each class, each group
          s = rownames(meta_table)[meta_table[, group_col] == g] # Get the samples for the current group
          d = data_table[s, c] # Get the concentrations for all s samples in the current class c
          m = mean(d) # Get the mean concentration for samples s for class c

          # Subplot for the bar chart displaying the mean concentration
          subplot = subplot %>% add_trace(x = g, y = m, type  = "bar", name = g,
                                          color = colors[g], alpha = 1,
                                          legendgroup=i, showlegend = first_bool)

          # Subplot for boxplots displaying the median and all datapoints
          subplot = subplot %>% add_trace(x = g, y = d, type  = "box", boxpoints = "all",
                                          pointpos = 0, name = g, color = colors[g],
                                          line = list(color = 'rgb(100,100,100)'),
                                          marker = list(color = 'rgb(100,100,100)'), alpha = 1,
                                          legendgroup=i, showlegend = FALSE,
                                          text = s,
                                          hoverinfo = "text")
          subplot = subplot %>% layout(xaxis = list(showticklabels = FALSE),
                                       yaxis = list(tickfont = list(size = 8),
                                                    tickformat = "digits"))
          i = i + 1
        }
        plot_list[[j]] = plotly_build(subplot)
        j = j + 1
      }

      fig = subplot(plot_list, nrows = y_dim, margin = 0.035, titleX = TRUE)
      fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                           annotations = annotations)

      self$plots$class_comparison = fig
    },

    ## Volcano plot
    plot_volcano = function(data_table = self$tables$volcano_table,
                            adjustment = self$params$volcano_plot$adjustment,
                            group_1 = self$params$volcano_plot$group_1,
                            group_2 = self$params$volcano_plot$group_2,
                            feature_metadata = self$params$volcano_plot$feature_metadata,
                            displayed_plot = self$params$volcano_plot$displayed_plot,
                            p_val_threshold = self$params$volcano_plot$p_val_threshold,
                            fc_threshold = self$params$volcano_plot$fc_threshold,
                            marker_size = self$params$volcano_plot$marker_size,
                            opacity = self$params$volcano_plot$opacity,
                            color_palette = self$params$volcano_plot$color_palette,
                            width = NULL,
                            height = NULL){
      data_table = self$table_check_convert(data_table)

      p_val_threshold = as.numeric(p_val_threshold)
      fc_threshold = as.numeric(fc_threshold)
      marker_size = as.numeric(marker_size)
      opacity = as.numeric(opacity)

      if (adjustment == 'BH') {
        p_vals = data_table$q_val_bh
        y_label = '-Log10(BH(p-value))'
      } else {
        p_vals = data_table$p_val
        y_label = '-Log10(p-value)'
      }

      if (!is.null(feature_metadata)) {
        if (feature_metadata %in% colnames(data_table)) {
          feature_metadata = data_table[,feature_metadata]
        } else {
          feature_metadata = NULL
        }
      }

      displayed_text = paste0(paste0(rownames(data_table), '\n'),
                              paste0('p-value: ', round(p_vals, 3), '\n'),
                              paste0('FC: ', round(data_table$fold_change, 2)))

      fig = volcano_main(fc_vals = data_table$fold_change,
                         p_vals = p_vals,
                         names = displayed_text,
                         y_label = y_label,
                         left_label = group_1,
                         right_label = group_2,
                         groups = feature_metadata,
                         displayed_plot = displayed_plot,
                         color_palette = color_palette,
                         p_val_threshold = p_val_threshold,
                         fc_threshold = fc_threshold,
                         marker_size = marker_size,
                         opacity = opacity)


      self$plots$volcano_plot = fig
    },

    ## Heatmap plot
    plot_heatmap = function(data_table = self$params$heatmap$dataset,
                            impute = self$params$heatmap$impute,
                            meta_table = self$tables$raw_meta,
                            meta_table_features = self$tables$feature_table,
                            cluster_rows = self$params$heatmap$cluster_samples,
                            cluster_cols = self$params$heatmap$cluster_features,
                            row_annotations = self$params$heatmap$map_sample_data,
                            col_annotations = self$params$heatmap$map_feature_data,
                            apply_da = self$params$heatmap$apply_da,
                            group_column_da = self$params$heatmap$group_column_da,
                            alpha_da = self$params$heatmap$alpha_da,
                            color_palette = self$params$heatmap$color_palette,
                            reverse_palette = self$params$heatmap$reverse_palette,
                            factor_height = self$params$heatmap$factor_height,
                            width = NULL,
                            height = NULL) {
      data_table = self$table_check_convert(data_table)

      if (apply_da) {
        data_table = apply_discriminant_analysis(data_table = data_table,
                                                 group_list = meta_table[,group_column_da],
                                                 nlambda = 100,
                                                 alpha = alpha_da)

        meta_table_features = meta_table_features[colnames(data_table),]
      }


      # Save table as heatmap table
      self$tables$heatmap_table = data_table

      # Set the clustering
      if (cluster_rows & cluster_cols) {
        dendrogram_list = "both"
      } else if (cluster_rows) {
        # only samples
        dendrogram_list = "column" # Because of the transpose, rows => cols
      } else if (cluster_cols) {
        # only features
        dendrogram_list = "row" # Because of the transpose, cols => rows
      } else {
        dendrogram_list = "none"
      }

      # get the relative sizes for the subplots
      subplot_sizes <- calc_subplot_size(dendrogram = dendrogram_list,
                                         cluster_rows = row_annotations,
                                         cluster_columns = col_annotations,
                                         factor_height = factor_height)

      # make sure the color scale is symmetrical
      zmax <- max(c(min(data_table, na.rm = TRUE),
                    max(data_table, na.rm = TRUE)))
      zmin <- -zmax


      # Annotations
      if (!is.null(row_annotations)) {
        if (length(row_annotations) > 1) {
          row_annotations = meta_table[, row_annotations]
          colnames(row_annotations) = stringr::str_replace_all(colnames(row_annotations), "_", " ")
        } else {
          row_names = row_annotations
          row_annotations = as.data.frame(meta_table[, row_annotations],
                                          row.names = rownames(meta_table))
          colnames(row_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      # Reorder the feature metadata according to the data_table order
      meta_table_features = meta_table_features[c(colnames(data_table)),]

      if (!is.null(col_annotations)) {
        clean_names = col_annotations
        col_annotations = as.data.frame(meta_table_features[, col_annotations],
                                        row.names = rownames(meta_table_features))
        colnames(col_annotations) = clean_names
      }

      if (impute) {
        print('Imputing NAs')
        data_table[is.na(data_table)] <- min(data_table, na.rm = TRUE)
      }

      # Get the color palette
      color_count = colors_switch(color_palette)
      color_palette = RColorBrewer::brewer.pal(color_count, color_palette)
      if (reverse_palette) {
        color_palette = base::rev(color_palette)
      }

      # customise the x-axis labels
      # use group name and the last 3 number of the sample name
      print("Rico: customise x-axis labels")
      print(head(meta_table))
      print(meta_table[, c(self$indices$group_col)])
      group_names <- meta_table[, c(self$indices$group_col)]
      names(group_names) <- rownames(meta_table)
      xlabels <- paste0(group_names,
                        "_",
                        gsub(x = names(group_names),
                             pattern = ".*([0-9]{3})$",
                             replacement = "\\1"))


      # Plot the data
      self$plots$heatmap = heatmaply::heatmaply(x = t(data_table),
                                                colors = base::rev(color_palette),
                                                fontsize_row = 7,
                                                plot_method = "plotly",
                                                colorbar_len = 0.3 / factor_height,
                                                colorbar_yanchor = "top",
                                                colorbar_xpos = 1.15,
                                                colorbar_ypos = 1,
                                                width = width,
                                                height = factor_height * height,
                                                limits = c(zmin, zmax),
                                                subplot_widths = subplot_sizes$width,
                                                subplot_heights = subplot_sizes$height,
                                                col_side_colors = row_annotations,
                                                row_side_colors = col_annotations,
                                                dendrogram = dendrogram_list,
                                                labCol = xlabels)
    },

    ## PCA scores and loading plots
    plot_pca = function(data_table = self$params$pca$data_table,
                        meta_table = self$tables$raw_meta,
                        feature_table = self$tables$feature_table,
                        sample_groups_col = self$params$pca$sample_groups_col,
                        sample_groups_col_shape = self$params$pca$sample_groups_col_shape,
                        feature_groups_col = self$params$pca$feature_groups_col,
                        apply_da = self$params$pca$apply_da,
                        alpha_da = self$params$pca$alpha_da,
                        pca_method = self$params$pca$pca_method,
                        nPcs = self$params$pca$nPcs,
                        displayed_pc_1 = self$params$pca$displayed_pc_1,
                        displayed_pc_2 = self$params$pca$displayed_pc_2,
                        completeObs = self$params$pca$completeObs,
                        displayed_plots = self$params$pca$displayed_plots,
                        colors_palette = self$params$pca$colors_palette,
                        return_data = TRUE,
                        width = NULL,
                        height = NULL) {
      data_table = self$table_check_convert(data_table)

      alpha_da = as.numeric(alpha_da)
      nPcs = as.numeric(nPcs)
      displayed_pc_1 = as.numeric(displayed_pc_1)
      displayed_pc_2 = as.numeric(displayed_pc_2)

      if (is.character(data_table)) {
        data_table = self$tables[[data_table]]
      }

      sample_groups = meta_table[rownames(data_table), sample_groups_col]
      sample_groups_shape <- meta_table[rownames(data_table), sample_groups_col_shape]

      if (apply_da) {
        data_table = apply_discriminant_analysis(data_table = data_table,
                                                 group_list = sample_groups,
                                                 nlambda = 100,
                                                 alpha = alpha_da)}

      ncol_1 = ncol(data_table)
      data_table = data_table[,!is.na(colSums(data_table, na.rm = T))]
      ncol_2 = ncol(data_table)
      if(ncol_2 != ncol_1) {
        print_time(paste0("PCA : dropped ", ncol_1 - ncol_2, " features with no signal variation."))
      }

      if (!is.null(feature_groups_col) & !is.null(feature_table)) {
        feature_groups = feature_table[colnames(data_table),feature_groups_col]
        if (length(which(is.na(feature_groups))) < 30) {
          feature_groups[which(is.na(feature_groups))] = colnames(data_table)[which(is.na(feature_groups))]
        } else {
          feature_groups[which(is.na(feature_groups))] = "UNK"
        }

      } else {
        feature_groups = NULL
      }

      pca_out = pca_main(data_table = data_table,
                         sample_groups = sample_groups,
                         sample_groups_shape = sample_groups_shape,
                         feature_groups = feature_groups,
                         nPcs = nPcs,
                         displayed_pc_1 = displayed_pc_1,
                         displayed_pc_2 = displayed_pc_2,
                         pca_method = pca_method,
                         completeObs = completeObs,
                         displayed_plots = displayed_plots,
                         colors_palette = colors_palette,
                         return_data = return_data)

      self$tables$pca_scores_table = pca_out$pca_data@scores
      self$tables$pca_loadings_table = pca_out$pca_data@loadings
      self$plots$pca_plot = pca_out$fig
    },

    ## FA analysis
    plot_fa_analysis = function(data_table = self$tables$raw_data,
                                feature_table = self$tables$feature_table,
                                sample_meta = self$tables$raw_meta,
                                group_col = self$params$fa_analysis_plot$group_col,
                                pathway = self$params$fa_analysis_plot$pathway,
                                selected_lipidclass = self$params$fa_analysis_plot$selected_lipidclass,
                                color_palette = self$params$fa_analysis_plot$color_palette,
                                width = NULL,
                                height = NULL) {

      ## At the moment this function is using the raw data table
      # do the calculations
      res <- fa_analysis_calc(data_table = data_table,
                              feature_table = feature_table,
                              sample_meta = sample_meta,
                              selected_lipidclass = selected_lipidclass)


      # Produce the class x group table
      # add ID's, group's and make long
      res$ID <- rownames(res)
      res$group <- sample_meta[res$ID, group_col]
      res_long <- res |>
        tidyr::pivot_longer(cols = -c(ID, group),
                            names_to = "fa_chain",
                            values_to = "value")

      # calculate mean and stdev per group
      plot_table <- tapply(as.data.frame(res_long), list(res_long$group, res_long$fa_chain), function(x) {
        avg <- mean(x[, "value"], na.rm = TRUE)
        stdev <- sd(x[, "value"], na.rm = TRUE)

        return(list(avg = avg,
                    stdev = stdev,
                    fa_chain = x[1, "fa_chain"],
                    group = x[1, "group"]))
        # print(x)
      })

      plot_table <- do.call(rbind.data.frame, plot_table)

      # filter plot_table based on pathway selection
      pathway_fa <- c(
        paste(seq(16, 26, 2), 0, sep = ":"),
        paste(seq(16, 24, 2), 1, sep = ":"),
        c("18:2", "18:3", "20:2", "20:3", "20:4",
          "22:4", "22:5","24:4", "24:5"),
        c("18:3", "18:4", "20:3", "20:4", "20:5",
           "22:5", "22:6", "24:5", "24:6")
      )
      names(pathway_fa) <- c(rep("SFA", 6),
                             rep("MUFA", 5),
                             rep("PUFA(n-6)", 9),
                             rep("PUFA(n-3)", 9))

      if(!is.null(pathway)) {
        selected_pathway_fa <- unique(pathway_fa[names(pathway_fa) %in% pathway])
        plot_table <- plot_table[plot_table$fa_chain %in% selected_pathway_fa, ]
      }

      # Store the plot_table
      self$tables$fa_analysis_table <- plot_table

      group_list = sort(unique(plot_table$group))
      colors = brewer.pal(as.numeric(colors_switch(color_palette)), color_palette)
      colors = colorRampPalette(colors)(length(group_list))
      colors = setNames(colors, group_list)

      # plotting
      i <- 1
      fig <- plotly::plot_ly(colors = unname(colors), width = width, height = height)
      for (grp in unique(plot_table$group)) {
        fig <- fig |>
          plotly::add_trace(data = plot_table[plot_table$group == grp, ],
                            x = ~fa_chain,
                            y = ~avg,
                            color = colors[i],
                            type = "bar",
                            name = grp,
                            error_y = ~ list(array = stdev,
                                             color = "#000000"))
        fig <- fig |>
          plotly::layout(legend = list(orientation = 'h',
                                       xanchor = "center",
                                       x = 0.5),
                         xaxis = list(title = "Fatty acid chain"),
                         yaxis = list(title = "Concentration"))
        i <- i + 1
      }
      fig <- fig |>
        plotly::layout(annotations =
                         list(x = 1,
                              y = -0.175,
                              text = "NOTE: error bars are standard deviation",
                              showarrow = FALSE,
                              xref = "paper",
                              yref = "paper",
                              xanchor = "right",
                              yanchor = "auto",
                              xshift = 0,
                              yshift = 0,
                              font = list(size = 10))
        )
      fig

      self$plots$fa_analysis_plot <- fig
    }
  )
)
