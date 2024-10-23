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
        color_palette = 'Set1',
        img_format = "png"
      ),

      # Class comparison parameters
      class_comparison = list(
        dataset = 'Class table total normalized',
        group_col = NULL,
        color_palette = 'Set1',
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
        color_palette = 'Set1',
        selected_function = "mean",
        selected_test = "t-Test",
        img_format = "png"
      ),

      # Heatmap parameters self$params$heatmap$
      heatmap = list(
        dataset = 'Z-scored total normalized table',
        impute = TRUE,
        cluster_samples = TRUE,
        sample_color_palette = "Set1",
        cluster_features = TRUE,
        map_sample_data = NULL,
        map_feature_data = "lipid_class",
        group_column_da = NULL,
        apply_da = TRUE,
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
        colors_palette = 'Set1',
        img_format = "png"
      ),

      # Fatty acid analysis parameters self$params$fa_analysis_plot$
      fa_analysis_plot = list(
        data_table = "Total normalized table",
        feature_meta = NULL,
        sample_meta = "Raw meta table",
        group_col = NULL,
        selected_view = "lipidclass",
        selected_lipidclass = "All",
        fa_norm = FALSE,
        color_palette = "Set1",
        img_format = "png"
      ),

      # Fatty acid analysis heatmap self$params$fa_comp_plot$
      fa_comp_plot = list(
        data_table = "Total normalized table",
        sample_meta = "Raw meta table",
        composition = "fa_tail",
        feature_meta = NULL,
        group_col = NULL,
        group_1 = NULL,
        group_2 = NULL,
        selected_lipidclass = "CE",
        color_palette = "Blues",
        img_format = "png"
      )
    ),
    #----------------------------------------------------Hard coded settings----
    hardcoded_settings = list(
      # general
      meta_column = c(
        "Culture conditions" = "cultureConditions",
        "Genotype" = "genoType",
        "Parental cell line / Brain region" = "parentCellLineBrainregion",
        "Sample type" = "sampleType",
        "Sex" = "sex",
        "Treatment/Diagnosis" = "treatmentDiagnosis"
      ),
      color_palette = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                        "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
                        "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "BrBG", "PiYG", "PRGn",
                        "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent",
                        "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
                        "Magma", "Inferno", "Plasma", "Viridis", "Cividis", "Rocket",
                        "Mako", "Turbo", "plotly_1", "plotly_2", "ggplot2"),
      image_format = c("png", "svg", "jpeg", "webp"),

      # plot specific
      class_distribution = list(
        datasets = list(
          # "Lipid classes (absolute conc.)" = "Class table",
          "Lipid classes (normalized, % of total lipid classes)" = "Class table total normalized"
        )
      ),

      class_comparison = list(
        datasets = list(
          # "Lipid classes (absolute conc.)" = "Class table",
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
          "Tail 1 number of carbons" = "carbons_1",
          "Tail 2 number of carbons" = "carbons_2",
          "Total number of carbons" = "carbons_sum",
          "Tail 1 number of double bonds" = "unsat_1",
          "Tail 2 number of double bonds" = "unsat_2",
          "Total number of double bonds" = "unsat_sum"
        )
      ),

      heatmap = list(
        datasets = list(
          # "Lipid species (z-scores)" = "Z-scored table",
          "Lipid species (z-scores, normalized, % of total lipids)" = "Z-scored total normalized table",
          "Lipid species (z-scores, normalized, % of total lipids within class)" = "Z-scored class normalized table",
          # "Lipid classes (z-scores)" = "Class table z-scored",
          "Lipid classes (z-scores, normalized, % of total lipids)" = "Class table z-scored total normalized"
        ),
        map_cols = list(
          "Lipid classes" = "lipid_class",
          "Tail 1 number of carbons" = "carbons_1",
          "Tail 2 number of carbons" = "carbons_2",
          "Total number of carbons" = "carbons_sum",
          "Tail 1 number of double bonds" = "unsat_1",
          "Tail 2 number of double bonds" = "unsat_2",
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
        feature_metadata = list(
          "None" = "None",
          "Lipid classes" = "lipid_class",
          "Tail 1 number of carbons" = "carbons_1",
          "Tail 2 number of carbons" = "carbons_2",
          "Total number of carbons" = "carbons_sum",
          "Tail 1 number of double bonds" = "unsat_1",
          "Tail 2 number of double bonds" = "unsat_2",
          "Total number of double bonds" = "unsat_sum"
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
      ),
      fa_composition = list(
        composition_options = list(
          "Fatty acid tail" = "fa_tail",
          "Total lipid" = "total_lipid"
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

      excluded_cols = NULL,

      # the group column used for blank filtering
      group_col_blank = NULL
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
      fa_analysis_plot = NULL,
      fa_comp_plot = NULL
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

    param_heatmap = function(dataset, impute, cluster_samples, cluster_features, map_sample_data, map_feature_data, sample_color_palette, group_column_da, apply_da, alpha_da, color_palette, reverse_palette, factor_height, img_format) {
      self$params$heatmap$dataset = dataset
      self$params$heatmap$impute = impute
      self$params$heatmap$cluster_samples = cluster_samples
      self$params$heatmap$cluster_features = cluster_features
      self$params$heatmap$map_sample_data = map_sample_data
      self$params$heatmap$sample_color_palette = sample_color_palette
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

    param_fa_analysis_plot = function(data_table, feature_meta, sample_meta, group_col, selected_view, selected_lipidclass, selected_fa, fa_norm, color_palette, img_format) {
      self$params$fa_analysis_plot$data_table = data_table
      self$params$fa_analysis_plot$feature_meta = feature_meta
      self$params$fa_analysis_plot$sample_meta = sample_meta
      self$params$fa_analysis_plot$group_col = group_col
      self$params$fa_analysis_plot$selected_view = selected_view
      self$params$fa_analysis_plot$selected_lipidclass = selected_lipidclass
      self$params$fa_analysis_plot$selected_fa = selected_fa
      self$params$fa_analysis_plot$fa_norm = fa_norm
      self$params$fa_analysis_plot$color_palette = color_palette
      self$params$fa_analysis_plot$img_format = img_format
    },

    param_fa_comp_plot = function(data_table, sample_meta, composition, feature_meta, group_col, group_1, group_2, selected_lipidclass, color_palette, img_format) {
      self$params$fa_comp_plot$data_table = data_table
      self$params$fa_comp_plot$sample_meta = sample_meta
      self$params$fa_comp_plot$composition = composition
      self$params$fa_comp_plot$feature_meta = feature_meta
      self$params$fa_comp_plot$group_col = group_col
      self$params$fa_comp_plot$group_1 = group_1
      self$params$fa_comp_plot$group_2 = group_2
      self$params$fa_comp_plot$group_2 = group_2
      self$params$fa_comp_plot$selected_lipidclass = selected_lipidclass
      self$params$fa_comp_plot$color_palette = color_palette
      self$params$fa_comp_plot$img_format = img_format
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
          # this one is used by soda-light
          # Filtering alone
          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       # provide the column name for the group filtering
                                       group_col = "group_col_blank", #self$indices$group_col,
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
      batches <- unique(self$tables$imp_meta$batchNumber)

      qc_cells <- c()
      qc_plasma <- c()
      for(batch in batches) {
        id_cells <- paste(batch, self$tables$imp_meta[tolower(self$tables$imp_meta$sampleType) == "quality control cells" &
                                                        self$tables$imp_meta$batchNumber == batch, "analystId"], sep = "_")
        id_plasma <- paste(batch, self$tables$imp_meta[tolower(self$tables$imp_meta$sampleType) == "quality control plasma" &
                                                         self$tables$imp_meta$batchNumber == batch, "analystId"], sep = "_")
        # get the data
        tmp_cells <- self$tables$imp_data[rownames(self$tables$imp_data) %in% id_cells, ]
        tmp_plasma <- self$tables$imp_data[rownames(self$tables$imp_data) %in% id_plasma, ]

        # fix QC naming
        if(nrow(tmp_cells) > 0) {
          tmp_cells$ID <- id_cells
        }
        if(nrow(tmp_plasma) > 0) {
          tmp_plasma$ID <- id_plasma
        }

        qc_cells <- rbind(qc_cells, tmp_cells)
        qc_plasma <- rbind(qc_plasma, tmp_plasma)
      }

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
      self$param_class_distribution(dataset = self$params$class_distribution$dataset,
                                    group_col = self$indices$group_col,
                                    color_palette = self$params$class_distribution$color_palette,
                                    img_format = self$params$class_distribution$img_format)

      self$param_class_comparison(dataset = self$params$class_comparison$dataset,
                                  group_col = self$indices$group_col,
                                  color_palette = self$params$class_comparison$color_palette,
                                  img_format = self$params$class_comparison$img_format)

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
                              color_palette = 'Set1',
                              selected_function = "mean",
                              selected_test = "t-Test",
                              img_format = "png")

      self$param_heatmap(dataset = 'Z-scored total normalized table',
                         impute = TRUE,
                         cluster_samples = TRUE,
                         cluster_features = TRUE,
                         map_sample_data = self$indices$group_col,
                         map_feature_data = "lipid_class",
                         sample_color_palette = "Set1",
                         group_column_da = self$indices$group_col,
                         apply_da = self$params$heatmap$apply_da,
                         alpha_da = 0.8,
                         color_palette = 'RdYlBu',
                         reverse_palette = FALSE,
                         factor_height = 2,
                         img_format = "png")

      self$param_pca(auto_refresh = TRUE,
                     data_table = 'z_scored_total_norm_data',
                     sample_groups_col = self$indices$group_col,
                     sample_groups_col_shape = "",
                     feature_groups_col = "lipid_class",
                     apply_da = self$params$pca$apply_da,
                     alpha_da = 0.8,
                     pca_method = 'svd',
                     nPcs = 10,
                     displayed_pc_1 = 1,
                     displayed_pc_2 = 2,
                     completeObs = F,
                     displayed_plots = 'both',
                     colors_palette = 'Set1',
                     img_format = "png")

      self$param_fa_analysis_plot(data_table = self$tables$total_norm_data,
                                  feature_meta = self$tables$feature_table,
                                  sample_meta = self$tables$raw_meta,
                                  group_col = self$indices$group_col,
                                  selected_view = self$params$fa_analysis_plot$selected_view,
                                  selected_lipidclass = self$params$fa_analysis_plot$selected_lipidclass,
                                  selected_fa = self$params$fa_analysis_plot$selected_fa,
                                  fa_norm = self$params$fa_analysis_plot$fa_norm,
                                  color_palette = 'Set1',
                                  img_format = "png")

      self$param_fa_comp_plot(
        data_table = self$tables$total_norm_data,
        sample_meta = self$tables$raw_meta,
        composition = self$params$fa_comp_plot$composition,
        feature_meta = self$tables$feature_table,
        group_col = self$indices$group_col,
        group_1 = unique(self$tables$raw_meta[, self$indices$group_col])[1],
        group_2 = unique(self$tables$raw_meta[, self$indices$group_col])[2],
        selected_lipidclass = self$params$fa_comp_plot$selected_lipidclass,
        color_palette = "Blues",
        img_format = "png"
      )

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

      if(length(idx_group_1) <= 1 | length(idx_group_2) <= 1) {
        stop("not enough observations")
      }

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
                                                   impute_inf = FALSE)


      volcano_table$p_val = get_p_val(data_table = data_table,
                                      idx_group_1 = idx_group_1,
                                      idx_group_2 = idx_group_2,
                                      used_function = test,
                                      impute_na = FALSE)

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
          m = mean(as.matrix(table[s, c]), na.rm = TRUE)
          plot_table[c, g] = m
        }
      }

      # Store the plot_table
      self$tables$class_distribution_table = plot_table

      colors <- get_color_palette(groups = group_list,
                                  color_palette = color_palette,
                                  reverse_color_palette = TRUE)

      # Produce the plot
      plot_table$lipid_class <- rownames(plot_table)
      plot_table_long <- tidyr::pivot_longer(
        data = plot_table,
        cols = tidyr::contains(group_list),
        names_to = "groups",
        values_to = "value"
      )

      # make sure that the bars and legend colors are in the same order
      plot_table_long$groups <- factor(x = plot_table_long$groups,
                                       levels = sort(unique(plot_table_long$groups)),
                                       labels = sort(unique(plot_table_long$groups)))

      fig <- plot_table_long %>%
        plotly::plot_ly(
          type = "bar",
          x = ~lipid_class,
          y = ~value,
          color = ~groups,
          colors = unname(colors),
          hovertext = ~groups,
          hovertemplate = paste("Lipid class: %{x}<br>",
                                "Value: %{y:.3g}%<br>",
                                paste0("Group: %{hovertext}"),
                                "<extra></extra>")
        )

      fig <-  fig %>%
        plotly::layout(legend = list(orientation = 'h',
                                     xanchor = "center", x = 0.5),
                       yaxis = list(title = "%",
                                    tickformat = "digits"),
                       xaxis = list(title = list(text = "")))

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
      groups = sort(unique(meta_table[, group_col]))
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

      colors <- get_color_palette(groups = groups,
                                  color_palette = color_palette,
                                  reverse_color_palette = TRUE)

      # Plot list will be the list of subplots
      plot_list = c()

      j = 1
      for (c in class_list) {
        subplot = plot_ly(colors = unname(colors),
                          width = width,
                          height = height,
                          hovertemplate = paste("Group: %{x}<br>",
                                                "Value: %{y:.3g}%",
                                                "<extra></extra>"))
        # get the data for the lipid class
        # boxplot data
        d <- as.data.frame(data_table[, c, drop = FALSE])
        d$sampleId <- rownames(d)
        d <- merge(
          x = d,
          y = meta_table[, c("sampleId", group_col)],
          by.x = "sampleId",
          by.y = "sampleId",
          all.x = TRUE
        )
        colnames(d)[2:3] <- c("value", "groups")
        # make sure that the bars and legend colors are in the same order
        d$groups <- factor(x = d$groups,
                           levels = sort(unique(d$groups)),
                           labels = sort(unique(d$groups)))

        # bar data
        m <- as.data.frame(aggregate(d$value, by = list(d$groups), FUN = mean, na.rm = TRUE))
        colnames(m) <- c("groups", "value")

        subplot <- subplot |>
          plotly::add_trace(data = m,
                            type = "bar",
                            x = ~groups,
                            y = ~value,
                            color = ~groups,
                            showlegend = ifelse(j == 1, TRUE, FALSE)) |>
          plotly::add_trace(data = d,
                            type = "box",
                            x = ~groups,
                            y = ~value,
                            color = ~groups,
                            boxpoints = "all",
                            pointpos = 0,
                            line = list(color = "rgb(100, 100, 100)"),
                            marker = list(color = "rgb(100, 100, 100)"),
                            alpha = 1,
                            showlegend = FALSE)

        # add the title to the plot
        subplot = subplot |>
          plotly::add_annotations(
            text = paste0("<b>", c, "</b>"),
            x = 0.5,
            y = 1,
            yref = "paper",
            xref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 11)) |>
          plotly::layout(
            xaxis = list(showticklabels = FALSE),
            yaxis = list(tickfont = list(size = 8),
                         tickformat = "digits"),
            shapes = list(
              list(
                type = "rect",
                x0 = 0,
                x1 = 1,
                y0 = 1.,
                y1 = 1.2,
                yref = "paper",
                xref = "paper",
                fillcolor = "#0255e9",
                opacity = 0.4,
                line = list(color = "#0255e9",
                            width = 1,
                            opacity = 0.4)
              )
            ))
        plot_list[[j]] = subplot
        j = j + 1
      }

      fig = plotly::subplot(plot_list,
                            nrows = y_dim,
                            margin = 0.035) |>
        plotly::layout(legend = list(orientation = 'h',
                                     xanchor = "center",
                                     x = 0.5))

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
                            sample_color_palette = self$params$heatmap$sample_color_palette,
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
          # multiple annotations
          row_annotations_df = meta_table[, row_annotations]
          colnames(row_annotations_df) = stringr::str_replace_all(colnames(row_annotations_df), "_", " ")

          sample_colors <- c()
          for(a in 1:length(row_annotations)) {
            tmp <- get_color_palette(groups = sort(unique(row_annotations_df[, row_annotations[a]])),
                                     color_palette = sample_color_palette,
                                     reverse_color_palette = TRUE)
            sample_colors <- c(sample_colors, tmp)
          }
        } else {
          # 1 annotation
          row_names = row_annotations
          row_annotations_df = as.data.frame(meta_table[, row_annotations],
                                             row.names = rownames(meta_table))
          colnames(row_annotations_df) = stringr::str_replace_all(row_names, "_", " ")

          sample_colors <- get_color_palette(groups = sort(unique(row_annotations_df[, row_annotations])),
                                             color_palette = sample_color_palette,
                                             reverse_color_palette = TRUE)
        }
      }

      # Reorder the feature metadata according to the data_table order
      meta_table_features = meta_table_features[c(colnames(data_table)),]

      if (!is.null(col_annotations)) {
        clean_names = col_annotations
        col_annotations_df = as.data.frame(meta_table_features[, col_annotations],
                                           row.names = rownames(meta_table_features))
        colnames(col_annotations_df) = clean_names
      }

      if (impute) {
        print('Imputing NAs')
        # data_table[is.na(data_table)] <- min(data_table, na.rm = TRUE)
        # use the minimum value per sample
        data_table <- t(apply(data_table, 1, function(x) {
          x[is.na(x)] <- min(x, na.rm = TRUE)
          return(x)
        }))
      }

      # Get the color palette
      color_count = colors_switch(color_palette)
      color_palette <- get_colors(color_count = color_count,
                                  color_palette = color_palette)
      if(reverse_palette) {
        color_palette <- rev(color_palette)
      }

      # customise the x-axis labels
      # use group name and the last 3 number of the sample name
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
                                                colorbar_yanchor = "middle",
                                                colorbar_xpos = 1.15,
                                                colorbar_ypos = 0.65,
                                                width = width,
                                                height = factor_height * height,
                                                limits = c(zmin, zmax),
                                                subplot_widths = subplot_sizes$width,
                                                subplot_heights = subplot_sizes$height,
                                                col_side_colors = row_annotations_df,
                                                col_side_palette = sample_colors,
                                                row_side_colors = col_annotations_df,
                                                dendrogram = dendrogram_list)
      # labCol = xlabels)
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
    plot_fa_analysis = function(data_table = self$tables$total_norm_data,
                                feature_table = self$tables$feature_table,
                                sample_meta = self$tables$raw_meta,
                                group_col = self$params$fa_analysis_plot$group_col,
                                selected_view = self$params$fa_analysis_plot$selected_view,
                                selected_lipidclass = self$params$fa_analysis_plot$selected_lipidclass,
                                selected_fa = self$params$fa_analysis_plot$selected_fa,
                                fa_norm = self$params$fa_analysis_plot$fa_norm,
                                color_palette = self$params$fa_analysis_plot$color_palette,
                                width = NULL,
                                height = NULL) {
      ## At the moment this function is using the raw data table
      # do the calculations
      if(selected_view == "lipidclass") {
        res <- fa_analysis_calc(data_table = data_table,
                                feature_table = feature_table,
                                sample_meta = sample_meta,
                                selected_lipidclass = selected_lipidclass,
                                fa_norm = fa_norm)
        # column names are fa tail names, rownames sample names
      } else if(selected_view == "fa") {
        res <- fa_analysis_rev_calc(data_table = data_table,
                                    feature_table = feature_table,
                                    sample_meta = sample_meta,
                                    selected_fa = selected_fa,
                                    fa_norm = fa_norm)
      }

      # Produce the class x group table
      # add ID's, group's and make long
      res$ID <- rownames(res)
      res$group <- sample_meta[res$ID, group_col]
      res_long <- res |>
        tidyr::pivot_longer(cols = -c(ID, group),
                            names_to = "names",
                            values_to = "value")

      # calculate mean and stdev per group
      plot_table <- tapply(as.data.frame(res_long), list(res_long$group, res_long$names), function(x) {
        avg <- mean(x[, "value"], na.rm = TRUE)
        stdev <- sd(x[, "value"], na.rm = TRUE)

        return(list(avg = avg,
                    stdev = stdev,
                    names = x[1, "names"],
                    group = x[1, "group"]))
        # print(x)
      })

      plot_table <- do.call(rbind.data.frame, plot_table)

      # Store the plot_table
      self$tables$fa_analysis_table <- plot_table

      colors <- get_color_palette(groups = sort(unique(plot_table$group)),
                                  color_palette = color_palette,
                                  reverse_color_palette = TRUE)

      # set the main title for FA overview per lipid class
      if(selected_view == "lipidclass") {
        if(selected_lipidclass == "All") {
          main_title <- "All lipid classes (incl. TG)"
        } else if(selected_lipidclass == "All_noTG") {
          main_title <- "All lipid classes (excl. TG)"
        } else {
          main_title <- paste0("Lipid class: ", selected_lipidclass)
        }
        xlabel <- "Fatty acid chain"
      }

      if(fa_norm) {
        ylabel <- "%"
      } else {
        ylabel <- "Normalized value"
      }

      # set the main title for lipid class overview per fatty acids
      if(selected_view == "fa") {
        main_title <- paste0("FA tails: ", paste(selected_fa, collapse = ", "))
        xlabel <- "Lipid classes"
      }

      # plotting
      fig <- plotly::plot_ly(colors = unname(colors),
                             width = width,
                             height = height)

      for (grp in sort(unique(plot_table$group))) {
        fig <- fig |>
          plotly::add_trace(data = plot_table[plot_table$group == grp, ],
                            x = ~names,
                            y = ~avg,
                            color = ~as.factor(group),
                            type = "bar",
                            name = grp,
                            hovertext = ~stdev,
                            error_y = ~ list(array = stdev,
                                             color = "#000000"),
                            hovertemplate = paste("Fatty acid chain: %{x}<br>",
                                                  "Value: %{y:.3g} +/- %{hovertext:0.3g}<br>",
                                                  paste0("Group: ", grp),
                                                  "<extra></extra>"))
        fig <- fig |>
          plotly::layout(legend = list(orientation = 'h',
                                       xanchor = "center",
                                       x = 0.5),
                         xaxis = list(title = xlabel),
                         yaxis = list(title = ylabel))
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
                              font = list(size = 10)),
                       title = list(text = main_title,
                                    x = 0,
                                    xanchor = "left")
        )
      fig

      self$plots$fa_analysis_plot <- fig
    },

    # plot Fatty acid composition heatmaps
    plot_fa_comp = function(data_table = self$tables$total_norm_data,
                            sample_meta = self$tables$raw_meta,
                            composition = self$params$fa_comp_plot$composition,
                            feature_table = self$tables$feature_table,
                            group_col = self$params$fa_comp_plot$group_col,
                            group_1 = self$params$fa_comp_plot$group_1,
                            group_2 = self$params$fa_comp_plot$group_2,
                            selected_lipidclass = self$params$fa_comp_plot$selected_lipidclass,
                            color_palette = self$params$fa_comp_plot$color_palette,
                            width = NULL,
                            height = NULL) {
      # Get the color palette
      # color_count = colors_switch(color_palette)
      # color_palette = RColorBrewer::brewer.pal(color_count, color_palette)
      color_palette <- get_color_palette(groups = c(group_1, group_2),
                                         color_palette = color_palette)

      ## left side
      # heatmap
      hm_left_data <- fa_comp_hm_calc(data_table = data_table,
                                      sample_meta = sample_meta,
                                      composition = composition,
                                      feature_table = feature_table,
                                      group_col = group_col,
                                      selected_group = group_1,
                                      selected_lipidclass = selected_lipidclass)
      # bar left top
      bar_top_left_data <- data.frame(x = factor(colnames(hm_left_data),
                                                 levels = sort(as.numeric(colnames(hm_left_data))),
                                                 labels = sort(as.numeric(colnames(hm_left_data)))),
                                      y = colSums(hm_left_data))
      avg_carbon_left <- weighted.mean(x = as.numeric(as.character(bar_top_left_data$x)),
                                       w = bar_top_left_data$y)
      # bar left
      bar_left_data <- data.frame(x = factor(rownames(hm_left_data),
                                             levels = sort(as.numeric(rownames(hm_left_data)), decreasing = TRUE),
                                             labels = sort(as.numeric(rownames(hm_left_data)), decreasing = TRUE)),
                                  y = rowSums(hm_left_data))
      avg_unsat_left <- weighted.mean(x = as.numeric(as.character(bar_left_data$x)),
                                      w = bar_left_data$y)


      ## right side
      # heatmap
      hm_right_data <- fa_comp_hm_calc(data_table = data_table,
                                       sample_meta = sample_meta,
                                       composition = composition,
                                       feature_table = feature_table,
                                       group_col = group_col,
                                       selected_group = group_2,
                                       selected_lipidclass = selected_lipidclass)
      # bar right top
      bar_top_right_data <- data.frame(x = factor(colnames(hm_right_data),
                                                  levels = sort(as.numeric(colnames(hm_right_data))),
                                                  labels = sort(as.numeric(colnames(hm_right_data)))),
                                       y = colSums(hm_right_data))
      avg_carbon_right <- weighted.mean(x = as.numeric(as.character(bar_top_right_data$x)),
                                        w = bar_top_right_data$y)
      # bar right
      bar_right_data <- data.frame(x = factor(rownames(hm_right_data),
                                              levels = sort(as.numeric(rownames(hm_right_data)), decreasing = TRUE),
                                              labels = sort(as.numeric(rownames(hm_right_data)), decreasing = TRUE)),
                                   y = rowSums(hm_right_data))
      avg_unsat_right <- weighted.mean(x = as.numeric(as.character(bar_right_data$x)),
                                       w = bar_right_data$y)


      # get the min and max value for the heatmap colorbar
      min_value <- min(c(min(hm_left_data), min(hm_right_data)))
      max_value <- max(c(max(hm_left_data), max(hm_right_data)))

      ## plots
      # left side
      fig_hm_left <- fa_comp_heatmap(data = hm_left_data,
                                     vline = avg_carbon_left,
                                     hline = avg_unsat_left,
                                     composition = composition,
                                     color_limits = c(min_value, max_value),
                                     color_palette = color_palette)

      fig_bar_top_left <- plotly::plot_ly(
        data = bar_top_left_data,
        x = ~x,
        y = ~y,
        type = "bar",
        showlegend = FALSE,
        color = I("gray"),
        hovertemplate = paste("Number of carbons: %{x}<br>",
                              "Proportion: %{y:.3g}<br>",
                              "<extra></extra>")
      ) |>
        plotly::layout(
          xaxis = list(showticklabels = FALSE,
                       fixedrange = TRUE),
          yaxis = list(
            fixedrange = TRUE,
            title = list(
              text = "Proportion",
              standoff = 3,
              font = list(
                size = 10
              )
            )
          )
        )

      fig_bar_left <- plotly::plot_ly(
        data = bar_left_data,
        x = ~y,
        y = ~x,
        type = "bar",
        showlegend = FALSE,
        orientation = "h",
        color = I("gray"),
        hovertemplate = paste("Number of double bonds: %{y}<br>",
                              "Proportion: %{x:.3g}<br>",
                              "<extra></extra>")
      ) |>
        plotly::layout(
          xaxis = list(
            autorange = "reversed",
            fixedrange = TRUE,
            title = list(
              text = "Proportion",
              standoff = 3,
              font = list(
                size = 10
              )
            )
          ),
          yaxis = list(
            showticklabels = FALSE,
            fixedrange = TRUE,
            title = "")
        )

      # right side
      fig_hm_right <- fa_comp_heatmap(data = hm_right_data,
                                      vline = avg_carbon_right,
                                      hline = avg_unsat_right,
                                      composition = composition,
                                      color_limits = c(min_value, max_value),
                                      color_palette = color_palette,
                                      y_pos_right = TRUE,
                                      showlegend = TRUE)

      fig_bar_top_right <- plotly::plot_ly(
        data = bar_top_right_data,
        x = ~x,
        y = ~y,
        type = "bar",
        showlegend = FALSE,
        color = I("gray"),
        hovertemplate = paste("Number of carbons: %{x}<br>",
                              "Proportion: %{y:.3g}<br>",
                              "<extra></extra>")
      )|>
        plotly::layout(
          xaxis = list(
            showticklabels = FALSE,
            fixedrange = TRUE),
          yaxis = list(
            fixedrange = TRUE,
            side = "right",
            title = list(
              text = "Proportion",
              standoff = 3,
              font = list(
                size = 10
              )
            )
          )
        )

      fig_bar_right <- plotly::plot_ly(
        data = bar_right_data,
        x = ~y,
        y = ~x,
        type = "bar",
        showlegend = FALSE,
        orientation = "h",
        color = I("gray"),
        hovertemplate = paste("Number of double bonds: %{y}<br>",
                              "Proportion: %{x:.3g}<br>",
                              "<extra></extra>")
      ) |>
        plotly::layout(
          yaxis = list(
            showticklabels = FALSE,
            fixedrange = TRUE,
            title = ""),
          xaxis = list(
            fixedrange = TRUE,
            title = list(
              text = "Proportion",
              standoff = 3,
              font = list(
                size = 10
              )
            )
          )
        )

      # blank plot
      blank <- plotly::plot_ly(type = "scatter", mode = "markers")
      blank <- plotly::layout(blank,
                              xaxis = list(zeroline = FALSE,
                                           showticklabels = FALSE,
                                           showgrid = FALSE,
                                           fixedrange = TRUE),
                              yaxis = list(zeroline = FALSE,
                                           showticklabels = FALSE,
                                           showgrid = FALSE,
                                           fixedrange = TRUE))

      # set annotation for combined plots
      annotations <- list(
        list(
          x = 0.3,
          y = 0.975,
          text = paste0("<b>", group_1, "</b>"),
          font = list(size = 12),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        ),
        list(
          x = 0.7,
          y = 0.975,
          text = paste0("<b>", group_2, "</b>"),
          font = list(size = 12),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        )
      )

      # combine plots
      fig_top <- plotly::subplot(list(blank, fig_bar_top_left, fig_bar_top_right, blank),
                                 nrows = 1,
                                 widths = c(0.1, 0.4, 0.4, 0.1),
                                 titleY = TRUE)

      fig_bottom <- plotly::subplot(list(fig_bar_left, fig_hm_left, fig_hm_right, fig_bar_right),
                                    nrows = 1,
                                    widths = c(0.1, 0.4, 0.4, 0.1),
                                    titleX = TRUE,
                                    titleY = TRUE)

      fig <- plotly::subplot(list(fig_top, fig_bottom),
                             nrows = 2,
                             heights = c(0.2, 0.75),
                             titleX = TRUE,
                             titleY = TRUE) |>
        plotly::layout(title = list(
          text = ifelse(selected_lipidclass == "All",
                        paste0("<b>Lipid class: ", selected_lipidclass, " (excl. PA)</b>"),
                        paste0("<b>Lipid class: ", selected_lipidclass, "</b>")),
          size = 14
        ),
        annotations = annotations) |>
        plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "select2d", "lasso2d"))

      self$plots$fa_comp_plot <- fig
    }
  )
)
