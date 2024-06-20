# get all file names
# hidden files are not selected
incl_files <- list.files(path = "./",
                         recursive = TRUE)

# remove certain files
incl_files <- incl_files[!grepl(x = incl_files,
                                pattern = "^(240220.*|neurolipid_meta.csv)$")]
incl_files <- incl_files[!grepl(x = incl_files,
                                pattern = "^(install_packages.R|deploy.R)$")]
incl_files <- incl_files[!grepl(x = incl_files,
                                pattern = "^tests.*")]
incl_files <- incl_files[!grepl(x = incl_files,
                                pattern = "^(data/Database/NLA_2024_701/Norm_mgprotein_40.xlsx|data/Database/NLA_2024_801/Raw_concatenated.xlsx)$")]
incl_files <- incl_files[!grepl(x = incl_files,
                                pattern = "^(soda_home.html|soda-light.Rproj)$")]
incl_files <- incl_files[!grepl(x = incl_files,
                                pattern = "^rsconnect/shinyapps.io/cpm-lumc/soda-light.dcf$")]


# deploy the app
rsconnect::deployApp(appFiles = incl_files)