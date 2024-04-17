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

  line_exp <- '<table>
<tr>
<th>Experiment</th>
<th>Links</th>
<th>Genotype</th>
<th>Cell type</th>
<th>Parental cell line</th>
<th>Cell line</th>
</tr>\n'
  for(experiment in experiments) {
    genoType <- unique(meta_data$genoType[meta_data$experimentId == experiment])
    genoType <- genoType[!is.na(genoType)]
    genoType <- genoType[genoType != "NA"]
    genoType <- paste(genoType, collapse = " | ")

    cellType <- unique(meta_data$cellType[meta_data$experimentId == experiment])
    cellType <- cellType[!is.na(cellType)]
    cellType <- cellType[cellType != "NA"]
    cellType <- paste(cellType, collapse = " | ")

    parentCellLine <- unique(meta_data$parentalCellLineBrainregion[meta_data$experimentId == experiment])
    parentCellLine <- parentCellLine[!is.na(parentCellLine)]
    parentCellLine <- parentCellLine[parentCellLine != "NA"]
    parentCellLine <- paste(parentCellLine, collapse = " | ")

    cellLine <- unique(meta_data$cellLineName[meta_data$experimentId == experiment])
    cellLine <- cellLine[!is.na(cellLine)]
    cellLine <- cellLine[cellLine != "NA"]
    cellLine <- paste(cellLine, collapse = " | ")
    tmp <- paste("<tr><td>", experiment, "</td>",
                 "<td><a href=\"https://ricoderks.shinyapps.io/soda-light/?experimentId=", experiment, "\" target=\"_blank\">Online</a><br>",
                 "<a href=\"http://2019-9014:3839/soda-light/?experimentId=", experiment, "\" target=\"_blank\">Old</a><br>",
                 "<a href=\"http://2019-9014:3839/soda-light_dev/?experimentId=", experiment, "\" target=\"_blank\">New</a>",
                 "</td><td>",
                 genoType,
                 "</td><td>",
                 cellType,
                 "</td><td>",
                 parentCellLine,
                 "</td><td>",
                 cellLine,
                 "</td></tr>\n",
                 sep = "")
    line_exp <- c(line_exp, tmp)
  }
  line_exp <- c(line_exp, '</table>')

  lines_exp <- paste(line_exp, collapse = "")

  first_part <-
    '<!DOCTYPE html>
   <html lang="">
     <head>
       <meta charset="utf-8">
       <title>Experiments</title>
       <style>
         tr:nth-child(even) {
           background-color: #D6EEEE;
         }
       </style>
     </head>
     <body>
       <header></header>
         <main>'
  last_part <-
    '     </main>
      <footer></footer>
    </body>
  </html>'


  writeLines(c(first_part, lines_exp, last_part),
             "./soda_home.html")
}