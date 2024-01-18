dev <- FALSE

if(dev){
  library(fresh)

  create_theme(
    # main layout color
    bs4dash_layout(
      main_bg = "#ffffff"
    ),
    bs4dash_status(
      danger = "#db285a",
      info = "#ededed"
    ),
    bs4dash_sidebar_light(
      bg = "#ededed",  # working for bg whole sidebar
      # header_color = "#094af9",  # not working
      color = "#db285a",  # working, text color
      hover_color = "#0255e9"
      # submenu_bg = "#FF0000",
      # submenu_color = "#FFF",
      # submenu_hover_color = "#FFF"
    ),
    bs4dash_sidebar_dark(
      bg = "#ededed",  # working for bg whole sidebar
      color = "#db285a", #"#0255e9",  # working, text color NOT working
      # header_color = "#094af9"
      hover_color = "#0255e9",
      # submenu_bg = "#272c30",
      # submenu_color = "#FFF",
      # submenu_hover_color = "#FFF"
    ),

    # define a file name for output (has to be in www folder)
    output_file = file.path("www", "custom.css")
  )

  # not all things are working
  # dirty hack here
  css <- readLines(file.path("www", "custom.css"))
  # fix bg color on top of page
  css_new <- gsub(x = css,
                  pattern = "navbar-white\\{background-color:#fff\\}",
                  replacement = "navbar-white\\{background-color:#ededed\\}")
  # fix, make footer smaller
  css_new <- gsub(x = css_new,
                  pattern = "main-footer\\{background:#fff;border-top:1px solid #dee2e6;color:#869099;padding:1rem\\}",
                  replacement = "main-footer\\{background:#fff;border-top:1px solid #dee2e6;color:#869099;padding:0.5rem\\}")

  # write back
  writeLines(text = css_new,
             file.path("www", "custom.css"))
}