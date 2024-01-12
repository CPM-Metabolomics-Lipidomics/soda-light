dev <- FALSE

if(dev){
  library(fresh)

  create_theme(
    # status colors
    bs4dash_status(
      primary = "#5E81AC",
      danger = "#BF616A",
      light = "#272c30"
    ),
    # main layout color
    bs4dash_layout(
      main_bg = "#353c42"
    ),
    # define some default colors
    bs4dash_color(gray_900 = "#FFF",
                  white = "#272c30"),
    # define some things for the sidebar
    bs4dash_sidebar_light(
      bg = "#272c30",
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30",
      submenu_color = "#FFF",
      submenu_hover_color = "#FFF"
    ),
    # define some things for the navbar (depends also on status light/dark coloring)
    bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    # define color contrast
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    # define a file name for output (has to be in www folder)
    output_file = file.path("www", "custom.css")
  )
}