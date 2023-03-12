
#==============================#
###### Importing Packages ######
#==============================#

pacman::p_load("tmap", "ExPanDaR", "kableExtra", "ggstatsplot", "plotly", "DT", "scales", "shiny", "shinydashboard", "fresh", "tidyverse")

#==============================#
###### Data Manipulation ######
#==============================#
touristdata_clean <- read_csv("data/touristdata_clean.csv")

touristdata_clean <- touristdata_clean %>%
  filter(total_cost > 0,
         total_tourist > 0) %>%
  mutate(cost_per_pax = round(total_cost/total_tourist,0))

#========================#
###### Custom Theme ######
#========================#

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#2a2d34"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#80C2AF",
    dark_hover_bg = "#5C946E",
    dark_color = "#2a2d34"
  ),
  adminlte_global(
    content_bg = "#A0DDE6",
    box_bg = "#80C2AF", 
    info_box_bg = "#80C2AF"
  )
)

#========================#
###### Shiny UI ######
#========================#

# Dashboard Header ----------------------------------------------------
header <- dashboardHeader(
  title = "Tanzania Tourism")

# Dashboard Sidebar ----------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "information", icon = icon("info")),
    menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard"))
    )
  )

# Dashboard Body ----------------------------------------------------
body <- dashboardBody(
  
  # Setting theme  ----------------------------------------------------
  use_theme(mytheme),

  # Dashboard Body Tabs  ----------------------------------------------------
  tabItems(
    tabItem(tabName = "information",
            h2("About the app")
            ),
    
    tabItem(tabName = "tab_dashboard",
            h2("Tanzania Tourism at a Glance")
            )
    )
  )

# User Interface  ----------------------------------------------------
ui <- dashboardPage(header, sidebar, body)


#========================#
###### Shiny Server ######
#========================#
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
