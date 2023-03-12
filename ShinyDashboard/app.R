
#==============================#
###### Importing Packages ######
#==============================#

pacman::p_load("tmap", "ExPanDaR", "kableExtra", "ggstatsplot", "plotly", "DT", "scales", "shiny", "shinydashboard", "fresh", "tidyverse")

#==============================#
###### Data Manipulation ######
#==============================#

# Base Data and Modification ----------------------------------------------------
touristdata_clean <- read_csv("data/touristdata_clean.csv")

touristdata_clean <- touristdata_clean %>%
  filter(total_cost > 0,
         total_tourist > 0,
         total_night_spent > 0) %>%
  mutate(cost_per_pax = round(total_cost/total_tourist,0),
         cost_per_night = round(total_cost/total_night_spent,0))

# Map Data and Joining ----------------------------------------------------
data("World")

touristdata_clean_country <- touristdata_clean %>%
  group_by(country,code,region) %>%
  summarise(total_female = sum(total_female),
            total_male = sum(total_male),
            total_tourist = sum(total_tourist),
            total_cost = round(sum(total_cost),0),
            total_night_spent = round(sum(total_night_spent),0)) %>%
  mutate(cost_per_pax = round(total_cost/total_tourist,0),
         cost_per_night = round(total_cost/total_night_spent,0))

touristdata_clean_map <- left_join(World, 
                                   touristdata_clean_country, 
                                   by = c("iso_a3" = "code")) %>%
  select(-c(2:15)) %>%
  na.omit()

# Aggregation and sorting by total cost ----------------------------------------------------
touristdata_clean_country_sorted <- touristdata_clean_country %>%
  arrange(desc(total_cost))

#========================#
###### Custom Theme ######
#========================#

# Create dashboard theme using fresh package ----------------------------------------------------
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#030708"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#80C2AF",
    dark_hover_bg = "#5C946E",
    dark_color = "#030708"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#80C2AF", 
    info_box_bg = "#80C2AF"
  )
)

# Title Logo ----------------------------------------------------
titlelogo <- tags$a(
  tags$img(
    src="tanzanialogo.jpg",
    height = '49.5',
    width = '100'))

#========================#
###### Shiny UI ######
#========================#

# Dashboard Header ----------------------------------------------------
header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 60px}"),
          tags$style(".main-header .logo {height: 60px}")
  ),
  title = titlelogo)

# Dashboard Sidebar ----------------------------------------------------
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
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
            h2("About the app", style = "font-family: sans-serif;")
            ),
    
    tabItem(tabName = "tab_dashboard",
            h2("Tanzania Tourism at a Glance", style = "font-family: sans-serif;")
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
