
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
         cost_per_night = round(total_cost/total_night_spent,0),
         cost_per_pax_night = round(total_cost/total_tourist/total_night_spent,0))

# Map Data and Joining ----------------------------------------------------
data("World")

touristdata_clean_country <- touristdata_clean %>%
  group_by(country,code,region) %>%
  summarise(total_female = sum(total_female),
            total_male = sum(total_male),
            total_tourist = sum(total_tourist),
            total_cost = round(sum(total_cost),0),
            total_night_spent = round(sum(total_night_spent),0),
            cost_per_pax = round(mean(cost_per_pax),0),
            cost_per_night = round(mean(cost_per_night),0),
            cost_per_pax_night = round(mean(cost_per_pax_night),0),
            trips = n()) %>%
  mutate(avg_night_spent = round(total_night_spent/trips,0),
         .after = total_night_spent) %>%
  mutate(avg_cost = round(total_cost/trips,0),
         .after = total_cost) %>%
  ungroup()

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
    light_blue = "#030708",
    aqua = "#A0DDE6"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#E7E8EC",
    dark_hover_bg = "#5C946E",
    dark_color = "#030708"
  ),
  adminlte_global(
    content_bg = "#E7E8EC",
    box_bg = "#FFF", 
    info_box_bg = "#A0DDE6"
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
    menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
    menuItem("Data Analysis", tabName = "tab_analysis", icon = icon("chart-simple"))
  )
)

# Dashboard Body ----------------------------------------------------
body <- dashboardBody(
  
  # CSS style  ----------------------------------------------------
  tags$style("h2 { font-weight: bold }"),
  tags$style(".small-box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".box-header h3.box-title{ color: #2A2D34; font-weight: bold }"),
  tags$style(".box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".box { font-size: 90%}"),
  tags$style(".fa-dollar-sign {font-size:80%}"),
  tags$style(".fa-people-group {font-size:80%}"),
  tags$style(".fa-bed {font-size:80%}"),
  
  # Setting theme  ----------------------------------------------------
  use_theme(mytheme),
  
  # Dashboard Body Tabs  ----------------------------------------------------
  
  tabItems(
    
    ## Information  ----------------------------------------------------
    tabItem(tabName = "information",
            h2("About the app", style = "font-family: sans-serif;")
    ),
    
    ## Dashboard  ----------------------------------------------------
    tabItem(tabName = "tab_dashboard",
            h2("Tanzania Tourism at a Glance", style = "font-family: sans-serif;"),
            fluidRow(
              
              ### First Column  ----------------------------------------------------
              column(width = 7,
                     #### First Value Boxes  ----------------------------------------------------
                     fluidRow(
                       valueBoxOutput("topspender_", width = 4),
                       valueBoxOutput("avgspenttrip_", width = 4),
                       valueBoxOutput("avgspentnight_", width = 4)
                     ),
                     #### Second Value Boxes  ----------------------------------------------------
                     div(style = "padding = 0em; margin-top: 0em",
                         fluidRow(
                           valueBoxOutput("totalvisitors_", width = 4),
                           valueBoxOutput("avgnight_", width = 4),
                           valueBoxOutput("avgpartysize_", width = 4)
                         )),
                     #### Interactive Map  ----------------------------------------------------
                     fluidRow(
                       box(
                         title = "Filter Panel", background = "aqua",
                         width = 3,
                         div(style = "padding = 0em; margin-top: -1em",
                             selectInput(inputId = "mapmetric_",
                                         label = "Select Metrics:",
                                         choices = c("Total Visitors" = "total_tourist",
                                                     "Total Spending" = "total_cost",
                                                     "Average Spending per Trip" = "avg_cost",
                                                     "Average Night Spent" = "avg_night_spent",
                                                     "Average Spending per Night" = "cost_per_night",
                                                     "Average Individual Spending per Night" = "cost_per_pax_night"),
                                         selected = "total_tourist")),
                         div(style = "padding = 0em; margin-top: -1em",
                             selectInput(inputId = "mapclassification_",
                                         label = "Classification method:",
                                         choices = list("sd" = "sd", 
                                                        "equal" = "equal", 
                                                        "pretty" = "pretty", 
                                                        "quantile" = "quantile", 
                                                        "kmeans" = "kmeans", 
                                                        "hclust" = "hclust", 
                                                        "bclust" = "bclust", 
                                                        "fisher" = "fisher", 
                                                        "jenks" = "jenks"),
                                         selected = "jenks")),
                         div(style = "padding = 0em; margin-top: -1em",
                             sliderInput(inputId = "mapclasses_",
                                         label = "Number of classes:",
                                         min = 5,
                                         max = 10,
                                         value = c(5))),
                         div(style = "padding = 0em; margin-top: -1em",
                             numericInput(inputId = "minvisitors_",
                                          label = "Min Total Visitors:",
                                          min = 0,
                                          max = 100,
                                          value = 20))
                       ),
                       column(width = 9,
                              div(style = "padding = 0em; margin-left: -1.5em",
                                  tmapOutput("map_", 
                                             width = "100%",
                                             height = 340)
                              )
                       )
                     ),
              ),
              
              ### Second Column  ----------------------------------------------------
              column(width = 5,
                     #### Data Table  ----------------------------------------------------
                     div(style = "padding = 0em; margin-left: -1em; margin-right: -0.5em",
                         fluidRow(
                           box(
                             title = "Top Spending Country",
                             status = "primary",
                             width = 12,
                             collapsible = T,
                             dataTableOutput("datatable_")
                           ))
                     ),
                     
                     fluidRow()
                     
              )
            )
    ),
    
    ## Analysis  ----------------------------------------------------
    tabItem(tabName = "tab_analysis",
            h2("Exploratory and Confirmatory Data Analysis", style = "font-family: sans-serif;")
    )
    
  )
)

# User Interface  ----------------------------------------------------
ui <- dashboardPage(header, sidebar, body)


#========================#
###### Shiny Server ######
#========================#
server <- function(input, output) {
  
  # Data Manipulation  ----------------------------------------------------
  top_country <- reactive({
    touristdata_clean_country_sorted$country[1]
  })
  
  top_value <- reactive({
    scales::comma(touristdata_clean_country_sorted$total_cost[1]/1000000)
  })
  
  touristdatatable <- reactive({
    touristdata_clean_country_sorted %>%
      filter(total_tourist >= input$minvisitors_) %>%
      select(!c(2,3,4,5,9,11,14)) %>%
      rename("Country of Origin" = "country",
             "Total Visitors" = "total_tourist",
             "Total Spending" = "total_cost",
             "Average Night Spent" = "avg_night_spent",
             "Average Spending per Trip" = "avg_cost",
             "Average Spending per Night" = "cost_per_night",
             "Average Individual Spending per Night" = "cost_per_pax_night"
      )
  })
  
  map_metrics_text <- reactive({
    switch(input$mapmetric_,
           "total_tourist" = "Total Visitors",
           "total_cost" = "Total Spending (TZS)",
           "avg_cost" = "Average Spending per Trip (TZS)",
           "avg_night_spent" = "Average Night Spent",
           "cost_per_night" = "Average Spending per Night (TZS)",
           "cost_per_pax_night" = "Average Individual Spending per Night (TZS)")
  })
  
  # Dashboard Server  ----------------------------------------------------
  
  output$topspender_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(top_value(), "M TZS"), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("Top Spending Country: ",top_country()), style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$avgspenttrip_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$total_cost)/1000,0)), "K TZS"), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Spending per Trip", style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$avgspentnight_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$cost_per_night)/1000,0)), "K TZS"), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Spending per Night", style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$totalvisitors_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(sum(touristdata_clean$total_tourist),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Total Visitors in dataset", style = "font-size: 80%;"), 
      icon = icon("people-group"),
      color = "aqua"
    )
  })
  
  output$avgnight_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$total_night_spent),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Night Spent by Tourist", style = "font-size: 80%;"), 
      icon = icon("bed"),
      color = "aqua"
    )
  })
  
  output$avgpartysize_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$total_tourist),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Party Size", style = "font-size: 80%;"), 
      icon = icon("people-group"),
      color = "aqua"
    )
  })
  
  output$map_ <- renderTmap({
    tmap_mode("view")
    tm_shape(touristdata_clean_map %>%
               filter(total_tourist >= input$minvisitors_))+
      tm_fill(input$mapmetric_, 
              n = input$mapclasses_,
              style = input$mapclassification_, 
              palette="Blues", 
              id = "country",
              title = map_metrics_text()
      ) +
      tm_borders(col = "grey20",
                 alpha = 0.5) 
  })
  
  output$datatable_ <- renderDataTable({
    formatCurrency(
      datatable(touristdatatable(),
                rownames = FALSE,
                class = "compact",
                options = list(
                  pageLength = 5, 
                  autoWidth = TRUE,
                  scrollX = TRUE)),
      c(3,4,6,7), currency = 'TZS ', 
      interval = 3, 
      mark = ',', digits = 0
    )
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
