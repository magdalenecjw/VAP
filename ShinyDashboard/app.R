
#==============================#
###### Importing Packages ######
#==============================#

pacman::p_load("sf", "tmap", "ExPanDaR", "kableExtra", "ggstatsplot", "plotly", "DT", "scales", "caret", "rpart", "rpart.plot", "sparkline", "visNetwork", "shiny", "shinydashboard", "shinyWidgets", "fresh", "shinyjs", "tidyverse")

#==============================#
###### Data Manipulation ######
#==============================#

# Base Data and Modification ----------------------------------------------------
touristdata <- read_csv("data/touristdata_clean.csv")

touristdata_clean <- touristdata %>%
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

# Aggregation and sorting by total_cost ----------------------------------------------------
touristdata_clean_country_sorted <- touristdata_clean_country %>%
  arrange(desc(total_cost))

# Cumulative sum of total cost for Pareto Chart ----------------------------------------------------
touristdata_clean_country_cum <- touristdata_clean_country_sorted %>%
  select(country, code, total_cost) %>%
  mutate(cumfreq = cumsum(total_cost)) %>%
  mutate(cum = cumsum(total_cost)/sum(total_cost)) %>%
  slice_head(n = 5)

# Aggregation and sorting by total_tourist ----------------------------------------------------
touristdata_clean_country_sorted_tourist <- touristdata_clean_country %>%
  arrange(desc(total_tourist))

top_world_data_tourist <- touristdata_clean_country_sorted_tourist %>%
  filter(total_tourist >= 30)

# Cumulative sum of total visitor for Pareto Chart ----------------------------------------------------
touristdata_clean_country_cum_tourist <- touristdata_clean_country_sorted_tourist %>%
  select(country, code, total_tourist) %>%
  mutate(cumfreq = cumsum(total_tourist)) %>%
  mutate(cum = cumsum(total_tourist)/sum(total_tourist)) %>%
  slice_head(n = 5)

# Finding Top Countries by total_cost ----------------------------------------------------

top_world_data <- touristdata_clean_country_sorted %>%
  filter(total_tourist >= 20) %>%
  arrange(desc(total_cost))

top_europe_data <- touristdata_clean_country_sorted %>%
  filter(total_tourist >= 20,
         region == "Europe") %>%
  arrange(desc(total_cost))

top_americas_data <- touristdata_clean_country_sorted %>%
  filter(total_tourist >= 20,
         region == "Americas") %>%
  arrange(desc(total_cost))

top_africa_data <- touristdata_clean_country_sorted %>%
  filter(total_tourist >= 20,
         region == "Africa") %>%
  arrange(desc(total_cost))

top_asia_data <- touristdata_clean_country_sorted %>%
  filter(total_tourist >= 20,
         region == "Asia") %>%
  arrange(desc(total_cost))

top_oceania_data <- touristdata_clean_country_sorted %>%
  filter(region == "Oceania") %>%
  arrange(desc(total_cost))

# Data for Decision Tree and Boosted Tree ----------------------------------------------------
df_analysis <- touristdata %>% 
  select(!ID) %>% 
  select(!code) %>% 
  select(!country) %>%
  filter(total_cost > 0,
         total_tourist > 0,
         total_night_spent > 0) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(across(11:17, as.factor)) %>%
  mutate(first_trip_tz = as.factor(first_trip_tz))


# Convert binary function  ----------------------------------------------------
convertbinary <- function(x){
  ifelse(x==1,"Yes","No")
}

#colorset
colorset <-  c("#30C5FF", "#D8A48F", "#5C946E", "#E9D758", "#FF5964", "#80C2AF", "#A0DDE6", "#B37BA4", "#F18F01", "#006E90")

#========================#
###### Custom Theme ######
#========================#

# Create dashboard theme using fresh package ----------------------------------------------------
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#030708",
    aqua = "#A0DDE6",
    green = "#5C946E",
    yellow = "#E9D758"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#E7E8EC",
    dark_hover_bg = "#5C946E",
    dark_color = "#030708",
    dark_submenu_bg = "#80C2AF",
    dark_submenu_color = "#030708",
    dark_submenu_hover_color = "#FFF"
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
    id = "tabs",
    menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
    menuItem("Data Analysis", tabName = "tab_analysis", icon = icon("chart-simple"), startExpanded = TRUE,
             menuSubItem("Spending Behavior", tabName = "tab_spend"),
             menuSubItem("Regional Overview", tabName = "tab_country"),
             menuSubItem("Country Comparisons", tabName = "tab_country_compare")
    ),
    menuItem("Clustering", tabName = "tab_cluster", icon = icon("circle-nodes")),
    menuItem("Regression Model", tabName = "tab_pred", icon = icon("network-wired"),
             menuSubItem("Decision Tree", tabName = "tab_dt"),
             menuSubItem("Boosted Tree", tabName = "tab_bdt")
    ),
    menuItem("About", tabName = "tab_about", icon = icon("info"))
  )
)

# Dashboard Body ----------------------------------------------------
body <- dashboardBody(
  
  # CSS style  ----------------------------------------------------
  tags$style("h2 { font-family: sans-serif; font-weight: bold; }"),
  tags$style("h3 { font-family: sans-serif; font-weight: bold; }"),
  tags$style(".small-box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".small-box.bg-yellow { color: #2A2D34 !important; }"),
  tags$style(".box-header h3.box-title{ color: #2A2D34; font-weight: bold }"),
  tags$style(".box { font-size: 90%}"),
  tags$style(".box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".nav-tabs-custom .nav-tabs li.active { border-top-color: #E9D758 !important; }"),
  tags$style(".fa-dollar-sign {font-size:80%}"),
  tags$style(".fa-people-group {font-size:80%}"),
  tags$style(".fa-bed {font-size:80%}"),
  tags$style(".fa-circle-info {font-size:60%}"),
  
  #Adding text
  tags$head(tags$style(HTML(
    '.myClass { 
            font-size: 200%;
            line-height: 50px;
            text-align: left;
            font-family: sans-serif;
            font-weight: bold;
            padding: 0 15px;
            overflow: hidden;
            color: white;
            }'
  ))),
  tags$script(HTML('
                           $(document).ready(function() {
                           $("header").find("nav").append(\'<div id="pageHeader" class="myClass"></div>\');
                           })'
  )),
  
  # Setting theme  ----------------------------------------------------
  use_theme(mytheme),
  
  useShinyjs(),
  
  # Dashboard Body Tabs  ----------------------------------------------------
  
  tabItems(
    
    ## Dashboard  ----------------------------------------------------
    tabItem(tabName = "tab_dashboard",
            #h2("Tanzania Tourism at a Glance", style = "font-family: sans-serif;"),
            fluidRow(
              
              ### Dashboard First Column  ----------------------------------------------------
              column(width = 7,
                     #### Dashboard First Value Boxes  ----------------------------------------------------
                     fluidRow(
                       valueBoxOutput("dash_topspender_", width = 4),
                       valueBoxOutput("dash_avgspenttrip_", width = 4),
                       valueBoxOutput("dash_topvisitor_", width = 4)
                     ),
                     #### Dashboard Second Value Boxes  ----------------------------------------------------
                     div(style = "padding = 0em; margin-top: 0em",
                         fluidRow(
                           valueBoxOutput("dash_avgpartysize_", width = 4),
                           valueBoxOutput("dash_avgnight_", width = 4),
                           valueBoxOutput("dash_avgspentnight_", width = 4)
                         )),
                     
                     #### Dashboard Interactive Map  ----------------------------------------------------
                     fluidRow(
                       box(
                         title = tags$p("Map Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                         status = "primary",
                         background = "aqua",
                         solidHeader = TRUE,
                         collapsible = FALSE,
                         width = 3,
                         div(style = "padding = 0em; margin-top: -0.5em",
                             tags$p("ONLY manipulate map", style = "font-style: italic;")),
                         div(style = "padding = 0em; margin-top: -0.5em",
                             selectInput(inputId = "dash_mapmetric_",
                                         label = "Select metrics:",
                                         choices = c("Total Visitors" = "total_tourist",
                                                     "Total Spend (TZS)" = "total_cost",
                                                     "Average TZS/Trip" = "avg_cost",
                                                     "Average Nights" = "avg_night_spent",
                                                     "Average TZS/Night" = "cost_per_night",
                                                     "Average TZS/pax/Night" = "cost_per_pax_night"),
                                         selected = "total_tourist")),
                         div(style = "padding = 0em; margin-top: -1em",
                             selectInput(inputId = "dash_mapclassification_",
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
                             sliderInput(inputId = "dash_mapclasses_",
                                         label = "Number of classes:",
                                         min = 5,
                                         max = 10,
                                         value = c(5))),
                         div(style = "padding = 0em; margin-top: -1em",
                             numericInput(inputId = "dash_minvisitors_",
                                          label = "Min total visitors:",
                                          min = 0,
                                          max = 100,
                                          value = 20))
                       ),
                       
                       column(width = 9,
                              div(style = "padding = 0em; margin-left: -1.5em",
                                  tmapOutput("dash_map_", 
                                             width = "100%",
                                             height = "57vh")
                              )
                       )
                     ),
              ),
              
              ### Dashboard Second Column  ----------------------------------------------------
              column(width = 5,
                     #### Dashboard Data Table  ----------------------------------------------------
                     div(style = "padding = 0em; margin-left: -1em; margin-right: -0.5em",
                         fluidRow(
                           box(
                             title = "Top Spending Country",
                             status = "primary",
                             width = 12,
                             collapsible = T,
                             DT::dataTableOutput("dash_datatable_")
                           ))
                     ),
                     
                     #### Dashboard Pareto Chart  ----------------------------------------------------
                     fluidRow(
                       div(style = "padding = 0em; margin-top: -2em; margin-left: -1em; margin-right: -0.5em",
                           tabBox(
                             title = tags$p("Top 5 Countries", style = "font-family: sans-serif; font-weight: bold; font-size: 90%"),
                             width = 12,
                             
                             ##### Dashboard Spending Pareto ----------------------------------------------------
                             tabPanel(
                               title = tags$p("By spending", style = "font-weight: bold;"),
                               plotlyOutput("dash_spending_pareto_",
                                            height = "30vh",
                                            width = "100%")
                             ),
                             
                             ##### Dashboard Visitor Pareto  ----------------------------------------------------
                             tabPanel(
                               title = tags$p("By visitors", style = "font-weight: bold;"),
                               plotlyOutput("dash_visitor_pareto_",
                                            height = "30vh",
                                            width = "100%")
                             )
                             
                             
                             
                           )
                           
                       )
                     )
                     
              )
            )
    ),
    
    ## Analysis by Impact on Spending  ----------------------------------------------------
    tabItem(tabName = "tab_spend",
            #h3("Factors affecting Spending"),
            fluidRow(
              
              ### Analysis_Spending First Column  ----------------------------------------------------
              column(width = 2,
                     div(style = "padding = 0em; margin-right: -0.5em",
                         box(
                           title = tags$p("First Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                           status = "primary",
                           background = "aqua",
                           solidHeader = TRUE,
                           collapsible = FALSE,
                           width = 12,
                           div(style = "padding = 0em; margin-top: -0.5em",
                               selectInput(inputId = "spend_cat_",
                                           label = "Select category:",
                                           choices = list("Age group" = "age_group", 
                                                          "Travelling with" = "travel_with", 
                                                          "Trip purpose" = "purpose",
                                                          "Main activity" = "main_activity",
                                                          "Source of information" = "info_source",
                                                          "Tour arrangement" = "tour_arrangement",
                                                          "Incl. int'l. transport?" = "package_transport_int",
                                                          "Incl. accom?" = "package_accomodation",
                                                          "Incl. food?" = "package_food",
                                                          "Incl. dom. transport?" = "package_transport_tz",
                                                          "Incl. sightseeing?" = "package_sightseeing",
                                                          "Incl. guided tour?" = "package_guided_tour",
                                                          "Incl. insurance?" = "package_insurance",
                                                          "Mode of payment" = "payment_mode",
                                                          "First trip to TZA?" = "first_trip_tz",
                                                          "Most impressive attr." = "most_impressing"),
                                           selected = "tour_arrangement")),
                           div(style = "padding = 0em; margin-top: -1em",
                               selectInput(inputId = "spend_test_",
                                           label = "Test type:",
                                           choices = list("parametric" = "p", 
                                                          "non-parametric" = "np", 
                                                          "robust" = "r", 
                                                          "Bayes Factor" = "bf"),
                                           selected = "np")),
                           div(style = "padding = 0em; margin-top: 0em",
                               radioButtons(inputId = "spend_cf_",
                                            label = "Confidence level:",
                                            choices = c("95%" = 0.95,
                                                        "99%" = 0.99),
                                            selected = 0.95)),
                           div(style = "padding = 0em; margin-top: 0em",
                               checkboxInput(inputId = "spend_outliers_", 
                                             label = "Treat outliers",
                                             value = TRUE)),
                           div(style = "padding = 0em; margin-top: 0em",
                               tags$p("Refer to second panel to continue plotting", style = "font-style: italic;")),
                           
                         )
                     )
              ),
              
              ### Analysis_Spending Second Column  ----------------------------------------------------
              column(width = 10,
                     div(style = "padding = 0em; margin-left: -2em",
                         tabBox(
                           #title = h3("Hypothesis Testing"),
                           title = htmlOutput("spend_title_"),
                           width = 12,
                           height = "80vh",
                           
                           #### Analysis_Spending Scatter ----------------------------------------------------
                           tabPanel(
                             title = tags$p("Ind. Spending vs Night Spent by Category", style = "font-weight: bold;"),
                             fluidRow(
                               
                               #### Analysis_Spending Scatterplot Control Panel ----------------------------------------------------
                               column(width = 3,
                                      box(
                                        title = tags$p("Second Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                                        status = "primary",
                                        background = "aqua",
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        width = 12,
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            tags$p("Press button below to update graph", style = "font-style: italic;")),
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            actionButton(inputId = "spend_scatter_action_", 
                                                         label = "Update plot"))
                                        
                                      )
                                      
                               ),
                               
                               #### Analysis_Spending Scatterplot Plot ----------------------------------------------------
                               column(width = 9,
                                      plotOutput("spend_scatter_plot_",
                                                 height = "65vh")
                               )
                               
                               
                             )
                             
                           ),
                           
                           #### Analysis_Spending Box ----------------------------------------------------
                           tabPanel(
                             title = tags$p("Spending by Category", style = "font-weight: bold;"),
                             fluidRow(
                               
                               #### Analysis_Spending Boxviolin Control Panel ----------------------------------------------------
                               column(width = 3,
                                      box(
                                        title = tags$p("Second Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                                        status = "primary",
                                        background = "aqua",
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        width = 12,
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            selectInput(inputId = "spend_yaxis_",
                                                        label = "Select y-axis:",
                                                        choices = list("Spending per Trip" = "total_cost", 
                                                                       "Individual Spending per Trip" = "cost_per_pax", 
                                                                       "Spending per Night" = "cost_per_night",
                                                                       "Individual Spending per Night" = "cost_per_pax_night"),
                                                        selected = "cost_per_pax")),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            selectInput(inputId = "spend_plottype_",
                                                        label = "Plot type:",
                                                        choices = list("Box" = "box", 
                                                                       "Violin" = "violin", 
                                                                       "Box Violin" = "boxviolin"),
                                                        selected = "boxviolin")),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            checkboxInput(inputId = "spend_compare_", 
                                                          label = "Show pairwise comparison",
                                                          value = TRUE)),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            radioButtons(inputId = "spend_w_compare_", 
                                                         label = "Display comparison:", 
                                                         choices = c("significant" = "s",
                                                                     "non-significant" = "ns"),
                                                         selected = "ns")),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            tags$p("Press button below to update graph", style = "font-style: italic;")),
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            actionButton(inputId = "spend_boxplot_action_", 
                                                         label = "Update plot"))
                                        
                                      )
                               ),
                               
                               
                               #### Analysis_Spending Boxviolin Plot ----------------------------------------------------
                               column(width = 9,
                                      plotOutput("spend_box_plot_",
                                                 height = "65vh"))
                               
                               
                               
                             )
                           )
                           
                           
                         )
                         
                     )
              )
              
              
            )
    ),
    
    ## Analysis by Country  ----------------------------------------------------
    tabItem(tabName = "tab_country",
            #h3("Comparison among Regions and Countries"),
            fluidRow(
              ### Analysis_Country First Column  ----------------------------------------------------
              column(width = 2,
                     div(style = "padding = 0em; margin-right: -0.5em",
                         box(
                           title = tags$p("First Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                           status = "primary",
                           background = "aqua",
                           solidHeader = TRUE,
                           collapsible = FALSE,
                           width = 12,
                           div(style = "padding = 0em; margin-top: -0.5em",
                               radioButtons(inputId = "acou_reg_cou_", 
                                            label = "Analysed by:", 
                                            choices = c("Region" = "region",
                                                        "Country" = "country"),
                                            selected = "region")),
                           hidden(div(id = "acou_cou_div_",
                                      style = "padding = 0em; margin-top: -1em",
                                      selectInput(inputId = "acou_cou_",
                                                  label = "Select country (max 5):",
                                                  choices = list("Top (World)" = "top_world",
                                                                 "Top (Europe)" = "top_europe",
                                                                 "Top (Americas)" = "top_americas",
                                                                 "Top (Africa)" = "top_africa",
                                                                 "Top (Asia)" = "top_asia",
                                                                 "Top (Oceania)" = "top_oceania"),
                                                  selected = "top_world"))),
                           div(style = "padding = 0em; margin-top: -1em",
                               selectInput(inputId = "acou_test_",
                                           label = "Test type:",
                                           choices = list("parametric" = "p", 
                                                          "non-parametric" = "np", 
                                                          "robust" = "r", 
                                                          "Bayes Factor" = "bf"),
                                           selected = "np")),
                           div(style = "padding = 0em; margin-top: -1em",
                               radioButtons(inputId = "acou_cf_",
                                            label = "Confidence level:",
                                            choices = c("95%" = 0.95,
                                                        "99%" = 0.99),
                                            selected = 0.95)),
                           div(style = "padding = 0em; margin-top: 0em",
                               tags$p("Refer to second panel to continue plotting", style = "font-style: italic;")),
                         ))
              ),
              
              
              ### Analysis_Country Second Column  ----------------------------------------------------
              column(width = 10,
                     div(style = "padding = 0em; margin-left: -2em",
                         tabBox(
                           title = h3("Hypothesis Testing"),
                           width = 12,
                           height = "80vh",  
                           
                           #### Analysis_Country Numerical ----------------------------------------------------
                           tabPanel(
                             title = tags$p("Numerical Variables", style = "font-weight: bold;"),
                             fluidRow(
                               
                               #### Analysis_Country Numerical Control Panel ----------------------------------------------------
                               column(width = 3,
                                      box(
                                        title = tags$p("Second Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                                        status = "primary",
                                        background = "aqua",
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        width = 12,
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            selectInput(inputId = "acou_numvar_",
                                                        label = "Select y-axis:",
                                                        choices = list("Spending per Trip" = "total_cost", 
                                                                       "Individual Spending per Trip" = "cost_per_pax", 
                                                                       "Spending per Night" = "cost_per_night",
                                                                       "Individual Spending per Night" = "cost_per_pax_night",
                                                                       "Night Spent per Trip" = "total_night_spent",
                                                                       "Prop Night Spent in Mainland" = "prop_night_spent_mainland"),
                                                        selected = "total_cost")),
                                        div(style = "padding = 0em; margin-top: -0em",
                                            selectInput(inputId = "acou_plottype_",
                                                        label = "Plot type:",
                                                        choices = list("Box" = "box", 
                                                                       "Violin" = "violin", 
                                                                       "Box Violin" = "boxviolin"),
                                                        selected = "boxviolin")),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            checkboxInput(inputId = "acou_compare_", 
                                                          label = "Show pairwise comparison",
                                                          value = TRUE)),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            radioButtons(inputId = "acou_w_compare_", 
                                                         label = "Display comparison:", 
                                                         choices = c("significant" = "s",
                                                                     "non-significant" = "ns"),
                                                         selected = "ns")),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            checkboxInput(inputId = "acou_outliers_", 
                                                          label = "Treat outliers",
                                                          value = TRUE)),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            tags$p("Press button below to update graph", style = "font-style: italic;")),
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            actionButton(inputId = "acou_action_", 
                                                         label = "Update plot"))
                                      )
                               ),
                               
                               #### Analysis_Country Numerical Plot ----------------------------------------------------
                               column(width = 9,
                                      plotOutput("acou_num_plot_",
                                                 height = "65vh"
                                      )
                               ),
                               
                             )
                           ),
                           
                           tabPanel(
                             title = tags$p("Categorical Variables", style = "font-weight: bold;"),
                             fluidRow(
                               
                               #### Analysis_Country Categorical Control Panel ----------------------------------------------------
                               column(width = 3,
                                      box(
                                        title = tags$p("Second Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                                        status = "primary",
                                        background = "aqua",
                                        solidHeader = TRUE,
                                        collapsible = FALSE,
                                        width = 12,
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            selectInput(inputId = "acou_catvar_",
                                                        label = "Select y-axis:",
                                                        choices = list("Age group" = "age_group", 
                                                                       "Travelling with" = "travel_with", 
                                                                       "Trip purpose" = "purpose",
                                                                       "Main activity" = "main_activity",
                                                                       "Source of information" = "info_source",
                                                                       "Tour arrangement" = "tour_arrangement",
                                                                       "Incl. int'l. transport?" = "package_transport_int",
                                                                       "Incl. accom?" = "package_accomodation",
                                                                       "Incl. food?" = "package_food",
                                                                       "Incl. dom. transport?" = "package_transport_tz",
                                                                       "Incl. sightseeing?" = "package_sightseeing",
                                                                       "Incl. guided tour?" = "package_guided_tour",
                                                                       "Incl. insurance?" = "package_insurance",
                                                                       "Mode of payment" = "payment_mode",
                                                                       "First trip to TZA?" = "first_trip_tz",
                                                                       "Most impressive attr." = "most_impressing"),
                                                        selected = "age_group")),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            selectInput(inputId = "acou_catlabel_",
                                                        label = "Select label:",
                                                        choices = list("Percentage" = "percentage", 
                                                                       "Counts" = "counts"),
                                                        selected = "percentage")),
                                        div(style = "padding = 0em; margin-top: 0em",
                                            tags$p("Press button below to update graph", style = "font-style: italic;")),
                                        div(style = "padding = 0em; margin-top: -0.5em",
                                            actionButton(inputId = "acou_cat_action_", 
                                                         label = "Update plot"))
                                        
                                      )
                               ),
                               
                               #### Analysis_Country Categorical Plot ----------------------------------------------------
                               column(width = 9,
                                      plotOutput("acou_cat_plot_",
                                                 height = "65vh"
                                      )
                               )
                             )
                           )
                         )
                         
                         
                     )
                     
              )
              
              
              
            )
    ),
    
    ## Comparison by Country  ----------------------------------------------------
    tabItem(tabName = "tab_country_compare",
            #h3("Spending between Two Countries"),
            fluidRow(
              ### Analysis_Compare First Column  ----------------------------------------------------
              column(width = 2,
                     div(style = "padding = 0em; margin-right: -0.5em",
                         box(
                           title = tags$p("Panel", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                           status = "primary",
                           background = "aqua",
                           solidHeader = TRUE,
                           collapsible = FALSE,
                           width = 12,
                           div(style = "padding = 0em; margin-top: -0.5em",
                               selectInput(inputId = "acomp_yvar_",
                                           label = "Select y-axis:",
                                           choices = list("Spending per Trip" = "total_cost", 
                                                          "Individual Spending per Trip" = "cost_per_pax", 
                                                          "Spending per Night" = "cost_per_night",
                                                          "Individual Spending per Night" = "cost_per_pax_night"),
                                           selected = "total_cost")),
                           div(style = "padding = 0em; margin-top: -1em",
                               selectInput(inputId = "acomp_xvar_",
                                           label = "Select x-axis:",
                                           choices = list("Age group" = "age_group", 
                                                          "Travelling with" = "travel_with", 
                                                          "Trip purpose" = "purpose",
                                                          "Main activity" = "main_activity",
                                                          "Source of information" = "info_source",
                                                          "Mode of payment" = "payment_mode",
                                                          "Most impressive attr." = "most_impressing"),
                                           selected = "age_group")),
                           div(style = "padding = 0em; margin-top: -1em",
                               selectInput(inputId = "acomp_grvar_",
                                           label = "Select group:",
                                           choices = list("Tour arrangement" = "tour_arrangement",
                                                          "Incl. int'l. transport?" = "package_transport_int",
                                                          "Incl. accom?" = "package_accomodation",
                                                          "Incl. food?" = "package_food",
                                                          "Incl. dom. transport?" = "package_transport_tz",
                                                          "Incl. sightseeing?" = "package_sightseeing",
                                                          "Incl. guided tour?" = "package_guided_tour",
                                                          "Incl. insurance?" = "package_insurance",
                                                          "First trip to TZA?" = "first_trip_tz"),
                                           selected = "tour_arrangement")),
                           div(style = "padding = 0em; margin-top: 0em",
                               tags$p("Press button below to update graph", style = "font-style: italic;")),
                           div(style = "padding = 0em; margin-top: 0em",
                               actionButton(inputId = "acomp_eda_action_", 
                                            label = "Update plot"))
                         )
                     )
              ),
              
              ### Analysis_Compare Second Column  ----------------------------------------------------
              column(width = 10,
                     div(style = "padding = 0em; margin-left: -2em",
                         box(
                           status = "primary",
                           width = 12,
                           collapsible = FALSE,
                           fluidRow(
                             ##### Analysis_Compare EDA First Country ----------------------------------------------------
                             column(width = 6,
                                    align = "center",
                                    fluidRow(
                                      selectizeInput(inputId = "acomp_country1_",
                                                     width = "60%",
                                                     label = "Select country:",
                                                     choices = unique(top_world_data_tourist$country),
                                                     selected = "GERMANY"),
                                      div(style = "padding = 0em; margin-top: -1.5em",
                                          tags$p("only countries with at least 30 visitors are shown", style = "font-style: italic;"))
                                    ),
                                    fluidRow(
                                      plotlyOutput("acomp_eda_country1_",
                                                   height = "65vh",
                                                   width = "90%")
                                    )
                             ),
                             
                             ##### Analysis_Compare EDA Second Country ----------------------------------------------------
                             column(width = 6,
                                    align = "center",
                                    fluidRow(
                                      selectizeInput(inputId = "acomp_country2_",
                                                     width = "60%",
                                                     label = "Select country:",
                                                     choices = unique(top_world_data_tourist$country),
                                                     selected = "AUSTRALIA"),
                                      div(style = "padding = 0em; margin-top: -1.5em",
                                          tags$p("only countries with at least 30 visitors are shown", style = "font-style: italic;"))
                                    ),
                                    fluidRow(
                                      plotlyOutput("acomp_eda_country2_",
                                                   height = "65vh",
                                                   width = "90%")
                                    )
                             )
                           )
                         )
                         
                         
                         
                     )
                     
              )
              
            )
    ),
    
    ## Clustering  ----------------------------------------------------
    tabItem(tabName = "tab_cluster",
            #h3("Clustering Analysis")
    ),
    
    ## Decision Tree  ----------------------------------------------------
    tabItem(tabName = "tab_dt",
            #h3("Prediction by Decision Tree"),
            fluidRow(
              
              ### Decision Tree First Column  ----------------------------------------------------
              column(width = 3,
                     
                     #### Variable Selection ----------------------------------------------------
                     div(style = "padding = 0em; margin-right: -0.5em",
                         box(
                           title = tags$p("Model Initiation", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                           status = "primary",
                           background = "aqua",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           div(style = "padding = 0em; margin-top: -0.5em",
                               pickerInput(inputId = "dt_var_",
                                           label = "Variable Selection:",
                                           choices = list("Region" = "region", 
                                                          "Age group" = "age_group", 
                                                          "Travelling with" = "travel_with", 
                                                          "Trip purpose" = "purpose",
                                                          "Main activity" = "main_activity",
                                                          "Source of information" = "info_source",
                                                          "Tour arrangement" = "tour_arrangement",
                                                          "Incl. int'l. transport?" = "package_transport_int",
                                                          "Incl. accom?" = "package_accomodation",
                                                          "Incl. food?" = "package_food",
                                                          "Incl. dom. transport?" = "package_transport_tz",
                                                          "Incl. sightseeing?" = "package_sightseeing",
                                                          "Incl. guided tour?" = "package_guided_tour",
                                                          "Incl. insurance?" = "package_insurance",
                                                          "Mode of payment" = "payment_mode",
                                                          "First trip to TZA?" = "first_trip_tz",
                                                          "Most impressive attr." = "most_impressing",
                                                          "Total Visitors" = "total_tourist",
                                                          "Total Male Visitors" = "total_male",
                                                          "Total Female Visitors" = "total_female",
                                                          "Total Night Spent" = "total_night_spent",
                                                          "Prop. Night Spent in Mainland" = "prop_night_spent_mainland",
                                                          "Nights Spent in Mainland" = "night_mainland",
                                                          "Nights Spent in Zanzibar" = "night_zanzibar"),
                                           selected = colnames(df_analysis)[1:24],
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE))),
                           div(style = "padding = 0em; margin-top: -0.8em",
                               sliderInput(inputId = "dt_partition_",
                                           label = "Train-Test Partition Ratio:",
                                           min = 0.5,
                                           max = 0.95,
                                           value = c(0.8))),
                           div(style = "padding = 0em; margin-top: -0.8em",
                               actionButton(inputId = "dt_init_action_", 
                                            label = "Build Model"))
                           
                         )
                     ),
                     
                     
                     #### Model Tuning ----------------------------------------------------
                     hidden(div(id = "dt_model_tuning",
                                style = "padding = 0em; margin-right: -0.5em; margin-top: -1em",
                                box(
                                  title = tags$p("Model Tuning", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                                  status = "primary",
                                  background = "aqua",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  div(style = "padding = 0em; margin-top: -0.5em",
                                      sliderInput(inputId = "dt_minsplit_",
                                                  label = "Minimum Split:",
                                                  min = 5,
                                                  max = 20,
                                                  value = c(5))),
                                  div(style = "padding = 0em; margin-top: -0.8em",
                                      sliderInput(inputId = "dt_maxdepth_",
                                                  label = "Maximum Depth:",
                                                  min = 5,
                                                  max = 20,
                                                  value = c(10))),
                                  div(style = "padding = 0em; margin-top: -0.8em",
                                      checkboxInput(inputId = "dt_bestcp_", 
                                                    label = "Select Best CP",
                                                    value = TRUE)),
                                  hidden(div(id = "dt_cp_div_",
                                             style = "padding = 0em; margin-top: -0.8em",
                                             numericInput(inputId = "dt_cp_",
                                                          label = "Complexity Parameter:",
                                                          min = 0.005,
                                                          max = 1,
                                                          value = 0.01))),
                                  div(style = "padding = 0em; margin-top: -0.8em",
                                      actionButton(inputId = "dt_action_", 
                                                   label = "Tune Model"))
                                ))),
              ),
              
              ### Decision Tree Second Column  ----------------------------------------------------
              column(width = 9,
                     
                     #### Decision Tree cp error plot and table  ----------------------------------------------------
                     column(width = 6,
                            div(style = "padding = 0em; margin-left: -4em",
                                box(
                                  status = "primary",
                                  width = 12,
                                  collapsible = TRUE,
                                  plotOutput("dt_errorplot_",
                                             height = "33vh",
                                             width = "100%")
                                )),
                            div(style = "padding = 0em; margin-left: -4em",
                                box(
                                  status = "primary",
                                  width = 12,
                                  collapsible = TRUE,
                                  DT::dataTableOutput("dt_cp_datatable_",
                                                      height = "33vh",
                                                      width = "100%")
                                ))
                     ),
                     
                     
                     #### Decision Tree Model  ----------------------------------------------------
                     column(width = 6,
                            fluidRow(
                              div(style = "padding = 0em; margin-left: -2em; margin-right: -1em;",
                                  tabBox(
                                    #title = h3("Hypothesis Testing"),
                                    width = 12,
                                    height = "65vh",
                                    
                                    #### Decision Tree Tree ----------------------------------------------------
                                    tabPanel(
                                      title = tags$p("Decision Tree", style = "font-weight: bold;"),
                                      visNetworkOutput("dt_tree_",
                                                       width = "100%",
                                                       height = "50vh")
                                    ),
                                    
                                    #### Decision Tree Prediction vs Actual ----------------------------------------------------
                                    tabPanel(
                                      title = tags$p("Predicted vs Actual on Test Data", style = "font-weight: bold;"),
                                      plotOutput("dt_pred_actual_",
                                                 width = "100%",
                                                 height = "50vh")
                                    )
                                    
                                  )
                              )),
                            fluidRow(
                              div(style = "padding = 0em; margin-top: -1em; margin-left: -2em;",
                                  valueBoxOutput("dt_rmse_", width = 4)
                              ),
                              div(style = "padding = 0em; margin-top: -1em;",
                                  valueBoxOutput("dt_mae_", width = 4)
                              ),
                              div(style = "padding = 0em; margin-top: -1em;",
                                  valueBoxOutput("dt_r2_", width = 4)
                              )
                              )
                     )
              )
              
              
            )
    ),
    
    ## Boosted Tree  ----------------------------------------------------
    tabItem(tabName = "tab_bdt",
            #h3("Prediction by Boosted Tree")
    ),
    
    ## Information  ----------------------------------------------------
    tabItem(tabName = "tab_about",
            #h3("About the app")
    )
    
  )
)

# User Interface  ----------------------------------------------------
ui <- dashboardPage(header, sidebar, body)


#========================#
###### Shiny Server ######
#========================#
server <- function(input, output) {
  
  # Add Title  ----------------------------------------------------
  observeEvent(input$tabs, {
    header <- switch(input$tabs,
                     tab_about = "About the app",
                     tab_dashboard = "Dashboard",
                     tab_spend = "Spending Behavior",
                     tab_country = "Regional Overview",
                     tab_country_compare = "Country Comparison",
                     tab_cluster = "Clustering Analysis",
                     tab_dt = "Regression by Decision Tree",
                     tab_bdt = "Regression by Boosted Tree"
    )
    
    # you can use any other dynamic content you like
    shinyjs::html("pageHeader", header)
  })
  
  # Global Data Manipulation  ----------------------------------------------------
  top_country <- reactive({
    touristdata_clean_country_sorted$country[1]
  })
  
  top_value <- reactive({
    scales::comma(touristdata_clean_country_sorted$total_cost[1]/1000000)
  })
  
  top_tourist_country <- reactive({
    touristdata_clean_country_sorted_tourist$country[1]
  })
  
  top_tourist <- reactive({
    scales::comma(touristdata_clean_country_sorted_tourist$total_tourist[1])
  })
  
  
  
  # Dashboard Data Manipulation  ----------------------------------------------------
  dash_touristdatatable <- reactive({
    touristdata_clean_country_sorted %>%
      filter(total_tourist >= input$dash_minvisitors_) %>%
      select(!c(1,3,4,5,9,11,14)) %>%
      rename("Country" = "code",
             "Total Visitors" = "total_tourist",
             "Total Spend (TZS)" = "total_cost",
             "Avg Nights" = "avg_night_spent",
             "Avg TZS/Trip" = "avg_cost",
             "Avg TZS/Night" = "cost_per_night",
             "Avg TZS/pax/Night" = "cost_per_pax_night"
      )
  })
  
  dash_map_metrics_text <- reactive({
    switch(input$dash_mapmetric_,
           "total_tourist" = "Total Visitors",
           "total_cost" = "Total Spend (TZS)",
           "avg_cost" = "Avg TZS/Trip",
           "avg_night_spent" = "Avg Nights",
           "cost_per_night" = "Avg TZS/Night",
           "cost_per_pax_night" = "Avg TZS/pax/Night")
  })
  
  # Dashboard Server  ----------------------------------------------------
  output$dash_topspender_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0("TSZ ",top_value(), "m"), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("Top Spending Country: ",top_country()), style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$dash_avgspenttrip_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0("TSZ ",scales::comma(round(mean(touristdata_clean$total_cost)/1000,0)), "k"), style = "font-size: 60%;"), 
      subtitle = tags$p("Average TSZ/Trip", style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$dash_topvisitor_ <- renderValueBox({
    valueBox(
      value = tags$p(top_tourist(), style = "font-size: 60%;"), 
      subtitle = tags$p(paste0("Top Visiting Country: ",top_tourist_country()), style = "font-size: 80%;"), 
      icon = icon("people-group"),
      color = "green"
    )
  })
  
  output$dash_avgspentnight_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0("TSZ ",scales::comma(round(mean(touristdata_clean$cost_per_night)/1000,0)), "k"), style = "font-size: 60%;"), 
      subtitle = tags$p("Average TSZ/Night", style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
#  output$dash_totalvisitors_ <- renderValueBox({
#    valueBox(
#      value = tags$p(paste0(scales::comma(round(sum(touristdata_clean$total_tourist),0))), style = "font-size: 60%;"), 
#      subtitle = tags$p("Total Visitors in dataset", style = "font-size: 80%;"), 
#      icon = icon("people-group"),
#      color = "aqua"
#    )
#  })
  
  output$dash_avgnight_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$total_night_spent),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Nights Spent", style = "font-size: 80%;"), 
      icon = icon("bed"),
      color = "aqua"
    )
  })
  
  output$dash_avgpartysize_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$total_tourist),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Party Size", style = "font-size: 80%;"), 
      icon = icon("people-group"),
      color = "green"
    )
  })
  
  output$dash_map_ <- renderTmap({
    
    tmap_mode("view")
    tmap_options(check.and.fix = TRUE) +
      tm_shape(touristdata_clean_map %>%
                 filter(total_tourist >= input$dash_minvisitors_),
               bbox = c(-150, -20, 150, 70))+
      tm_fill(input$dash_mapmetric_, 
              n = input$dash_mapclasses_,
              style = input$dash_mapclassification_, 
              palette="YlGn", 
              id = "country",
              title = dash_map_metrics_text(),
              popup.vars = c(" " = paste0(input$dash_mapmetric_))
      ) +
      tm_view(view.legend.position = c("left","bottom")) +
      tm_borders(col = "grey20",
                 alpha = 0.5) 
    
    
  })
  
  output$dash_datatable_ <- DT::renderDataTable({
    formatCurrency(
      datatable(dash_touristdatatable(),
                rownames = FALSE,
                class = "compact",
                options = list(
                  pageLength = 5, 
                  lengthMenu = c(5, 10),
                  autoWidth = TRUE,
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all"),
                    list(width = '10px', targets = c(1))),
                  info = FALSE,
                  scrollX = TRUE)),
      c(3,4,6,7), currency = '', 
      interval = 3, 
      mark = ',', digits = 0
    )
  })
  
  output$dash_spending_pareto_ <- renderPlotly({
    
    t_spending_yaxis <- list(size = 12)
    
    plot_ly(touristdata_clean_country_cum) %>%
      add_trace(x = ~reorder(`code`,-`total_cost`), 
                y = ~`total_cost`, 
                type = "bar", name = "Total Spend (TZS)",
                marker = list(color = "#1F2F16"),
                hovertemplate = paste(touristdata_clean_country_cum$country,": TZS",format(round(touristdata_clean_country_cum$total_cost / 1e9, 2), trim = TRUE), "B")) %>%
      add_trace(x = ~reorder(`code`,-`total_cost`), 
                y = ~`cum`*100,
                type = "scatter", mode = "lines", 
                yaxis = "y2", name = "Cum. %",
                line = list(color = "#F4B266"),
                hovertemplate = paste(touristdata_clean_country_cum$country,": %{y:.2f}","%")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = ""),
             yaxis = list(title = list(text = "Total Spend (TZS)", font = t_spending_yaxis), showgrid = F),
             yaxis2 = list(overlaying = "y", side = "right", range = list(0, 100), showticklabels = FALSE),
             legend = list(orientation="h", yanchor="bottom",y=0.9,xanchor="top",x=0.2)) 
    
  })
  
  output$dash_visitor_pareto_ <- renderPlotly({
    
    t_visitor_yaxis <- list(size = 12)
    
    plot_ly(touristdata_clean_country_cum_tourist) %>%
      add_trace(x = ~reorder(`code`,-`total_tourist`), 
                y = ~`total_tourist`, 
                type = "bar", name = "Total Visitors",
                marker = list(color = "#1F2F16"),
                hovertemplate = paste(touristdata_clean_country_cum_tourist$country,": ",touristdata_clean_country_cum_tourist$total_tourist)) %>%
      add_trace(x = ~reorder(`code`,-`total_tourist`), 
                y = ~`cum`*100,
                type = "scatter", mode = "lines", 
                yaxis = "y2", name = "Cum. %",
                line = list(color = "#F4B266"),
                hovertemplate = paste(touristdata_clean_country_cum_tourist$country,": %{y:.2f}","%")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = ""),
             yaxis = list(title = list(text = "Total Visitors", font = t_visitor_yaxis), showgrid = F),
             yaxis2 = list(overlaying = "y", side = "right", range = list(0, 100), showticklabels = FALSE),
             legend = list(orientation="h", yanchor="bottom",y=0.9,xanchor="top",x=0.2)) 
    
  })
  
  
  # Analysis_Spend Data Manipulation  ----------------------------------------------------
  
  ## Dataset selection for no outlier treatment
  spend_data <- reactive({
    touristdata_clean %>%
      mutate(across(package_transport_int:package_insurance, convertbinary)) %>%
      mutate(across(first_trip_tz, convertbinary)) %>%
      drop_na()
  })
  
  ## Dataset selection for outlier treatment
  spend_data_nooutlier <- reactive({
    touristdata_clean %>%
      mutate(across(package_transport_int:package_insurance, convertbinary)) %>%
      mutate(across(first_trip_tz, convertbinary)) %>%
      drop_na() %>%
      treat_outliers() 
  })
  
  spend_category_text <- eventReactive(
    c(input$spend_scatter_action_, input$spend_boxplot_action_),
    ignoreInit = TRUE,
    {switch(input$spend_cat_,
            "age_group" = "Age group",
            "travel_with" = "Travelling with",
            "purpose" = "Trip purpose",
            "main_activity" = "Main activity",
            "info_source" = "Source of information",
            "tour_arrangement" = "Tour arrangement",
            "package_transport_int" = "Incl. int'l. transport?",
            "package_accomodation" = "Incl. accom?",
            "package_food" = "Incl. food?",
            "package_transport_tz" = "Incl. dom. transport?",
            "package_sightseeing" = "Incl. sightseeing?",
            "package_guided_tour" = "Incl. guided tour?",
            "package_insurance" = "Incl. insurance?",
            "payment_mode" = "Mode of payment",
            "first_trip_tz" = "First trip to TZA?",
            "most_impressing" = "Most impressive attr."
    )
    })
  
  ## Numerical Metrics text
  spend_yaxis_text <- reactive({
    switch(input$spend_yaxis_,
           "total_cost" = "Spending per Trip (TZS)",
           "cost_per_pax" = "Individual Spending per Trip (TZS)",
           "cost_per_night" = "Spending per Night (TZS)",
           "cost_per_pax_night" = "Individual Spending per Night (TZS)")
  })
  
  
  # Analysis_Spend Server  ----------------------------------------------------
  
  ## Title panel
  output$spend_title_ <- renderText({
    paste("<h3>by ", spend_category_text(),  "</h3>")
    
  })
  
  ## Wrap the scatter plot in eventReactive based on Update Plot Button
  spend_scatter_plotreact <- eventReactive(
    input$spend_scatter_action_, {
      grouped_ggscatterstats(data = if(input$spend_outliers_){spend_data_nooutlier()}else{spend_data()},
                             x = cost_per_pax, y = total_night_spent,
                             xlab = "Individual Spending per Trip (TZS)", ylab = "Total Nights Spent",
                             grouping.var = !!sym(input$spend_cat_),
                             results.subtitle = TRUE,
                             type = input$spend_test_,
                             conf.level = as.numeric(input$spend_cf_),
                             ggplot.component = scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))) #+ 
        #facet_wrap(vars(!!sym(input$spend_cat_)))
    })
  
  ## Wrap the box plot in eventReactive based on Update Plot Button
  spend_box_plotreact <- eventReactive(
    input$spend_boxplot_action_, {
      ggbetweenstats(data = if(input$spend_outliers_){spend_data_nooutlier()}else{spend_data()},
                     x = !!sym(input$spend_cat_), y = !!sym(input$spend_yaxis_),
                     plot.type = input$spend_plottype_,
                     xlab = spend_category_text(), ylab = spend_yaxis_text(),
                     type = input$spend_test_, pairwise.comparisons = input$spend_compare_, pairwise.display = input$spend_w_compare_, 
                     mean.ci = T, p.adjust.method = "fdr",  conf.level = as.numeric(input$spend_cf_)) +
        scale_color_manual(values = colorset) +
        scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
    })
  
  ## Render the scatter plot
  output$spend_scatter_plot_ <- renderPlot({
    spend_scatter_plotreact()
  })
  
  ## Render the box plot
  output$spend_box_plot_ <- renderPlot({
    spend_box_plotreact()
  })
  
  # Analysis_Country Data Manipulation  ----------------------------------------------------
  
  ## Select countrylist based on Region or Country Selection
  countrylist <- reactive({
    if(input$acou_reg_cou_ == "region"){
      unique(touristdata_clean$country)
    } else {
      switch(input$acou_cou_,
             "top_world" = unique(top_world_data$country[1:5]),
             "top_europe" = unique(top_europe_data$country[1:5]),
             "top_americas" = unique(top_americas_data$country[1:2]),
             "top_africa" = unique(top_africa_data$country[1:5]),
             "top_asia" = unique(top_asia_data$country[1:5]),
             "top_oceania" = unique(top_oceania_data$country[1:2]))
    }
  })
  
  ## Dataset selection for no outlier treatment
  acou_ANOVA <- eventReactive(
    input$acou_action_,{
      touristdata_clean %>%
        filter(country %in% countrylist()) %>%
        mutate(region = fct_reorder(region, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
        mutate(country = fct_reorder(country, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
        drop_na()
    })
  
  ## Dataset selection for outlier treatment
  acou_ANOVA_nooutlier <- eventReactive(
    input$acou_action_,{
      touristdata_clean %>%
        filter(country %in% countrylist()) %>%
        mutate(region = fct_reorder(region, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
        mutate(country = fct_reorder(country, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
        drop_na() %>%
        treat_outliers() 
    })
  
  ## Dataset selection categorical
  acou_barstats <- eventReactive(
    input$acou_cat_action_, {
      touristdata_clean %>%
        filter(country %in% countrylist()) %>%
        mutate(across(package_transport_int:package_insurance, convertbinary)) %>%
        mutate(across(first_trip_tz, convertbinary)) %>%
        drop_na()
    })
  
  ## Numerical Metrics text
  acou_ANOVA_metrics_text <- eventReactive(
    input$acou_action_,{
      switch(input$acou_numvar_,
             "total_cost" = "Spending per Trip (TZS)",
             "cost_per_pax" = "Individual Spending per Trip (TZS)",
             "cost_per_night" = "Spending per Night (TZS)",
             "cost_per_pax_night" = "Individual Spending per Night (TZS)",
             "total_night_spent" = "Night Spent per Trip",
             "prop_night_spent_mainland" = "Proportion of Night Spent in Mainland")
    })
  
  ## Categorical Metrics text
  acou_bar_metrics_text <- eventReactive(
    input$acou_cat_action_, {
      switch(input$acou_catvar_,
             "age_group" = "Age group",
             "travel_with" = "Travelling with",
             "purpose" = "Trip purpose",
             "main_activity" = "Main activity",
             "info_source" = "Source of information",
             "tour_arrangement" = "Tour arrangement",
             "package_transport_int" = "Incl. int'l. transport?",
             "package_accomodation" = "Incl. accom?",
             "package_food" = "Incl. food?",
             "package_transport_tz" = "Incl. dom. transport?",
             "package_sightseeing" = "Incl. sightseeing?",
             "package_guided_tour" = "Incl. guided tour?",
             "package_insurance" = "Incl. insurance?",
             "payment_mode" = "Mode of payment",
             "first_trip_tz" = "First trip to TZA?",
             "most_impressing" = "Most impressive attr."
      )
    })
  
  
  # Analysis_Country Server  ----------------------------------------------------
  
  ## Enable country selection when Country radio button is selected
  observe({
    toggle(id = "acou_cou_div_", condition = input$acou_reg_cou_ == "country", anim = TRUE)
  })
  
  ## Wrap the numerical plot in eventReactive based on Update Plot Button
  acou_num_plotreact <- eventReactive(
    input$acou_action_, {
      ggbetweenstats(data = if(input$acou_outliers_){acou_ANOVA_nooutlier()}else{acou_ANOVA()},
                     x = !!sym(input$acou_reg_cou_), y = !!sym(input$acou_numvar_),
                     plot.type = input$acou_plottype_,
                     xlab = str_to_title(input$acou_reg_cou_), ylab = acou_ANOVA_metrics_text(),
                     type = input$acou_test_, pairwise.comparisons = input$acou_compare_, pairwise.display = input$acou_w_compare_, 
                     mean.ci = T, p.adjust.method = "fdr",  
                     conf.level = as.numeric(input$acou_cf_)) +
        scale_color_manual(values = colorset) +
        scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
    })
  
  ## Wrap the categorical plot in eventReactive based on Update Plot Button
  acou_cat_plotreact <- eventReactive(
    input$acou_cat_action_, {
      ggbarstats(data = acou_barstats(),
                 x = !!sym(input$acou_catvar_), y = !!sym(input$acou_reg_cou_),
                 xlab = str_to_title(input$acou_reg_cou_), ylab = acou_bar_metrics_text(),
                 legend.title = acou_bar_metrics_text(),
                 type = input$acou_test_, 
                 conf.level = as.numeric(input$acou_cf_), 
                 label = input$acou_catlabel_,
                 package = "ggthemes", palette = "Tableau_10")
    })
  
  ## Render the numerical plot
  output$acou_num_plot_ <- renderPlot({
    acou_num_plotreact()
  })
  
  ## Render the categorical plot
  output$acou_cat_plot_ <- renderPlot({
    acou_cat_plotreact()
  })
  
  # Analysis_Compare Data Manipulation  ----------------------------------------------------
  
  ## Boxplot1 data
  acomp_boxplot1_data <- eventReactive(
    input$acomp_eda_action_,{
      touristdata_clean %>%
        filter(country == input$acomp_country1_) %>%
        mutate(across(package_transport_int:package_insurance, convertbinary)) %>%
        mutate(across(first_trip_tz, convertbinary)) %>%
        group_by(!!sym(input$acomp_grvar_)) %>%
        drop_na()
    })
  
  ## Boxplot2 data
  acomp_boxplot2_data <- eventReactive(
    input$acomp_eda_action_,{
      touristdata_clean %>%
        filter(country == input$acomp_country2_) %>%
        mutate(across(package_transport_int:package_insurance, convertbinary)) %>%
        mutate(across(first_trip_tz, convertbinary)) %>%
        group_by(!!sym(input$acomp_grvar_)) %>%
        drop_na()
    })
  
  ## Calculating y-axis limit
  acomp_boxplot_summary <- eventReactive(
    input$acomp_eda_action_, {
      touristdata_clean %>%
        filter(country %in% c(input$acomp_country1_, input$acomp_country2_)) %>%
        summarise(max = max(!!sym(input$acomp_yvar_)))
    })
  
  acomp_max_limit <- eventReactive(input$acomp_eda_action_,{acomp_boxplot_summary()$max})
  
  ## y-axis text
  acomp_box_yaxis_text <- eventReactive(input$acomp_eda_action_,{
    switch(input$acomp_yvar_,
           "total_cost" = "Spending per Trip (TZS)",
           "cost_per_pax" = "Individual Spending per Trip (TZS)",
           "cost_per_night" = "Spending per Night (TZS)",
           "cost_per_pax_night" = "Individual Spending per Night (TZS)")
  })
  
  ## x-axis text
  acomp_box_xaxis_text <- eventReactive(input$acomp_eda_action_,{
    switch(input$acomp_xvar_,
           "age_group" = "Age group",
           "travel_with" = "Travelling with",
           "purpose" = "Trip purpose",
           "main_activity" = "Main activity",
           "info_source" = "Source of information",
           "payment_mode" = "Mode of payment",
           "most_impressing" = "Most impressive attr.")
  })
  
  ## group text
  acomp_box_group_text <- reactive({
    switch(input$acomp_grvar_,
           "tour_arrangement" = "Tour arrangement",
           "package_transport_int" = "Incl. int'l. transport?",
           "package_accomodation" = "Incl. accom?",
           "package_food" = "Incl. food?",
           "package_transport_tz" = "Incl. dom. transport?",
           "package_sightseeing" = "Incl. sightseeing?",
           "package_guided_tour" = "Incl. guided tour?",
           "package_insurance" = "Incl. insurance?",
           "first_trip_tz" = "First trip to TZA?")
  })
  
  # Analysis_Compare Server  ----------------------------------------------------
  
  acomp_boxplot1 <- eventReactive(
    input$acomp_eda_action_, {
      ggplot(acomp_boxplot1_data(),
             aes(y = !!sym(input$acomp_yvar_), x = !!sym(input$acomp_xvar_))) + 
        geom_boxplot(aes(fill = !!sym(input$acomp_grvar_))) + 
        scale_fill_brewer(name = acomp_box_group_text(),palette="YlGnBu") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
        scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
    })
  
  acomp_boxplot2 <- eventReactive(
    input$acomp_eda_action_, {
      ggplot(acomp_boxplot2_data(),
             aes(y = !!sym(input$acomp_yvar_), x = !!sym(input$acomp_xvar_))) + 
        geom_boxplot(aes(fill = !!sym(input$acomp_grvar_))) + 
        scale_fill_brewer(name = NULL, palette="YlGnBu") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
        scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
    })
  
  ## Render the EDA country 1 plot
  output$acomp_eda_country1_ <- renderPlotly({
    
    t_acomp_axis_boxplot1 <- list(size = 12)
    t_acomp_tick_boxplot1 <- list(size = 10)
    
    acomp_boxplot1() %>%
      ggplotly() %>%
      layout(autosize = TRUE,
             plot_bgcolor='#e5ecf6',
             yaxis = list(title = list(text = acomp_box_yaxis_text(), font = t_acomp_axis_boxplot1),
                          tickmode = "linear",
                          dtick = 5000000, 
                          tick0 = 0, 
                          range = list(0, acomp_max_limit()*1.05),
                          tickfont = t_acomp_tick_boxplot1),
             xaxis = list(title = list(text = acomp_box_xaxis_text(), font = t_acomp_axis_boxplot1),
                          tickfont = t_acomp_tick_boxplot1),
             boxmode = "group",
             legend = list(orientation = 'h',
                           xanchor = "center",
                           x = 0.5,
                           yanchor = "top",
                           y = 1.15)) 
  })
  
  output$acomp_eda_country2_ <- renderPlotly({
    
    t_acomp_axis_boxplot2 <- list(size = 12)
    t_acomp_tick_boxplot2 <- list(size = 10)
    
    acomp_boxplot2() %>%
      ggplotly() %>%
      layout(autosize = TRUE,
             plot_bgcolor='#e5ecf6',
             yaxis = list(title = "",
                          tickmode = "linear",
                          dtick = 5000000, 
                          tick0 = 0, 
                          range = list(0, acomp_max_limit()*1.05),
                          tickfont = t_acomp_tick_boxplot2),
             xaxis = list(title = list(text = acomp_box_xaxis_text(), font = t_acomp_axis_boxplot2),
                          tickfont = t_acomp_tick_boxplot2),
             boxmode = "group",
             legend = list(orientation = 'h',
                           xanchor = "center",
                           x = 0.5,
                           yanchor = "top",
                           y = 1.15)) 
  })
  
  # DT Data Manipulation  ----------------------------------------------------
  dt_dataset <- eventReactive(
    input$dt_init_action_, {
      df_analysis %>%
        select(input$dt_var_, "total_cost")
    })
  
  dt_indices <- eventReactive(
    input$dt_init_action_, {
      set.seed(1234)
      caret::createDataPartition(dt_dataset()$total_cost, p =input$dt_partition_, list = FALSE)
      #rsample::initial_split(dt_dataset(), prop = input$dt_partition_)
    })
  
  dt_analysis_train <- eventReactive(
    input$dt_init_action_, {
      dt_dataset()[dt_indices(),]
      #rsample::training(dt_dataset_split())
    })
  
  dt_analysis_test <- eventReactive(
    input$dt_init_action_, {
      dt_dataset()[-dt_indices(),]
      #rsample::testing(dt_dataset_split())
    })
  
  # DT Server  ----------------------------------------------------
  ## Enable Complexity Parameter Selection when check box is selected
  observe({
    toggle(id = "dt_cp_div_", condition = input$dt_bestcp_ == FALSE, anim = TRUE)
  })
  
  observeEvent(input$dt_init_action_, {
    show(id = "dt_model_tuning", anim = TRUE)
  })
  
  ##Initial Model
  initialdtmodel <- eventReactive(
    input$dt_init_action_, {
      rpart(
        formula = total_cost ~ ., 
        data = dt_analysis_train(), 
        method = "anova",
        control = rpart.control(minsplit = 5, cp = 0.008, maxdepth = 10)
      )
    })
  
  ##Pruned Model
  ### Find best CP
  dt_bestcp_val <- eventReactive(
    input$dt_action_, {
      initialdtmodel()$cptable[which.min(initialdtmodel()$cptable[,"xerror"]),"CP"]
    })
  
  ### Tuned Model
  tuneddtmodel <- eventReactive(
    input$dt_action_, {
      rpart(
        formula = total_cost ~ ., 
        data = dt_analysis_train(), 
        method = "anova",
        control = rpart.control(minsplit = input$dt_minsplit_, maxdepth = input$dt_maxdepth_)
      )
    })
  
  ### Pruned Model
  pruneddtmodel <- eventReactive(
    input$dt_action_, {
      prune(tuneddtmodel(), cp = if(input$dt_bestcp_){dt_bestcp_val()}else{input$dt_cp_}
      )
    })
  
  ### Function to plot error
  dt_plotError = function(modeltype){
    output$dt_errorplot_ = renderPlot(plotcp(modeltype))
  }
  
  observeEvent(input$dt_init_action_, dt_plotError(initialdtmodel()))
  observeEvent(input$dt_action_, dt_plotError(pruneddtmodel()))
  
  ### Function to plot CP datatable
  dt_plotCPTable = function(modeltype){
    output$dt_cp_datatable_ = DT::renderDataTable(
      formatCurrency(
        DT::datatable(modeltype$cptable,
                      rownames = FALSE,
                      class = "compact",
                      options = list(
                        pageLength = 4, 
                        lengthMenu = c(4, 8),
                        autoWidth = TRUE,
                        info = FALSE
                      )),
        c(1,3,4,5), currency = '', 
        digits = 3) 
    )
  }
  
  observeEvent(input$dt_init_action_, dt_plotCPTable(initialdtmodel()))
  observeEvent(input$dt_action_, dt_plotCPTable(pruneddtmodel()))
  
  ### Function to plot tree
  dt_plotTree = function(modeltype){
    output$dt_tree_ = renderVisNetwork(
      visTree(modeltype, 
              edgesFontSize = 12, 
              nodesFontSize = 14,
              legendWidth = 0.3)
    )
  }
  
  observeEvent(input$dt_init_action_, dt_plotTree(initialdtmodel()))
  observeEvent(input$dt_action_, dt_plotTree(pruneddtmodel()))
  
  ##Initial Model Metrics
  pred_initialdtmodel <- eventReactive(
    input$dt_init_action_, {
      predict(initialdtmodel(), newdata = dt_analysis_test())
    })
  
  pred_initialdtmodel_rmse <- eventReactive(
    input$dt_init_action_, {
      caret::RMSE(pred_initialdtmodel(), dt_analysis_test()$total_cost)
    })
  
  pred_initialdtmodel_mae <- eventReactive(
    input$dt_init_action_, {
      caret::MAE(pred_initialdtmodel(), dt_analysis_test()$total_cost)
    })
  
  pred_initialdtmodel_r2 <- eventReactive(
    input$dt_init_action_, {
      caret::R2(pred_initialdtmodel(), dt_analysis_test()$total_cost, form = "traditional")
    })
  
  ##Pruned Model Metrics
  pred_pruneddtmodel <- eventReactive(
    input$dt_action_, {
      predict(pruneddtmodel(), newdata = dt_analysis_test())
    })
  
  pred_pruneddtmodel_rmse <- eventReactive(
    input$dt_action_, {
      caret::RMSE(pred_pruneddtmodel(), dt_analysis_test()$total_cost)
    })
  
  pred_pruneddtmodel_mae <- eventReactive(
    input$dt_action_, {
      caret::MAE(pred_pruneddtmodel(), dt_analysis_test()$total_cost)
    })
  
  pred_pruneddtmodel_r2 <- eventReactive(
    input$dt_action_, {
      caret::R2(pred_pruneddtmodel(), dt_analysis_test()$total_cost, form = "traditional")
    })
  
  ### Function to display RMSE
  dt_display_RMSE = function(modeltype){
    output$dt_rmse_ = renderValueBox(
      valueBox(
        value = tags$p(paste0(scales::comma(round(modeltype/1000,0)), "k"), style = "font-size: 50%;"), 
        subtitle = tags$p("RMSE", style = "font-size: 90%;"), 
        icon = icon("circle-info"),
        color = "aqua"
      )
    )
  }
  
  observeEvent(input$dt_init_action_, dt_display_RMSE(pred_initialdtmodel_rmse()))
  observeEvent(input$dt_action_, dt_display_RMSE(pred_pruneddtmodel_rmse()))
  
  ### Function to display MAE
  dt_display_MAE = function(modeltype){
    output$dt_mae_ = renderValueBox(
      valueBox(
        value = tags$p(paste0(scales::comma(round(modeltype/1000,0)), "k"), style = "font-size: 50%;"), 
        subtitle = tags$p("MAE", style = "font-size: 90%;"), 
        icon = icon("circle-info"),
        color = "aqua"
      )
    )
  }
  
  observeEvent(input$dt_init_action_, dt_display_MAE(pred_initialdtmodel_mae()))
  observeEvent(input$dt_action_, dt_display_MAE(pred_pruneddtmodel_mae()))
  
  ### Function to display R2
  dt_display_R2 = function(modeltype){
    output$dt_r2_ = renderValueBox(
      valueBox(
        value = tags$p(round(modeltype, 3), style = "font-size: 50%;"), 
        subtitle = tags$p("Rsquare", style = "font-size: 90%;"), 
        icon = icon("circle-info"),
        color = "aqua"
      )
    )
  }
  
  observeEvent(input$dt_init_action_, dt_display_R2(pred_initialdtmodel_r2()))
  observeEvent(input$dt_action_, dt_display_R2(pred_pruneddtmodel_r2()))
  
  
  ##Initial Model Predicted vs Actual
  dt_predvsactual_initial <- eventReactive(
    input$dt_init_action_, {
      data.frame(actual = dt_analysis_test()$total_cost,
                 pred = pred_initialdtmodel())
    })
  
  ##Pruned Model Predicted vs Actual
  dt_predvsactual_pruned <- eventReactive(
    input$dt_action_, {
      data.frame(actual = dt_analysis_test()$total_cost,
                 pred = pred_pruneddtmodel())
    })
  
  dt_plotpredvsactual = function(modeltype){
    output$dt_pred_actual_ = renderPlot(
      ggplot(data = modeltype, 
             aes(actual, pred)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
        scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
        theme_minimal() +
        labs(x = "Actual Value", y = "Predicted Value")
    )
  }
  
  observeEvent(input$dt_init_action_, dt_plotpredvsactual(dt_predvsactual_initial()))
  observeEvent(input$dt_action_, dt_plotpredvsactual(dt_predvsactual_pruned()))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
