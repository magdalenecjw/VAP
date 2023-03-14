
#==============================#
###### Importing Packages ######
#==============================#

pacman::p_load("tmap", "ExPanDaR", "kableExtra", "ggstatsplot", "plotly", "DT", "scales", "shiny", "shinydashboard", "fresh", "shinyjs", "tidyverse")

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

# Aggregation and sorting by total_cost ----------------------------------------------------
touristdata_clean_country_sorted <- touristdata_clean_country %>%
  arrange(desc(total_cost))

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
    menuItem("Information", tabName = "information", icon = icon("info")),
    menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
    menuItem("Data Analysis", tabName = "tab_analysis", icon = icon("chart-simple"), startExpanded = TRUE,
             menuSubItem("Analysis by Region/Country", tabName = "tab_country",
             ),
             menuSubItem("Analysis by Others", tabName = "tab_others")
    ),
    menuItem("Clustering", tabName = "tab_cluster")
  )
)

# Dashboard Body ----------------------------------------------------
body <- dashboardBody(
  
  # CSS style  ----------------------------------------------------
  tags$style("h2 { font-family: sans-serif; font-weight: bold; }"),
  tags$style("h3 { font-family: sans-serif; font-weight: bold; }"),
  tags$style(".small-box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".box-header h3.box-title{ color: #2A2D34; font-weight: bold }"),
  tags$style(".box { font-size: 90%}"),
  tags$style(".box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".fa-dollar-sign {font-size:80%}"),
  tags$style(".fa-people-group {font-size:80%}"),
  tags$style(".fa-bed {font-size:80%}"),
  
  # Setting theme  ----------------------------------------------------
  use_theme(mytheme),
  
  useShinyjs(),
  
  # Dashboard Body Tabs  ----------------------------------------------------
  
  tabItems(
    
    ## Information  ----------------------------------------------------
    tabItem(tabName = "information",
            h2("About the app")
    ),
    
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
                       valueBoxOutput("dash_avgspentnight_", width = 4)
                     ),
                     #### Dashboard Second Value Boxes  ----------------------------------------------------
                     div(style = "padding = 0em; margin-top: 0em",
                         fluidRow(
                           valueBoxOutput("dash_totalvisitors_", width = 4),
                           valueBoxOutput("dash_avgnight_", width = 4),
                           valueBoxOutput("dash_avgpartysize_", width = 4)
                         )),
                     
                     #### Dashboard Interactive Map  ----------------------------------------------------
                     fluidRow(
                       box(
                         title = tags$p("Control Panel", style = "color: #FFF; font-weight: bold;"),
                         status = "primary",
                         background = "aqua",
                         solidHeader = TRUE,
                         collapsible = FALSE,
                         width = 3,
                         div(style = "padding = 0em; margin-top: -0.5em",
                             selectInput(inputId = "dash_mapmetric_",
                                         label = "Select Metrics:",
                                         choices = c("Total Visitors" = "total_tourist",
                                                     "Total Spending" = "total_cost",
                                                     "Average Spending per Trip" = "avg_cost",
                                                     "Average Night Spent" = "avg_night_spent",
                                                     "Average Spending per Night" = "cost_per_night",
                                                     "Average Individual Spending per Night" = "cost_per_pax_night"),
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
                                          label = "Min Total Visitors:",
                                          min = 0,
                                          max = 100,
                                          value = 20))
                       ),
                       
                       column(width = 9,
                              div(style = "padding = 0em; margin-left: -1.5em",
                                  tmapOutput("dash_map_", 
                                             width = "100%",
                                             height = 450)
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
                     
                     fluidRow()
                     
              )
            )
    ),
    
    ## Analysis by Country  ----------------------------------------------------
    tabItem(tabName = "tab_country",
            #h2("Data Analysis by Country"),
            fluidRow(
              ### Analysis_Country First Column  ----------------------------------------------------
              column(width = 2,
                     div(style = "padding = 0em; margin-right: -0.5em",
                         box(
                           title = tags$p("First Panel", style = "color: #FFF; font-weight: bold;"),
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
                           disabled(div(style = "padding = 0em; margin-top: -1em",
                                        selectInput(inputId = "acou_cou_",
                                                    label = "Select Country (max 5):",
                                                    choices = list("Top (World)" = "top_world",
                                                                   "Top (Europe)" = "top_europe",
                                                                   "Top (Americas)" = "top_americas",
                                                                   "Top (Africa)" = "top_africa",
                                                                   "Top (Asia)" = "top_asia",
                                                                   "Top (Oceania)" = "top_oceania"),
                                                    selected = "top_world"))),
                           div(style = "padding = 0em; margin-top: -1em",
                               selectInput(inputId = "acou_test_",
                                           label = "Test Type:",
                                           choices = list("parametric" = "p", 
                                                          "non-parametric" = "np", 
                                                          "robust" = "r", 
                                                          "Bayes Factor" = "bf"),
                                           selected = "np")),
                           div(style = "padding = 0em; margin-top: -1em",
                               radioButtons(inputId = "acou_cf_",
                                            label = "Confidence Level:",
                                            choices = c("95%" = 0.95,
                                                        "99%" = 0.99),
                                            selected = 0.95)),
                           div(style = "padding = 0em; margin-top: 0em",
                               tags$p("Refer to second panel to continue plotting", style = "font-style: italic;")),
                         ))
              ),
              
              
              ### Analysis_Country Second Column  ----------------------------------------------------
              column(width = 10,
                     tabBox(
                       title = h3("Hypothesis Testing"),
                       width = 12,
                       
                       #### Analysis_Country Numerical ----------------------------------------------------
                       tabPanel(
                         title = tags$p("Numerical Variables", style = "font-weight: bold;"),
                         fluidRow(
                           
                           #### Analysis_Country Numerical Control Panel ----------------------------------------------------
                           column(width = 3,
                                  box(
                                    title = tags$p("Second Panel", style = "color: #FFF; font-weight: bold;"),
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
                                                    label = "Plot Type:",
                                                    choices = list("Box" = "box", 
                                                                   "Violin" = "violin", 
                                                                   "Box Violin" = "boxviolin"),
                                                    selected = "boxviolin")),
                                    div(style = "padding = 0em; margin-top: 0em",
                                        checkboxInput(inputId = "acou_compare_", 
                                                      label = "Show Pairwise Comparison",
                                                      value = TRUE)),
                                    div(style = "padding = 0em; margin-top: 0em",
                                        radioButtons(inputId = "acou_w_compare_", 
                                                     label = "Display Comparions:", 
                                                     choices = c("significant" = "s",
                                                                 "non-significant" = "ns"),
                                                     selected = "ns")),
                                    div(style = "padding = 0em; margin-top: 0em",
                                        checkboxInput(inputId = "acou_outliers_", 
                                                      label = "Treat Outliers",
                                                      value = TRUE)),
                                    div(style = "padding = 0em; margin-top: 0em",
                                        tags$p("Press button below to show graph", style = "font-style: italic;")),
                                    div(style = "padding = 0em; margin-top: -0.5em",
                                        actionButton(inputId = "acou_action_", 
                                                     label = "Update Plot"))
                                    
                                  )
                                  
                                  
                           ),
                           
                           #### Analysis_Country Numerical Plot ----------------------------------------------------
                           column(width = 9,
                                  plotOutput("acou_num_plot_")),
                           
                         )
                       ),
                       
                       tabPanel(
                         title = tags$p("Categorical Variables", style = "font-weight: bold;"),
                       )
                       
                       
                     )
                     
              )
              
              
              
            )
    ),
    
    ## Analysis by Others  ----------------------------------------------------
    tabItem(tabName = "tab_others",
            h2("Data Analysis by Others")
    ),
    
    ## Clustering  ----------------------------------------------------
    tabItem(tabName = "tab_cluster",
            h2("Clustering Analysis")
    )
    
  )
)

# User Interface  ----------------------------------------------------
ui <- dashboardPage(header, sidebar, body)


#========================#
###### Shiny Server ######
#========================#
server <- function(input, output) {
  
  # Global Data Manipulation  ----------------------------------------------------
  top_country <- reactive({
    touristdata_clean_country_sorted$country[1]
  })
  
  top_value <- reactive({
    scales::comma(touristdata_clean_country_sorted$total_cost[1]/1000000)
  })
  
  # Dashboard Data Manipulation  ----------------------------------------------------
  dash_touristdatatable <- reactive({
    touristdata_clean_country_sorted %>%
      filter(total_tourist >= input$dash_minvisitors_) %>%
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
  
  dash_map_metrics_text <- reactive({
    switch(input$dash_mapmetric_,
           "total_tourist" = "Total Visitors",
           "total_cost" = "Total Spending (TZS)",
           "avg_cost" = "Average Spending per Trip (TZS)",
           "avg_night_spent" = "Average Night Spent",
           "cost_per_night" = "Average Spending per Night (TZS)",
           "cost_per_pax_night" = "Average Individual Spending per Night (TZS)")
  })
  
  # Dashboard Server  ----------------------------------------------------
  output$dash_topspender_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0("TSZ ",top_value(), "m"), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("Top Spending Country: ",top_country()), style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$dash_avgspenttrip_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0("TSZ ",scales::comma(round(mean(touristdata_clean$total_cost)/1000,0)), "k"), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Spending per Trip", style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$dash_avgspentnight_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0("TSZ ",scales::comma(round(mean(touristdata_clean$cost_per_night)/1000,0)), "k"), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Spending per Night", style = "font-size: 80%;"), 
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$dash_totalvisitors_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(sum(touristdata_clean$total_tourist),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Total Visitors in dataset", style = "font-size: 80%;"), 
      icon = icon("people-group"),
      color = "aqua"
    )
  })
  
  output$dash_avgnight_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$total_night_spent),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Night Spent by Tourist", style = "font-size: 80%;"), 
      icon = icon("bed"),
      color = "aqua"
    )
  })
  
  output$dash_avgpartysize_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(scales::comma(round(mean(touristdata_clean$total_tourist),0))), style = "font-size: 60%;"), 
      subtitle = tags$p("Average Party Size", style = "font-size: 80%;"), 
      icon = icon("people-group"),
      color = "aqua"
    )
  })
  
  output$dash_map_ <- renderTmap({
    tmap_mode("view")
    tmap_options(check.and.fix = TRUE) +
      tm_shape(touristdata_clean_map %>%
                 filter(total_tourist >= input$dash_minvisitors_))+
      tm_fill(input$dash_mapmetric_, 
              n = input$dash_mapclasses_,
              style = input$dash_mapclassification_, 
              palette="Blues", 
              id = "country",
              title = dash_map_metrics_text()
      ) +
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
                  autoWidth = TRUE,
                  scrollX = TRUE)),
      c(3,4,6,7), currency = 'TZS ', 
      interval = 3, 
      mark = ',', digits = 0
    )
  })
  
  
  # Analysis_Country Data Manipulation  ----------------------------------------------------
  
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
  
  
  acou_ANOVA <- reactive({
    touristdata_clean %>%
      filter(country %in% countrylist()) %>%
      mutate(region = fct_reorder(region, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
      mutate(country = fct_reorder(country, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
      drop_na()
  })
  
  acou_ANOVA_nooutlier <- reactive({
    touristdata_clean %>%
      filter(country %in% countrylist()) %>%
      mutate(region = fct_reorder(region, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
      mutate(country = fct_reorder(country, !!sym(input$acou_numvar_), median, .desc = TRUE)) %>%
      drop_na() %>%
      treat_outliers() 
  })
  
  acou_ANOVA_metrics_text <- reactive({
    switch(input$acou_numvar_,
           "total_cost" = "Spending per Trip (TZS)",
           "cost_per_pax" = "Individual Spending per Trip (TZS)",
           "cost_per_night" = "Spending per Night (TZS)",
           "cost_per_pax_night" = "Individual Spending per Night (TZS)",
           "total_night_spent" = "Night Spent per Trip",
           "prop_night_spent_mainland" = "Proportion of Night Spent in Mainland")
  })
  
  # Analysis_Country Server  ----------------------------------------------------
  
  observe({
    toggleState(id = "acou_cou_", condition = input$acou_reg_cou_ == "country")
  })
  
  acou_num_plotreact <- eventReactive(
    input$acou_action_, {
      ggbetweenstats(data = if(input$acou_outliers_){acou_ANOVA_nooutlier()}else{acou_ANOVA()},
                     x = !!sym(input$acou_reg_cou_), y = !!sym(input$acou_numvar_),
                     plot.type = input$acou_plottype_,
                     xlab = str_to_title(input$acou_reg_cou_), ylab = acou_ANOVA_metrics_text(),
                     type = input$acou_test_, pairwise.comparisons = input$acou_compare_, pairwise.display = input$acou_w_compare_, 
                     mean.ci = T, p.adjust.method = "fdr",  conf.level = input$acou_cf_,
                     package = "ggthemes", palette = "Tableau_10")
    })
  
  output$acou_num_plot_ <- renderPlot({
    acou_num_plotreact()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
