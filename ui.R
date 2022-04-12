fluidPage(
  shinyjs::useShinyjs(),
  #setBackgroundImage(src = "airplane_grey.png", shinydashboard = TRUE),
  theme = bs_theme(bootswatch = "darkly"),
  tags$head(
    tags$style(HTML("/*
                    * Component: Info Box
                    * -------------------
                    */
                    
                    .info-box {
                    display: block;
                    min-height: 90px;
                    background: #fff;
                    width: 100%;
                    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
                    border-radius: 2px;
                    margin-bottom: 15px;
                    }
                    .info-box small {
                    font-size: 14px;
                    }
                    .info-box .progress {
                    background: rgba(0, 0, 0, 0.2);
                    margin: 5px -10px 5px -10px;
                    height: 2px;
                    }
                    .info-box .progress,
                    .info-box .progress .progress-bar {
                    border-radius: 0;
                    }
                    .info-box .progress .progress-bar {
                    background: #fff;
                    }
                    .info-box-icon {
                    border-top-left-radius: 2px;
                    border-top-right-radius: 0;
                    border-bottom-right-radius: 0;
                    border-bottom-left-radius: 2px;
                    display: block;
                    float: left;
                    height: 90px;
                    width: 90px;
                    text-align: center;
                    font-size: 45px;
                    line-height: 90px;
                    background: rgba(0, 0, 0, 0.2);
                    }
                    .info-box-icon > img {
                    max-width: 100%;
                    }
                    .info-box-content {
                    padding: 5px 10px;
                    margin-left: 90px;
                    }
                    .info-box-number {
                    display: block;
                    font-weight: bold;
                    font-size: 18px;
                    }
                    .progress-description,
                    .info-box-text {
                    display: block;
                    font-size: 18px;
                    white-space: nowrap;
                    overflow: hidden;
                    text-overflow: ellipsis;
                    }
                    .info-box-text {
                    text-transform: uppercase;
                    }
                    .info-box-more {
                    display: block;
                    }
                    .progress-description {
                    margin: 0;
                    }
                    
                    .bg-green,
                    .callout.callout-warning,
                    .alert-warning,
                    .label-warning,
                    .modal-warning .modal-body {
                      background-color: #375a7f !important;
                    }
                    
                    .bg-yellow,
                    .callout.callout-warning,
                    .alert-warning,
                    .label-warning,
                    .modal-warning .modal-body {
                      background-color: #577a9f !important;
                    }
                    
                    .bg-red,
                    .callout.callout-warning,
                    .alert-warning,
                    .label-warning,
                    .modal-warning .modal-body {
                      background-color: #779abf !important;
                    }
                    
                    hr {border-top: 1px solid #ffffff;}
  
                    "))),
  navbarPage(
      title = "Airline Fare Prediction with Random Forest",
      
      tabPanel(title = "Background",
               tabName = "background",
               icon = icon("calendar"),
               
               navlistPanel(
                 widths = c(3, 9),
                 tabPanel("Welcome!",
                          imageOutput(outputId = "welcome", height = "600px")),
                 tabPanel("Background",
                          icon = icon("box"),
                          htmlOutput(width = 8,
                                     outputId = "background_1_1",
                                     title = "Background and Problem Research"),#hr(),
                          imageOutput("bg1_image1",height = "240px"),
                          htmlOutput(width = 8,
                          outputId = "background_1_2"),#hr(),
                          imageOutput("bg1_image2", height = "350px"),
                          htmlOutput(width = 8,
                                     outputId = "background_1_3"),
                          hr(),
                          htmlOutput(width = 8,
                                     outputId = "background_1_4",
                                     title = "Business Impact"),
                          hr(),
                          htmlOutput(width = 8,
                                     outputId = "background_1_5",
                                     title = "Target User")),
                 tabPanel("About the Dataset",
                          icon = icon("table"),
                          htmlOutput(width = 8,
                                     outputId = "background_2_1"),
                          hr(),
                          dataTableOutput("background2_table"),
                          hr(),
                          htmlOutput(width = 8,
                                     outputId = "background_2_2"),
                          hr(),
                          htmlOutput(width = 8,
                                     outputId = "background_2_3"),
                          hr(),
                          dataTableOutput("background2_table2"),
                          hr(),
                          htmlOutput(width = 8,
                                     outputId = "background_2_4")
                          ),
                 tabPanel("Machine Learning Algorithm",
                          icon = icon("robot"),
                          htmlOutput(width = 8, outputId = "background_4_1"),
                          imageOutput(outputId = "background_4_img1", height = "240px"),
                          htmlOutput(width = 8, outputId = "background_4_2"),
                          imageOutput(outputId = "background_4_img2", height = "240px"),
                          htmlOutput(width = 8, outputId = "background_4_3"),
                          imageOutput(outputId = "background_4_img3", height = "240px"),
                          htmlOutput(width = 8, outputId = "background_4_4"),
                          plotlyOutput(outputId = "bg_plot01"),
                          htmlOutput(width = 8, outputId = "background_4_5"),
                          imageOutput(outputId = "background_4_img4", height = "240px"),
                          htmlOutput(width = 8, outputId = "background_4_6"),
                          imageOutput(outputId = "background_4_img5", height = "240px"),
                          htmlOutput(width = 8, outputId = "background_4_7"),
                          imageOutput(outputId = "background_4_img6", height = "240px"),
                          htmlOutput(width = 8, outputId = "background_4_8")
                          ),
               )
      ),
      
      tabPanel(title = "Fare Prediction",
               tabName = "menu_1",
               icon = icon("chart-line"),
               sidebarLayout(
                 fluid = FALSE,
                 sidebarPanel = sidebarPanel(
                   width = 3,
                   #menuItem(text = "Welcome", icon = icon("star")),
                   dateRangeInput(inputId = "arr_date",
                                  label = "Possible Flight Dates:",
                                  start = today(),
                                  end = today()),
                   fluidRow(column(width = 6,
                                   radioButtons(
                                     inputId = "arr_hour_yn",
                                     label = "Preferred Arrival Hour?",
                                     choiceNames = c("Yes", "No"),
                                     choiceValues = c("Y","N"),
                                     inline = FALSE,
                                     selected = "N")),
                            column(width = 6,
                                   numericInput(inputId = "arr_hour",
                                                label = "Arrival Hour (0-23):",
                                                value = 0,
                                                min = 0,
                                                max = 23))
                   ),
                   fluidRow(column(width = 6,
                                   selectInput(inputId = "dep_city",
                                               label = "Depart From:",
                                               choices = levels(flfare_clean3$departure_city))),
                            column(width = 6,
                                   selectInput(inputId = "arr_city",
                                               label = "Arrive To:",
                                               choices = levels(flfare_clean3$arrival_city)))
                   ),
                   fluidRow(column(width = 6,
                                   radioButtons(
                                     inputId = "airline_yn",
                                     label = "Preferred Airline?",
                                     choiceNames = c("Yes", "No"),
                                     choiceValues = c("Y","N"),
                                     inline = FALSE,
                                     selected = "N")),
                            column(width = 6,
                                   selectInput(inputId = "airline",
                                               label = "Airline Carrier:",
                                               choices = levels(flfare_clean3$airline)))
                   ),
                   actionButton(inputId = "submit",
                                label = "Start Predicting!",
                                width = "100%",
                                style = "color: white; background-color: #375a7f"),
                   hr(),
                   htmlOutput("mini_info")
                 ),
                 mainPanel = mainPanel(
                   width = 9,
                   box(width = 12,
                       title = "Airline Fare Prediction - Cheapest Date to Arrive At Your Destination",
                       htmlOutput("recommendation_title"),
                       infoBoxOutput(outputId = "recommendation1", width = 8),
                       # infoBoxOutput(outputId = "recommendation2", width = 8),
                       # infoBoxOutput(outputId = "recommendation3", width = 8),
                       # hr(),
                       # htmlOutput("plot_title"),
                       # plotlyOutput(outputId = "prediction_plot"),
                       # hr(),
                       # htmlOutput("table_title"),
                       # dataTableOutput("calculate_date_2")
                   )
                 )
               )
      ),
      
      tabPanel(title = "Route vs Price",
               tabName = "menu_2",
               icon = icon("route"),
               sidebarLayout(
                 fluid = FALSE,
                 sidebarPanel = sidebarPanel(
                   width = 3,
                   sliderInput(inputId = "plot1_price",
                               label = "Price Cutoff:",
                               min = min(flfare_clean4$price),
                               max = max(flfare_clean4$price),
                               value = c(min(flfare_clean4$price), max(flfare_clean4$price))),
                   HTML("Route(s) :"),
                   fluidRow(column(width = 4,
                                   style='padding-left:15px; padding-right:5px; padding-top:5px; padding-bottom:5px',
                                   checkboxGroupInput(inputId = "plot1_route",
                                                      label = "",
                                                      choices = levels(flfare_clean4$route) %>% head(30),
                                                      selected = levels(flfare_clean4$route) %>% head(30))),
                            column(width = 4,
                                   style='padding-left:5px; padding-right:5px; padding-top:5px; padding-bottom:5px',
                                   checkboxGroupInput(inputId = "plot1_route2",
                                                      label = "",
                                                      choices = levels(flfare_clean4$route) %>% tail(-30) %>% head(30),
                                                      selected = levels(flfare_clean4$route) %>% tail(-30) %>% head(30))),
                            column(width = 4,
                                   style='padding-left:5px; padding-right:15px; padding-top:5px; padding-bottom:5px',
                                   checkboxGroupInput(inputId = "plot1_route3",
                                                      label = "",
                                                      choices = levels(flfare_clean4$route) %>% tail(-60),
                                                      selected = levels(flfare_clean4$route) %>% tail(-60))))
                 ),
                 mainPanel = mainPanel(
                   width = 9,
                   box(width = 12,
                       title = "Route vs Price",
                       plotlyOutput(outputId = "plotly01"),
                       hr(),
                       htmlOutput(width = 8,
                                  outputId = "plotly01_desc")
                   ),
                 )
               )
      ),
      
      tabPanel(title = "Airline vs Price",
               tabName = "menu_3",
               icon = icon("plane"),
               sidebarLayout(
                 fluid = FALSE,
                 sidebarPanel = sidebarPanel(
                   width = 3,
                   sliderInput(inputId = "plot2_price",
                               label = "Price Cutoff:",
                               min = min(flfare_clean4$price),
                               max = max(flfare_clean4$price),
                               value = c(min(flfare_clean4$price), max(flfare_clean4$price))),
                   HTML("Airline Carrier(s) :"),
                   fluidRow(column(width = 6,
                                   style='padding-left:15px; padding-right:5px; padding-top:5px; padding-bottom:5px',
                                   checkboxGroupInput(inputId = "plot2_airline",
                                                      label = "",
                                                      choices = levels(flfare_clean4$airline) %>% head(8),
                                                      selected = levels(flfare_clean4$airline) %>% head(8))),
                            column(width = 6,
                                   style='padding-left:5px; padding-right:15px; padding-top:5px; padding-bottom:5px',
                                   checkboxGroupInput(inputId = "plot2_airline2",
                                                      label = "",
                                                      choices = levels(flfare_clean4$airline) %>% tail(-8),
                                                      selected = levels(flfare_clean4$airline) %>% tail(-8)))),
                   radioButtons(inputId = "plot2_metric",
                                label = "Method to Summarize :",
                                choices = c("Median", "Mean / Average", "Maximum", "Minimum"),
                                selected = "Median")
                 ),
                 mainPanel = mainPanel(
                   width = 9,
                   box(width = 12,
                       title = "Airline vs Price",
                       plotlyOutput(outputId = "plotly02"),
                       hr(),
                       htmlOutput(width = 8,
                                  outputId = "plotly02_desc")
                   ),
                 )
               )
      ),
      
      tabPanel(title = "Time Seasonality vs Price",
               tabName = "menu_4",
               icon = icon("clock"),
               sidebarLayout(
                 fluid = FALSE,
                 sidebarPanel = sidebarPanel(
                   width = 3,
                   sliderInput(inputId = "plot3_price",
                               label = "Price Cutoff:",
                               min = min(flfare_clean4$price),
                               max = max(flfare_clean4$price),
                               value = c(min(flfare_clean4$price), max(flfare_clean4$price))),
                   radioButtons(inputId = "plot3_season",
                                label = "Seasonality :",
                                choiceNames = c("Day of Week", "Day of Month", "Hour of Day"),#, "Month of Year"),
                                choiceValues = c("dow", "dom", "hour"),#, "mon"),
                                selected = "dow"),
                   radioButtons(inputId = "plot3_metric",
                                label = "Method to Summarize :",
                                choices = c("Median", "Mean / Average", "Maximum", "Minimum"),
                                selected = "Median")
                 ),
                 mainPanel = mainPanel(
                   width = 9,
                   box(width = 12,
                       title = "Time Seasonality vs Price",
                       plotlyOutput(outputId = "plotly03"),
                       hr(),
                       htmlOutput(width = 8,
                                  outputId = "plotly03_desc")
                   ),
                 )
               )
      ),
      
      tabPanel(title = "About Me",
               tabName = "aboutme",
               icon = icon("address-book"),
               htmlOutput(width = 8,
                          outputId = "aboutme"
                          )
               )
    )
)