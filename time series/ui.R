library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(DT)
library(V8)
library(discrimARTs)
library(leaflet)
library(raster)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Time Series Analysis"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Prerequisite", tabName = "pre", icon = icon("book")),
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Simulation", tabName = "sim", icon = icon("wpexplorer")),
        menuItem("Analyzing Real Data", tabName = "data", icon = icon("cogs")),
        menuItem("Concept Game", tabName = "game", icon = icon("gamepad"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
      ),
      tabItems(
        
        # Prerequisite
        tabItem(tabName = "pre",
                h3("Prerequisites for Time Series Analysis"),
                hr(),
                h4("Recall the following:"),
                column(11,
                       h3("1. Stationarity"),
                       h4("Diagnostics for stationarity include looking for constant mean (or, trend) and variance over time"),
                       column(11, offset=1,
                              h4("Constant mean is apparent when the data doesnt have any sort of vertical (typically linear) trend over time."),
                              h4("Seasonality could also be apparent in the mean structure. Recall that seasonal ARIMA cannot explain a seasonal trend, only seasonal correlations (ARIMA models work to explain correlation structure of a time series AFTER the mean and variance are constant)."),
                              h4("Constant variance is apparent when the data's vertical spread (in the valleys and peaks) is constant over the duration of the time series.")
                       ),
                       h3("2. Autocorrelation Functions of Stationary Time Series"),
                       h4("We typically trust the dashed lines in the autocorrelation function (ACF) plots to be the significance cut-off bounds for any lag's correlation"),
                       h4("In a model with non-zero autoregressive (AR) and moving average (MA) parts, there is no logical interpretation for both ACFS cutting off, thus,"),
                       column(11, offset=1,
                              h4("a. For AR(p) models, the ACF will tail off and the PACF will cut off after lag p."),
                              h4("b. For MA(q) models, the ACF will cut off after lag q, and the PACF will tail off."),
                              h4("c. For ARMA(p, q) models, both the ACF and the PACF will both tail off.")
                       ),
                       h4("The ARMA subsets plot is not the best tool for determining ARMA(p,q) orders, and thus will only be used as a tie breaker or guide after the ACF and PACF plots have been thoroughly inspected."),
                       h3("3. Model Diagnostics"),
                       h4("The ARIMA model aims to forecast future values of a stationary time series by estimating a mathematical function to explain the underlying correlation structure. For this reason, the ACF and PACF of the residuals of the ARIMA model that has been fitted should not contain any significant remaing correlation."),
                       h4("Though forecasting is the purpose for fitting an ARIMA model, looking at the forecast itself (against future values that have been reserved) isnt the best way to assess the goodness of the model's fit, this is why we look at the AIC and the ACF plots of the residuals of the model.")
                ),
                fluidRow(
                  br()
                ),
                br(),
                fluidRow(
                  div(style = "text-align: center",
                      bsButton("go0", "Go to overview", icon("bolt"), style = "danger", size = "large", class="circle grow")
                  )
                ),
                br()
        ),
        # About
        tabItem(tabName = "overview",
                fluidRow(
                  tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180))
                ),
                br(),
                hr(),
                h3(tags$b("About:")),
                h4("In this app the goal is to become more familiar with time series analysis"),
                h4("In the following tabs, you will explore time series with simulations, challange yourself with time series analysis, and review your knowledge of associated material with a game."),
                h4("Moreover, the first tab requires students to engage with simulations so they could find out how model equations may relate to the graphical representation of the time series. The second feature walks the user through time series analysis of a selected real world data set. The user will make the data stationary, fit a model, and observe the quality of their model in this analysis. The third feature will quiz the user about time series concepts as a part of a fun tic-tac-toe game."),
                hr(),
                h3(tags$b("Instructions:")),
                
                h3("Simulation Exploration:"),
                column(11,
                       h4("1. Use the sliders for the coefficients and explore how changing parameter values affects the time series plot"),
                       h4("2. Use the drop down menus and observe how different orders of models effect the autocorrelation function (ACF) and partial autocorrelation function (PACF) plots")
                ),
                fluidRow(
                  br()
                ),
                fluidRow(
                  div(style = "text-align: center",
                      bsButton("go1", "G O !", icon("bolt"), style = "danger", size = "large", class="circle grow")
                  )
                ),
                
                h3("Time Series Analysis with real data:"),
                column(11,
                       h4("1. In the first tab, select the data set that you would like to analyze, and fit transformations until the data seems stationary."),
                       h4("2. Here, you must consider that the last 12 observations of each data set are hidden from the user so that they could be presented in the last tab alongside the user's model's forecasts."),
                       h4("3. For each transformation consider the following:")
                ),
                column(9, offset=1,
                       h4("i. The transformations are of the form `seas_diff(diff(log(data)))`, that is, the log transformation will always be taken first, followed by the difference of lag one, and then the seasonal differncing."),
                       h4("ii. The trend can only be removed before any other transformation, or after all transformations. The trend is removed using regression, and the transformed data is the residuals from that regression.")
                ),
                fluidRow(
                  br()
                ),
                div(style = "text-align: center",
                    bsButton("go2", "G O !", icon("bolt"), style = "danger", size = "large", class="circle grow")
                ),
                
                fluidRow(
                  column(12,
                         h3("Time Series Concept Review Game:")
                  )
                ),
                column(11,
                       h4("1. Click the tic-tac-toe image to begin"),
                       h4("2. To play the game, you will select the square that you want to place an X"),
                       h4("3. Then you will answer the question that is given, if you get it right, an X will go in the square you selected - if not, an O will go in that spot"),
                       h4("4. You can only submit an answer after choosing a spot on the image"),
                       h4("5. You are playing as the X's, the object of the game is to get 3 X's in a row"),
                       h4("6. You win when 3 X's line up horizontally, vertically, or diagonally, and you lose when 3 O's line up horizontally, vertically or diagonally, otherwise the game results in a tie.")
                ),
                fluidRow(
                  br()
                ),
                
                div(style = "text-align: center",
                    bsButton("go3", "G O !", icon("bolt"), style = "danger", size = "large", class="circle grow")
                ),
                
                hr(),
                h3(tags$b("Acknowledgements:")),
                h4("This app was conceived in its entirety by Ryan Voyack and Yubaihe Grace Zhou in June and July of 2018. The simulation exploration part was coded by Grace and then revised by Ryan, the data analysis part was coded by Ryan, and the concept check game was built on top of code that Ryan wrote originally for a different app in this collection, Grace then revised the tic-tac-toe game's code in order to use questions of her choosing for this app. Grace used questions that were obtained from AnalyticsVidhya.com and written by Saurabh Jugalkishor Jaju. Special thanks to Saurabh from AnalyticsVidhya and University of California, Berkeley, for giving us permission to use his questions, special thanks to Professor Scott Roths, Penn State University, for help on using time series ARIMA functions, and special thanks to Angela Ting for help with applying the front-end design."),
                br()
                # h3(p("Explore Time Series with simulations")),
                # div(style = "text-align: center",
                #     bsButton("go1", "G O !", size = "large", style = "primary")),
                # hr(),
                # h3(p("Explore Time Series with data sets")),
                # div(style = "text-align: center",
                #     bsButton("go2", "G O !", size = "large", style = "primary")),
        ),
        
        tabItem(tabName = "sim",
                tags$style(type= "text/css", ".content-wrapper,.right-side {background-color: white;}"),
                
                fluidPage(
                  fluidRow(
                    column(width = 4,
                           selectInput("models","Models",
                                       list( "Autoregressive" = "AR",
                                             "Moving Average" = "MA",
                                             "Autoregressive Moving Average" = "ARMA"
                                       )
                           ),
                           sliderInput("n",
                                       label = "Sample Size",
                                       min = 10,
                                       max = 1000,
                                       step = 5,
                                       value = 20,
                                       ticks = T
                           ),
                           conditionalPanel(
                             #AR
                             condition = ("input.models=='AR' || input.models=='ARMA'"),
                             h4(p("AR(p)")),
                             
                             selectInput("p","p order",
                                         list( "1",
                                               "2"
                                         )
                             ),
                             conditionalPanel(
                               condition = ("input.p == '1' || input.p == '2'"),
                               #h5(p(withMathJax(textOutput("Phi1")))),
                               sliderInput("phi1",
                                           label = "Phi1",
                                           min = -0.9,
                                           max = 0.9,
                                           step = 0.1,
                                           value = 0.5,
                                           ticks = T
                               ),
                               conditionalPanel(
                                 condition = "input.p == '2'",
                                 
                                 sliderInput("phi2",
                                             label = "Phi2",
                                             min = -0.9,
                                             max = 0.9,
                                             step = 0.1,
                                             value = 0,
                                             ticks = T
                                 )
                               )
                             )
                             
                           ),
                           conditionalPanel(
                             condition = ("input.models=='ARMA'"),
                             hr()
                           ),
                           conditionalPanel(
                             #MA
                             condition = ("input.models=='MA' || input.models=='ARMA'"),
                             h4(p("MA(q)")),
                             
                             selectInput("q","q order",
                                         list( "1",
                                               "2"
                                         )
                             ),
                             conditionalPanel(
                               condition = ("input.q == '1' || input.q == '2'"),
                               #h5(p(withMathJax(textOutput("Phi1")))),
                               
                               sliderInput("theta1",
                                           label = "Theta1",
                                           min = -0.9,
                                           max = 0.9,
                                           step = 0.1,
                                           value = 0.5,
                                           ticks = T
                               ),
                               conditionalPanel(
                                 condition = ("input.q == '2'"),
                                 
                                 sliderInput("theta2",
                                             label = "Theta2",
                                             min = -0.9,
                                             max = 0.5,
                                             step = 0.1,
                                             value = 0.1,
                                             ticks = T
                                 )
                               )
                             )
                           )
                    ),
                    column(width = 8,
                           fluidRow(
                             column(12,
                                    plotOutput("plotSIM")
                             )
                           ),
                           fluidRow(
                             conditionalPanel(
                               condition="input.models=='ARMA'",
                               column(width = 6, plotOutput("plot.ACF")),
                               column(width = 6, plotOutput("plot.PACF"))
                             )
                           )
                    )
                  ),
                  
                  fluidRow(column(width = 12,
                                  fluidRow(
                                    conditionalPanel(
                                      condition="input.models!='ARMA'",
                                      fluidRow(
                                        column(6, plotOutput("plotACF")),
                                        column(6, plotOutput("plotPACF"))
                                      )
                                    )
                                  )
                  )
                  )
                )#fluidpage
        ),
        
        #### Datasets ####
        tabItem(tabName = "data",
                (tabsetPanel(id = "tabs2",
                             tabPanel(title = h4("Achieving Stationarity"), value = "step1",
                                      h4("To begin, please choose from of the data sets below any one that you would like"),
                                      h4("Use the checkbox options to make your data look satisfactorily stationary"),
                                      h4("The first part of our analysis is achieving stationarity, so that you can correctly judge the correlation structure to assign to the arima model"),
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("sets",tags$b("Choose a Dataset Below"),
                                                      list("Choose"="Choose",
                                                           "Internet Traffic"= "internet",
                                                           "Monthly Traffic Fatalities" = "monthly",
                                                           "varve" = "varve",
                                                           "sheep" = "sheep",
                                                           "Southern Oscillation Index" = "soi",
                                                           "Daily Max Temp State College" = "temperature"
                                                      )
                                          ),
                                          br(),
                                          conditionalPanel(
                                            condition = "input.sets!='Choose'",
                                            checkboxInput(inputId="trend", label="Remove trend/non-constant mean in data", value=FALSE),
                                            # bsPopover(id="trend", title = "tit", content = "The trend can only be removed before or after all transformations.", trigger = "hover", placement = "top"),
                                            conditionalPanel(
                                              condition = "input.trend",
                                              tags$div(style="margin-left: 3vw;",
                                                       checkboxInput(inputId="trend1", label="Estimate and Remove linear Trend in Data With Regression", value=FALSE),
                                                       checkboxInput(inputId="trend2", label="Estimate and Remove Seasonal Trend in Data With Regression", value=FALSE),
                                                       checkboxInput(inputId="trend3", label="Remove Seasonal Trend in Data With Cosine Regression", value=FALSE)
                                              ),
                                              conditionalPanel(
                                                condition="input.sets != 'temperature' && input.sets != 'monthly' && input.sets != 'soi' && (input.trend2 || input.trend3)",
                                                numericInput(inputId="frequency", label="Choose the frequency of the time series", value=1, min=1, max=365)
                                              )
                                            ),
                                            checkboxInput(inputId="log", label="Take log transformation of data", value=FALSE),
                                            checkboxInput(inputId="diff", label="Take first difference of data", value=FALSE),
                                            conditionalPanel(
                                              condition="input.diff",
                                              tags$div(style="margin-left: 3vw;",
                                                       checkboxInput(inputId="diff2", label="Take second difference of data", value=FALSE)
                                              )
                                            ),
                                            checkboxInput(inputId="seas_diff", label="Take first seasonal difference of data", value=FALSE),
                                            conditionalPanel(
                                              condition="input.seas_diff",
                                              tags$div(style="margin-left: 3vw;",
                                                       numericInput(inputId="seas", label="Choose the seasonal period", value=1, min=1, max=30),
                                                       checkboxInput(inputId="seas_diff2", label="Take second seasonal difference of data", value=FALSE)
                                              )
                                            ),
                                            div(style = "text-align: center",
                                                div(id = "div",
                                                    actionButton("go4", "Next step!", style = "primary", disabled = TRUE)
                                                )
                                            )
                                          )
                                        ),
                                        mainPanel(
                                          conditionalPanel(
                                            condition = "input.sets!='Choose'",
                                            plotOutput("original"),
                                            uiOutput("cite"),
                                            plotOutput("transform")
                                          )
                                        )
                                      )
                             ),
                             tabPanel(title = h4("Determine ARMA order"), value = "step2",
                                      h4("Now, that you have made your data stationary, you can inspect the resulting acf plots as well as the ARMAsubsets plot below to determine the arima order and fit a model"),
                                      fluidRow(
                                        plotOutput("ACF"),
                                        bsPopover(id="ACF", title = "ACF of transformed data", content = "The blue dashed line represents significance bounds for correlation at different lags in the data.", trigger = "hover", placement = "bottom"),
                                        plotOutput("PACF"),
                                        bsPopover(id="PACF", title = "PACF of tranformed data", content = "The blue dashed line represents significance bounds for correlation at different lags in the data.", trigger = "hover", placement = "top")
                                      ),
                                      div(style = "text-align: center",
                                          h4('After you are finished making your choices, press the "Next step!" button below to see how good of a fit your model was')
                                      ),
                                      fluidRow(
                                        column(4,
                                               numericInput("p.order", "AR part order", value=0, max=10, min=0),
                                               numericInput("q.order", "MA part order", value=0, max=10, min=0),
                                               numericInput("P.order", "Seasonal AR part order", value=0, max=10, min=0),
                                               numericInput("Q.order", "Seasonal MA part order", value=0, max=10, min=0),
                                               numericInput("period", "Seasonal Period", value=0, max=12, min=0)
                                        ),
                                        column(8,
                                               plotOutput("subsets"),
                                               bsPopover(id="subsets", title = "ARMA subsets", content = "This plot shows the best combinations of ARMA orders using AIC. The greyed squares indicate that the parameter is used in the model.", trigger = "hover", placement = "top")
                                        ),
                                        div(style = "text-align: center",
                                            bsButton("go5", "Next step!", style = "primary")
                                        )
                                      )
                             ),
                             tabPanel(title = h4("Forecast"), value = "step3",
                                      fluidRow(
                                        column(11,
                                               h4("Below you can see how well you fit the data to a time series by seeing the resulting forecasts (plotted against the last 12 observations of the data set, which were hidden from the initial time series plot), this plot will show you-in blue-only the final 100 observations in the data set. You can also observe the correlation structure of the residuals of the arima fit to see if you were able to fully explain the correlation with the model. The progress bar will show you how close you are to the best possible fit for the data set.")
                                        )
                                      ),
                                      fluidRow(
                                        div(style = "position:relative; z-index: 950;",
                                            plotlyOutput("forecast")
                                        ),
                                        br(),
                                        # fluidRow(
                                        #   div(style = "position:relative; z-index: 900;",
                                        #     div(style = "margin-top: -20vh;",
                                        #       div(style = "text-align: center",
                                        #         imageOutput("bar") #, height = "1000px")
                                        #       )
                                        #     )
                                        #   )
                                        # ),
                                        fluidRow(
                                          div(style = "position:relative; z-index: auto;",
                                              div(style = "margin-top: -23vh;",
                                                  column(5, plotOutput("bar")),
                                                  column(6, 
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(), #these are necessary
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         verbatimTextOutput("feedback", placeholder = TRUE)
                                                  )
                                              )
                                          ),
                                          div(style = "position:relative; z-index: auto;",
                                              bsPopover(id="bar", title = "Model fit evaluation", content = "This indicates how well your model was fit. 100 would indicate that your model is as good as can be. 0 would indicate that your model was no better than using just the mean as the predictor.", trigger = "hover", placement = "right")
                                          )
                                        ),
                                        # br(),
                                        fluidRow(
                                          div(style = "position:relative; z-index: 950;",
                                              div(style = "margin-top: -20vh;",
                                                  column(6, plotOutput("fitQuality1")),
                                                  bsPopover(id="fitQuality1", title = "ACF residuals", content = "The ACF and PACF of the residuals of the fitted model can indicate if theres any remaining correlation structure in the data.", trigger = "hover", placement = "top"),
                                                  column(6, plotOutput("fitQuality2")),
                                                  bsPopover(id="fitQuality2", title = "PACF residuals", content = "The ACF and PACF of the residuals of the fitted model can indicate if theres any remaining correlation structure in the data.", trigger = "hover", placement = "top")
                                              )
                                          )
                                        )
                                      )
                             )
                ))
                
                
                
        ),
        
        ##Concept Game tic-tac-toe
        tabItem(tabName = "game",
                fluidPage(
                  div(style="display: inline-block;vertical-align:top;",
                      circleButton("info",icon = icon("info",class = "glyphicon glyphicon-info-sign"), size = "xs")
                  ),
                  fluidRow(
                    div(style = "text-align: center;",
                        h4("If you answer correctly, You will receive an X in the square you chose, if not, it will be an O."),
                        h4("Try your best to win the game and get 3 X's in a row !"),
                        br()
                    )
                  ),
                  fluidRow(
                    column(4,
                           leafletOutput('image'),
                           br(),
                           textOutput("warning"),
                           textOutput("gameMessage")
                    ),
                    column(8,
                           
                           conditionalPanel("output.temp != 2",
                                            conditionalPanel("input.image_click",
                                                             uiOutput("Question"),
                                                             uiOutput("CurrentQuestion"),
                                                             uiOutput("CurrentQuestion.extra"),
                                                             br(),
                                                             br(),
                                                             br()
                                            ),
                                            textOutput("directions"),
                                            br()
                           ),
                           column(8,
                                  fluidRow(
                                    column(6, div(style="text-align: center", bsButton(inputId = 'submit', label = 'Submit Question', style="primary", disabled=TRUE))),
                                    column(6, div(style="text-align: center", bsButton(inputId = "nextButton",label = "Next Question", style="primary", disabled=TRUE)))
                                  ),
                                  fluidRow(
                                    column(12, div(style="text-align: center", bsButton(inputId="reset", label="Start new game", style="primary")))
                                  )
                                  
                           )
                    )
                  ),
                  fluidRow(
                    # column(width=12, offset = 6,
                    #        bsButton(inputId="reset", label="Start new game", style="primary")
                    # ),
                    column(12,uiOutput("Feedback"))
                  )
                )
                
        )
      )
    )
  )
  
))