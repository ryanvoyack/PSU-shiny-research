library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)
library(shinyjs)
library(V8)
library(discrimARTs)
library(leaflet)
library(raster)


shinyUI(dashboardPage(skin="blue",
              #Title
              dashboardHeader(title="Hypothesis Testing Game",titleWidth=450),
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(id = "tabs",
                            
                            menuItem("Overview", tabName = "rules", icon = icon("dashboard")),
                            #menuItem("Part 1", tabName = "first", icon = icon("table")),
                            menuItem("Concept Check Game", tabName = "qqq", icon = icon("gamepad"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$style(
                  type = "text/css",
                  ".content-wrapper,.right-side {
                  background-color: white;
                  }"
                ),
                tabItems(
                  tabItem(tabName = "rules",
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(12,
                                   h3("About:"),
                                   h4("In this app the goal is to quiz your knowledge of hypothesis testing."),
                                   # h3("Background:"),
                                   # h4(""),
                                   h3("Instructions:"),
                                   h4("1. Click the tic-tac-toe image to begin"),
                                   h4("2. To play the game, you will select the square that you want to place an X"),
                                   h4("3. Then you will answer the question that is given, if you get it right, an X will go in the square you selected - if not, an O will go in that spot"),
                                   h4("4. You can only submit an answer after choosing a spot on the image"),
                                   h4("5. You are playing as the X's, the object of the game is to get 3 X's in a row"),
                                   h4("6. (i.e., When you have 3 X's line up horizontally, vertically, or diagonally)"),
                                   h3("Aknowledgements:"),
                                   h4("This app was originally conceived by David Robinson in 2017. In 2018, using the original question bank, the game was redesigned and the entire app was then reprogrammed and developed by Ryan Voyack."),
                                   br(),
                                   h4("When you are ready to  start the game press Go.")
                                   
                            )
                          ),
                          fluidRow(
                            column(5,
                                   br())
                          ),
                          tags$head(
                            tags$style(HTML('#go{background-color: #367fa9}'))
                          ),
                          div(style = "text-align: center;",
                            actionButton(inputId = "go", label = tags$b("Go to Game"), icon = icon("bolt"), width='30%', style="color:white;")
                          )
                           
                  ),
                  tabItem(tabName = "qqq",
                          fluidRow(
                            div(style = "text-align: center;",
                                h4("If you answer correctly, You will receive an X in the square you chose, if not, it will be an O."),
                                h4("Try your best to win the game and get 3 X's in a row !"),
                                br()
                            )
                          ),
                          fluidRow(
                            # tags$style(
                            #   type = "text/css",
                            #   ".content-wrapper,.right-side {background-color: white;}"
                            # ),
                            
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
                                                                       uiOutput("CurrentQuestion"),
                                                                       uiOutput("CurrentQuestion.extra"),
                                                                       uiOutput("tits"),
                                                                       br(),
                                                                       br(),
                                                                       br()
                                                      ),
                                     textOutput("directions"),
                                     br()
                                     )
                              ),
                              column(2,
                                     bsButton(inputId = 'submit', label = 'Submit Question')
                              ),
                              column(1,
                                     actionButton(inputId = "nextButton",label = "Next Question")
                                     )
                            ),
                            fluidRow(
                              column(width=12, offset = 5,
                                     bsButton(inputId="reset", label="Start new game")
                              )
                            )
                          )
                  )
                )
))
)


