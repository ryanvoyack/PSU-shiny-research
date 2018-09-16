#rm(list=ls()) 

library(RColorBrewer)
library(shiny)
library(shinyjs)
library(raster)
library(rgdal)
library(leaflet)
library(shiny)
library(DT)
library(shinyjs)
library(shinyBS)
library(V8)


############## CTRL F AND SEARCH "HARD CODE" TO FIND THINGS YOU MAY NEED TO MODIFY ###############


shinyServer(function(input, output, session) {
  
  #### question bank ####
  # your question bank should be formatted the same as mine, regarding what goes in which column
  bank <- read.csv('bank.csv', stringsAsFactors = FALSE)
  bank <- bank[,c(1,2,4:7,9,16)]
  bank[17:18,2] <- gsub('11',"\\\\",gsub("([\\\\])",'1',bank[17:18,2]))
  Qs <- nrow(bank)
  

  ######## MY SERVER CODE ##########
  
  #setwd("~/PSU (courses,etc) & Act-Sci (exams, etc)/shiny research/BOAST-EstimationTesting shiny")
  #the coordinates (sizing attributes) below are hard coded to work with the X and O images that I created/am currently using
  X.icon <- makeIcon(iconUrl = 'X.PNG', iconWidth = 95) #, iconHeight = 95)
  O.icon <- makeIcon(iconUrl = 'O.PNG', iconWidth = 105) #, iconHeight = 105)
  value <- matrix(rep(-3,9),3,3)
  values <- list(value) # 
  container <- c() # contains right or wrong answers after submit button is pressed 
  XsAndOs <- list()
  resolved <- c(rep(FALSE, Qs))
  #lists <- reactiveValues(container <- c(),values <- list(),value <- NULL)
  sr1.1=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,1.5,1.5,2.5))) # inner, middle horizontal lines
  sr1.2=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,3.5,3.5,2.5))) # inner, horizontal lines
  sr1.3=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(0.5,1.5,1.5,0.5))) # inner, horizontal lines
  sr2=Polygon(cbind(c(1.5,1.5,2.5,2.5), c(3.5,0.5,0.5,3.5)))
  sr3=Polygon(cbind(c(0.5,0.5,0.5,3.5), c(0.52,3.47,0.5,0.5))) #outer 
  sr4=Polygon(cbind(c(3.5,3.5,0.5,3.5), c(0.52,3.47,3.5,3.5))) #square
  srs1.1=Polygons(list(sr1.1), 's1.1')
  srs1.2=Polygons(list(sr1.2), 's1.2')
  srs1.3=Polygons(list(sr1.3), 's1.3')
  srs2=Polygons(list(sr2), 's2')
  srs3=Polygons(list(sr3), 's3')
  srs4=Polygons(list(sr4), 's4')
  #srs1.2,srs1.3
  spp = SpatialPolygons(list(srs1.1,srs2,srs3,srs4), 1:4)
  #spp = SpatialPolygons(list(srs3,srs4), 1:2)
  
  r <- raster(xmn = 0.5, xmx = 3.5, ymn = 0.5, ymx = 3.5, nrows = 3, ncols = 3)
  values(r) <- matrix(1:9, nrow(r), ncol(r), byrow = TRUE)
  crs(r) <- CRS("+init=epsg:4326")
  new.board <- leaflet(options = leafletOptions(zoomControl = FALSE, doubleClickZoom = FALSE, minZoom = 7, maxZoom = 7)) %>%  addPolygons(data=spp) %>% addRasterImage(r, colors="Set3")
  # %>% addTiles()
  line <- function(){
    #recall, in the value matrix, zero's represent O's, or wrong answers
    for(i in 1:3){
      total.1 <- 0 ; total.2 <- 0
      for(j in 1:3){
        total.1 <- total.1 + value[i, j]
        total.2 <- total.2 + value[j, i]
      }
      if(total.1==0 | total.2==0 | total.1==3 | total.2==3){
        break
      }
    }
    total.3 <- value[1, 1] + value[2, 2] + value[3, 3]
    total.4 <- value[1, 3] + value[2, 2] + value[3, 1]

    #if the game has been won:
    if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
      #place.na[!is.na(place.na)] <<- NA
      if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
        warning('LOSE')
        return("You Lose !")  #title(sub=list("You Are a Loser !", col="darkblue", font=2, cex=2.5), line=2)
      }else{
        warning('WIN')
        return("You Win ! Game Over !")  #title(sub=list("You Win ! Game Over !", col="red", font=2, cex=2.5), line=2)
      }
    }
    
    #if the previous is true (if the game is over) then this will fire through as well
    #this will also fire through if the board is full, regardless of whether the previous if() has executed
    #i dont know why these two statements should both be here
    if(length(which(value!=-3))==9){
      if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
        #if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
        #  title(sub=list("You Are a Loser !", col="darkblue", font=2, cex=2.5), line=2)
        #}else{
        #  title(sub=list("You Win ! Game Over !", col="orange", font=2, cex=2.5), line=2)
        #}
      }else{
        warning('RESTART')
        return("Draw ! Please try again !")  #title(sub=list("Draw ! Please try again !", col="blue", font=2, cex=2.5), line=2)
      }
    }
    return(NULL)
  }
  
  
  
  v <- reactiveValues(doPlot = FALSE)
  observeEvent(input$image_click, priority = 7, {
    validate(need(is.null(game()), label='Game is over'))
    
    #input$image_click will return coordinates when clicked, not a logical value
    if(!is.null(input$image_click)){
      v$doPlot <- TRUE
    }
  })
  
  #object that contains the coordinates returned from clicking the image
  coords <- eventReactive(input$image_click, {
    validate(need(is.null(game()), label='Game is over'))
    
    validate(need(!is.null(input$image_click), 'need to click image'))
    input$image_click
  })
  
  
  #### keep track of clicks on A: the plot, B: the submit button, C: the next button ####
  clicks <- reactiveValues(A=0,B=0,C=0)
  observeEvent(input$image_click, priority = 6, {
    validate(need(is.null(game()), label='Game is over'))
    mouse.at <- coords()
    output$warning <- renderText('')
    #we dont return an error for out of bounds clicks
    if(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5)){
      if(!is.null(input$image_click) & clicks$A==clicks$B){
        #we allow the user to switch his tic tac toe selection (before a corresponding answer) here
        clicks$A <- (clicks$A+1)
        warning(clicks$A, "A")
      }
    }else{
      if(clicks$A == clicks$B){
        output$warning <- renderText('Please click a valid square')
      }
    }
  })
  observeEvent(input$submit, priority = 6, {
    # (validate) make sure the player selected an answer when he pressed submit
    num <- numbers$question[length(numbers$question)]
    # DIRECTLY BELOW: YOU WILL SEE THIS EXACT SAME WARNING 5 TIMES TOTAL IN THIS SERVER. It refers to the following line directly below, which will be the EXACT SAME `ans <- ` line of code each time
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to be changed (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D", seq(0:100))), label='please select one of the multiple choice responses'))
    
    if(!is.null(input$submit)){clicks$B <- (clicks$B+1)}
    warning(clicks$B, 'b')
  })
  observeEvent(input$nextButton, priority = 6, {
    if(!is.null(input$nextButton)){clicks$C <- (clicks$C+1)}
    warning(clicks$C, 'c')
  })

  #called in the next observer below 
  ID <- reactive({
    validate(need(is.null(game()), label='Game is over'))
    
    validate(
      need(!is.null(input$image_click), 'click image') # because this will always come first
    )
    if(clicks$A>=clicks$B){
      if(clicks$A>clicks$B){
        "choice"
      }else if(clicks$A==clicks$B){
        #if the first few clicks are not valid squares, we must make sure theres no errors in the next observe handler below
        if(clicks$A==0){
          "choice"
        }else{
          "answer"
        }
      }
    }
  })

  game <- reactiveVal(NULL)
  #contains the board as it changes throughout the game
  #will store markers in XsAndOs list
  #as this event fires, only one marker will be added to the XsAndOs list each time
  #the first if statement will store an empty marker, when a box is chosen. Then,
  #then the second will overwrite it with an X or an O, when a question is answered
  observeEvent({input$submit
    input$image_click}, priority = 0,{
      validate(need(is.null(game()), label='Game is over'))
      
      mouse.at <- coords()
      ID <- ID()

      #if the user enters an invalid click after he already entered his choice on the board, it will not affect the outcome of an answer submission
      if(clicks$A > clicks$B | clicks$A == 0){
        validate(
          need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
        )
      }else if(clicks$A == clicks$B & ID == "answer" & XsAndOs[[ifelse(length(XsAndOs)==0,'',length(XsAndOs))]][3] %in% c(0,1) ){
        #or, if the user enters an invalid click before entering a valid click, it will not crash the app
        if(length(XsAndOs) == clicks$A){
          validate(
            need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
          )
        }
      }

      #statement that checks the ID return from either of the input triggering events. choice or answer
      if(ID=="choice"){
        mouse.at[[1]] <- round(mouse.at[[1]])
        mouse.at[[2]] <- round(mouse.at[[2]])
        x<<-mouse.at[[1]]
        y<<-mouse.at[[2]]

        #     should this be length(XsAndOs) ?  6/14/18
        XsAndOs[[(length(container)+1)]] <<- c(mouse.at[[2]],(mouse.at[[1]]-.3),NULL)
        leafletProxy("image", session) %>% addMarkers(lng=XsAndOs[[length(XsAndOs)]][1], lat=XsAndOs[[length(XsAndOs)]][2], layerId = "dummy")
                                                     
      }else if(ID=="answer"){
        value <<- values[[length(values)]]
        if(container[length(container)]==0){
          value[x, y] <<- 0
        }else if(container[length(container)]==1){
          value[x, y] <<- 1
        }
        values[[1]] <<- matrix(-3,3,3)
        values[[(length(values)+1)]] <<- value
        temp <<- which((values[[length(values)]] - values[[ (length(values)-1) ]])!=0)
        values[[length(values)]][temp] <<- container[length(container)]
        #plotting part, this will overwrite what previously passed through the first if statement in this observe handler
        if(temp<4){
          XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
        }else if(temp<7){ 
          XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
        }else if(temp<10){
          XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
        }else{
          stop("OH NO")
        }

        #the coordinates below are hard coded to work with the X and O images that I created/am currently using (the 2nd and 3rd leafletproxy calls below)
        #if you use the latest updated pictures that I used, then you should not need change any code
        leafletProxy("image", session)  %>% removeMarker(layerId = "dummy")
        warning(XsAndOs[[length(XsAndOs)]][3])
        if(!(XsAndOs[[length(XsAndOs)]][3] %in% c(0,1))){stop('VERY BAD')}
        if(XsAndOs[[length(XsAndOs)]][3]==1){
          leafletProxy("image", session)  %>% addMarkers(y-.0175,x+.61,icon = X.icon)
        }else{
          leafletProxy("image", session)  %>% addMarkers(y+.008,x+.54,icon = O.icon)
        }
        
        temp <- line()
        game(temp) #reactiveVal syntax
        message(temp)
      }
    }
  )  
  
  #### alerts user of status of game (if it is over) ####
  #return from line() function
  output$gameMessage <- renderText({
    validate(need(!is.null(game), label = 'Game is over'))

    game <- game()
    game
  })

    
  ####render image of tic tac toe board####
  output$image <- renderLeaflet({
    new.board
    #  out = out %>% addMarkers(lng=temp[[1]],lat=temp[[2]], icon=tryCatch(ifelse(temp[[3]]==1, X.icon, O.icon), error=function(e)NULL))
  })
  
  #### go button ####
  observeEvent(input$go, priority=1, {
    updateTabItems(session, "tabs", "qqq")
  })
  observe({
    validate(
      need(is.null(input$image_click), message='')
    )
    output$directions <- renderText({"Begin by selecting the square on the plot you would like"})
  })
  
  ####start over; new game####
  observeEvent(input$reset, priority = 5,{
    if(!is.null(game())){
      game(NULL)
    }
    leafletProxy("image", session) %>% clearMarkers()
    
    value <<- matrix(rep(-3,9),3,3)
    values <<- list(value)
    container <<- c()
    XsAndOs <<- list()
    #need to use scoping operator because i didnt create the (above) as reactive objects
    clicks$A <- 0
    clicks$B <- 0
    clicks$C <- 0

        
    #if there are less than 9 questions left, then we reset the question bank so that all questions can be drawn from again
    if(length(which(answers() %in% c("correct","incorrect")))>(Qs-9)){
      numbers$question <- c()
    }else{
      #put the incorrectly answered questions back into play
      if("incorrect" %in% unique(answers())){
        if("correct" %in% unique(answers())){
          #numbers$question <- numbers$question[which((answers()=="correct"))] #we assign the taken questions to be only the correctly answered ones
          numbers$question <- which(answers()=="correct") #we assign the taken questions to be only the correctly answered ones
        }else{
          numbers$question <- c() #since no answers were correct, all questions are in play
        }
      }
      #get rid of most recent question (but if it was answered correctly, it still will not be included)
      numbers$question <- numbers$question[-length(numbers$question)]
    }
    
    #resample to get new random question for when the game is restarted (when image is clicked)
    space<-c(1:Qs)
    numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(if(numbers$question){numbers$question}, error=function(e) (Qs+1))], 1)
    
    updateButton(session, 'nextButton', style = "color: white;
                 background-color: #1C2C5B;", disabled = TRUE)
    output$directions <- renderText({"Begin by selecting the square on the plot you would like"})
  })
  #temporary place holders, will be used as a logical pass to the conditional panel containing the renderUI output
  observeEvent(input$image_click, {
    validate(need(is.null(game()), label='Game is over'))
    
    output$temp <- renderText({'1'})
  })
  observeEvent(input$reset, priority = 8, {
    output$temp <- renderText({'2'})
  })

  
  #### enact game over mode ####
  observeEvent(input$submit, priority = -1, {
    validate(need(!is.null(game()), label='Game is over'))
    
    # (validate) makes sure the player selected an answer when he pressed submit
    num <- numbers$question[length(numbers$question)]
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to change (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D", seq(0:100))), label='please select one of the multiple choice responses'))
    
    updateButton(session, 'nextButton', style = "color: white;
                 background-color: #1C2C5B;", disabled = TRUE)
    updateButton(session, 'submit', style = "color: white;
                 background-color: #1C2C5B;", disabled = TRUE)
    output$directions <- renderText({"If you would like to play again, press 'Start new game'!"})
  })

  
  ######## QUESTIONS #########
  
  
  
  
  #### random question ####
  numbers <- reactiveValues(question = c())
  observeEvent(input$image_click, once=TRUE, priority = 9, {
    validate(need(is.null(game()), label='Game is over'))
    
    numbers$question[1] <- sample(1:Qs, 1)
    updateButton(session, 'nextButton', style = "color: white;
                 background-color: #1C2C5B;", disabled = TRUE)
    output$directions <- renderText({"Now answer the question and press submit"})
  })
  observeEvent(input$nextButton, priority = 4, {
      space <- c(1:Qs)
      numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(numbers$question, error=function(e) (Qs+1))], 1)
      updateButton(session, 'nextButton', style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
      output$directions <- renderText({"Now select another square on the board"})
      if(clicks$A == (clicks$C + 1)){
        updateButton(session, 'submit', style = "color: white;
                   background-color: #1C2C5B;", disabled = FALSE)
        output$directions <- renderText({"Now answer the question and press submit"})
      }
  })

  
  ####output random question####
  output$CurrentQuestion <- renderUI({
    num <- numbers$question[length(numbers$question)]
    temp <- NULL
    
    #this mathjax call is hard coded for specific questions in my CSV
    if(num %in% c(17,18)){
      #if you wont need math jax, get rid of this entire if call (but not the following else if's below)
      withMathJax(
            h4(sprintf(
              bank[num, 2]
            ))
          )
      #below, we make the radio button assignments to questions dynamic
    }else if(!(FALSE %in% unique(as.vector(bank[num,3:6]=='')))){
      # ^ if the question is not multiple choice/ requires a numeric input
      numericInput(inputId = (num), bank[num, 2], min=0,max=100, val=0)
    }else if(!(FALSE %in% unique(as.vector(bank[num,5:6]=='')))){
      # ^ if the question has 2 multiple choice responses
      radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4]), choiceValues = c("A", "B"),  selected = character(0))
    }else if(bank[num,6]==''){
      # ^ if the question has 3 multiple choice responses
      radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5]), choiceValues = c("A", "B", "C"), selected = character(0))
    }else{
      # ^ if the question has 4 multiple choice responses
      radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
    }
  })
  
  #hard coded observer for specific questions in the csv,
  #these questions were special in that they had pictures to go along with them, or they utilized latex to write mathematical functions
  output$CurrentQuestion.extra <- renderUI({
    num <- numbers$question[length(numbers$question)]
    setwd("~/")
    if(num == 3){
      img(src="CIQ3.png",height = 150,width = 500,align = "middle")
    }else if(num == 4){
      img(src="CIQ4.png",height = 150,width = 400,align = "middle")
    }else if(num == 17){
      radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5]), choiceValues = c("A", "B", "C"), selected = character(0))
    }else if(num == 18){
      radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
    }
  })  

  
  ####logical flow of answering questions; some extra code to deal with button disabling sequence####
  #these observers are for before the game is begun
  observeEvent(input$go, {
    updateButton(session, "submit", disabled = TRUE)
  })
  observeEvent(input$go, {
    updateButton(session, "nextButton", disabled = TRUE)
  })

  #these apply to if the game has already been started 
  observeEvent(input$submit, {
    # (validate) makes sure the player selected an answer when he pressed submit
    num <- numbers$question[length(numbers$question)]
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to change (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D", seq(0:100))), label='please select one of the multiple choice responses'))
    
    updateButton(session, 'submit', style = "color: white;
                 background-color: #1C2C5B;", disabled = TRUE)
    updateButton(session, 'nextButton', style = "color: white;
                 background-color: #1C2C5B;", disabled = FALSE)
    output$directions <- renderText({"Now press the next button"})
  })
  observeEvent(input$image_click, {
    validate(need(is.null(game()), label='Game is over'))
    
    if(!(clicks$A > (clicks$C + 1))){
      updateButton(session, 'submit', style = "color: white;
                   background-color: #1C2C5B;", disabled = FALSE)
      output$directions <- renderText({"Now answer the question and press submit"})
    }
  })

  
  ####checks answer####
  answers <- reactiveVal(c(rep('', Qs)))
  observeEvent(input$submit, {
    #(validate) makes sure the player selected an answer when he pressed submit
    num <- numbers$question[length(numbers$question)]
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to change (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D", seq(0:100))), label='please select one of the multiple choice responses'))
    
    temp <- answers()
    num <- numbers$question[length(numbers$question)]
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}
    if(ans == bank[num, ncol(bank)]){
      temp2 <- "correct"
    }else{
      temp2 <- "incorrect"
    }
    temp[num] <- temp2
    answers(temp)
    container[(length(container)+1)] <<- ifelse(temp[num] == "correct", 1, 0)
  })
  
  
  
  
  
  
})