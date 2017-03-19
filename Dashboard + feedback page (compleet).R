setwd("C:/Users/Win7/Desktop/HVA TWK/JAAR 3/Minor/Project de waag")
rm(list = ls())
load("C:/Users/Win7/Desktop/HVA TWK/JAAR 3/Minor/Project de waag/.RData")
library(lubridate)
library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(dplyr)
library(leaflet)
library(timevis)
library(plotly)
library(htmltools)
library(stringr)

# TotalData <- read.csv("TotalData.csv")
# TotalData$Date <- ymd(TotalData$Date)
# TotalData$id <- as.factor(TotalData$id)


### DASHBOARD UI------
ui <- dashboardPage(
  dashboardHeader(title = "Gewaagd Dashboard", titleWidth = 250,
                  tags$li(a(href = 'https://www.waag.org/nl',
                            img(src = 'waag-logo2.jpg',
                                title = "Company Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Advanced", tabName = "Advanced", icon = icon("th")),
    menuItem("MathFact", tabName = "MathFACT", icon = icon("dashboard")),
    menuItem("Feedback", tabName = "Feedback", icon = icon("dashboard")),
    menuItem("Norm voor luchtkwaliteit:",HTML(paste("Bovengrens is vastgesteld op <br/>
             gemiddeld 40 μg/m3 per uur.<br/>
             Overschrijding van het <br/> uurgemiddelde van 200 μg/m3 is <br/>
             toegestaan op niet meer <br/> dan 18 keer per jaar. <br/>
             Volgens EU-norm"))))),
  dashboardBody( 
    #####BASIC PAGE----
    tabItems(
      tabItem(tabName="Overview",
              fluidPage(
                    div(class="outer",
                        tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                        leafletOutput("leafBA", width="100%", height="100%")),
                    fluidRow(splitLayout(
                      valueBoxOutput("WindRBoxBA",width="16.6%"),
                      valueBoxOutput("WindKBoxBA",width="16.6%"),
                      valueBoxOutput("TempBoxBA",width="16.6%"),
                      valueBoxOutput("RainBoxBA",width="16.6%"),
                      valueBoxOutput("NO2BoxBA",width="16.6%"),
                      valueBoxOutput("PMBoxBA",width="16.6%"))),
                  box(title = "Inputs", solidHeader = TRUE, width = 4, background = "black",
                      collapsible = TRUE,
                      "Pas hier de input van de kaart aan",
                    selectInput("stofBA", "Toon stof:", #mogelijkheid tot uitbreiden
                                choices = list("StikstofDioxide")),
                    dateInput("anaBA", "Kies datum:", value = as.Date("2016-06-16"), language = "nl",
                              min = as.Date("2016-06-01"),max = as.Date("2016-08-31")),
                    sliderInput("timeBA", "Tijdlijn",
                                min = 0, max = 23, 
                                value = 18, step=1)
                  )
              )
      ),
      ####ADVANCED PAGE------
      tabItem(
        tabName="Advanced",
              sidebarLayout(
                mainPanel(
                  verticalLayout(
                    tabBox(
                      title = "First tabBox", width = "100%",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tabset1",
                      tabPanel("Map", leafletOutput("leafAD")),
                      tabPanel("Plot", plotlyOutput("TR"))),
                    timevisOutput("timelineAD"))
                ),
                sidebarPanel(
                  verticalLayout(
                    checkboxGroupInput("sensID","Sensor:", 
                                       choices = c("Februariplein 14",
                                                   "Korte Koningsstraat 5",
                                                   "Kromme Waal 29-30 a",       
                                                   "Kromme Waal 29-30 b",
                                                   "Nieuwmarkt 113",
                                                   "Prins Hendrikkade 82",
                                                   "Sarphatistraat 62",
                                                   "Sint Antoniesbreestraat 35",
                                                   "Valkenburgerstraat 123",    
                                                   "Valkenburgerstraat 83 a")),
                    dateRangeInput("ana", "Kies periode:",
                                   start = as.Date("2016-06-20"), end= as.Date("2016-06-28")),
                    sliderInput("time", "Tijdlijn",
                                min = 0, max = 23, 
                                value = c(0,23), step=1),
                    tags$textarea(id="foo", rows=3, cols=40, 
                                  "Heeft u iets waargenomen? Plaats het in de tijdlijn!"),
                    actionButton("readCom",label="Plaats opmerking")
                )
              )
      )),
      #### MATHFACTS PAGE-----
      tabItem(
        tabName="MathFACT",
              fluidRow(
                box(title="Lineair model", 
                    plotOutput("LinPlot"),
                    status = "primary", width = 6),
                box(title="GAM model", 
                    plotOutput("GAMPlot"),
                    status = "primary", width = 6)
              ),
              fluidRow(
                box(title = "Input", width = 4, solidHeader = TRUE, status = "primary",
                    selectInput(inputId = "featureInput1",
                                label = "Selecteer een GGD-Sensor",
                                choices =  c("GGD-Vondelpark", "GGD-OudeSchans")),
                    selectInput(inputId = "sensorInput1",
                                label = "Selecteer een Waag-Sensor",
                                choices = c("Februariplein 14",
                                            "Korte Koningsstraat 5",
                                            "Kromme Waal 29-30 a",       
                                            "Kromme Waal 29-30 b",
                                            "Nieuwmarkt 113",
                                            "Prins Hendrikkade 82",
                                            "Sarphatistraat 62",
                                            "Sint Antoniesbreestraat 35",
                                            "Valkenburgerstraat 123",    
                                            "Valkenburgerstraat 83 a"))),
                tabBox(title="Model Validation", width = 8,
                       tabPanel("FIT",dataTableOutput('modelfit')),
                       tabPanel("RMSE",dataTableOutput('performance')),side="right")
                
              )),
      #### FEEDBACK PAGE--------
      tabItem(
        tabName="Feedback",bootstrapPage(
        # We'll add some custom CSS styling -- totally optional
        includeCSS("shinychat.css"),
        
        # And custom JavaScript -- just to send a message when a user hits "enter"
        # and automatically scroll the chat window for us. Totally optional.
        includeScript("sendOnEnter.js"),
        
        div(
          # Definieer de layout
          class = "container-fluid", 
          div(class = "row-fluid",
              # Titel
              tags$head(tags$title("ShinyChat")),
              
              # Creeer de header
              div(class="span6", style="padding: 10px 0px;",
                  h1("ShinyChat"), 
                  h4("Feedback is always welcome")
              ), div(class="span6", id="play-nice",
                     "IP Addresses are logged... be a decent human being."
              )
              
          ),
          # The main panel
          div(
            class = "row-fluid", 
            mainPanel(
              # Create a spot for a dynamic UI containing the chat contents.
              uiOutput("chat"),
              
              # Create the bottom bar to allow users to chat.
              fluidRow(
                div(class="span10",
                    textInput("entry", "")
                ),
                div(class="span2 center",
                    actionButton("send", "Send")
                )
              )
            ),
            # The right sidebar
            sidebarPanel(
              # Let the user define his/her own ID
              textInput("user", "Your User ID:", value=""),
              tags$hr(),
              h5("Connected Users"),
              # Create a spot for a dynamic UI containing the list of users.
              uiOutput("userList"),
              tags$hr(),
              helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.<p>Source code available <a href =\"https://github.com/trestletech/ShinyChat\">on GitHub</a>."))
            ))))))))

server <- function(input, output, session) {
  ###Chatomgevings variabelen----
  # Globally define a place where all users can share some reactive data.
  vars <- reactiveValues(chat=NULL, users=NULL)
  
  # Restore the chat log from the last session.
  if (file.exists("chat.Rds")){
    vars$chat <- readRDS("chat.Rds")
  } else {
    vars$chat <- "Welcome to Shiny Chat!"
  }
  
  #' Get the prefix for the line to be added to the chat window. Usually a newline
  #' character unless it's the first line.
  linePrefix <- function(){
    if (is.null(isolate(vars$chat))){
      return("")
    }
    return("<br />")
  }
    ####FUNCTIONS------
  selDat <- function(){
    return(
      TotalData %>% filter(Date==input$anaBA,Time==input$timeBA,!is.na(lat)))
  }
  selSTOFba <- reactive({switch(input$stofBA,
                                StikstofDioxide="lm.pred")})#mogelijkheid tot uitbreiden
  selSTOFad <- reactive({switch(input$stofBA,
                                StikstofDioxide="lm.pred")})#mogelijkheid tot uitbreiden
  selDatAD <- function(){
    return(
      TotalData %>% filter(Date>=input$ana[1],Date<=input$ana[2],
                           Time>=input$time[1],Time<=input$time[2],
                           !is.na(lat)))
  }
  selBreak <- function(){
    x <- as.numeric(difftime(min(selDatAD()$localTime,na.rm=T),
                             max(selDatAD()$localTime,na.rm=T),
                             units = "days"))
    if(x <= -7 & x > -30){return("1 day")}
    else if(x <= -30){return("1 week")}
    else {return("5 hours")}
  }
  selSELOmf <- reactive({switch(input$featureInput1,
                                `GGD-Vondelpark`="ggd",
                                `GGD-OudeSchans`="ggd_os")})
  selSESEmf <- reactive({switch(input$sensorInput1,
                                `Februariplein 14`="Februariplein 14",
                                `Korte Koningsstraat 5`="Korte Koningsstraat 5",
                                `Kromme Waal 29-30 a`="Kromme Waal 29-30 a",       
                                `Kromme Waal 29-30 b`="Kromme Waal 29-30 b",
                                `Nieuwmarkt 113`="Nieuwmarkt 113",
                                `Prins Hendrikkade 82`="Prins Hendrikkade 82",      
                                `Sarphatistraat 62`="Sarphatistraat 62",
                                `Sint Antoniesbreestraat 35`="Sint Antoniesbreestraat 35",
                                `Valkenburgerstraat 123`="Valkenburgerstraat 123",    
                                `Valkenburgerstraat 83 a`="Valkenburgerstraat 83 a")}) 
  
    #####COMMENTTIMEVIS FUNCTIONS----------
  loadData <- function() {
    comdata <- read.csv("commDat.csv",stringsAsFactors = FALSE,header=TRUE,sep=";")
    comdata$start <- as.Date(comdata$start)
    comdata$end <- as.Date(comdata$end)
    return(data.frame(comdata))
  }
  comText <- eventReactive(input$readCom, {
    input$foo
  })
  obs <- observe({    
    cat(comText(),";",as.character(paste(input$ana[1],input$time[1],sep=" ")),";",
        end = as.character(paste(input$ana[2],input$time[2],sep=" ")),
        '\n', file = "commDat.csv", append = TRUE)
  })
  #####BASIC PAGE-----------
  output$leafBA <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      fitBounds(4.866167, 52.35968, 4.908988, 52.37665)
  })
  observe({
    data1 <- selDat()
    leafletProxy("leafBA",data=data1) %>% 
      clearPopups() %>% clearShapes() %>%
      addCircles(radius=20, fill=TRUE, col=~no2col,
                 popup=~htmlEscape(paste("Adress:",Adress,"NO2:",
                                         round(lm.pred),
                                         " μg/m3")))
  })
  ###valueboxesBA----------
  output$WindRBoxBA <- renderValueBox({
    valueBox(
      paste0(selDat()$direct), "Windrichting",
      icon = icon("location-arrow", lib = "font-awesome"),
      color = "blue")
  })
  output$WindKBoxBA <- renderValueBox({
    valueBox(
      paste0(selDat()$Windsnelheid," m/s"), "Windkracht",
      icon = icon("fa", lib = "font-awesome"),
      color = "blue")
  })
  output$TempBoxBA <- renderValueBox({
    valueBox(
      paste0(selDat()$Temp/10," °C"), "Temperatuur",
      icon = icon("sun-o", lib="font-awesome"), color = 'blue')
  })
  output$RainBoxBA <- renderValueBox({
    valueBox(
      paste0(selDat()$Neerslag," mm"), "Regen",
      icon = icon("tint", lib = "glyphicon"),
      color = "blue")
  })
  output$NO2BoxBA <- renderValueBox({
      valueBox(
        paste0(round(selDat()$lm.pred)," μg/m3"), "Stikstof",
        icon = icon("cloud", lib = "font-awesome"),
        color = names(sort(table(selDat()$no2col),decreasing = T))[1])
  })
  output$PMBoxBA <- renderValueBox({
    valueBox(
      paste0(round(selDat()$ggd)," μg/m3"), "Vondel \n GGD",
      icon = icon("yelp", lib = "font-awesome"),
      color = names(sort(table(selDat()$ggdcol),decreasing = T))[1])
  })
  
  ##### ADVANCED PAGE--------
  #### timevissesAD------
  output$timelineAD <- renderTimevis({
    timevis(loadData())
  })
  observeEvent(input$readCom, {
    addItem("timelineAD", list(content = comText(), 
                               start = as.character(paste(input$ana[1],input$time[1],sep=" ")),
                               end = as.character(paste(input$ana[2],input$time[2],sep=" "))))
    centerItem("mytime", "item1")
  })
  ### plotlyAD met tabs----
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  
  output$leafAD <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      fitBounds(4.866167, 52.35968, 4.908988, 52.37665)
  })
  observe({
    leafletProxy("leafAD",data=selDatAD()[selDatAD()$Adress%in%c(input$sensID),]) %>% 
      clearShapes() %>% clearPopups() %>%
      addCircles(radius=10, fill=TRUE, col="Darkred",
                 popup=~htmlEscape(paste("Adress:",Adress)))
  })
  output$TR <- renderPlotly({
    TR <- ggplot(data=selDatAD()[selDatAD()$Adress%in%input$sensID,],
                 aes(x = localTime))+
      geom_line(aes(y = lm.pred, col=Adress))+
      geom_line(aes(y = ggd), col="Black", linetype = 2)+
      labs(list(title = "Sensor vergelijking",x="Tijdlijn",y="NO2-waarde",col="Locatie"))+
      scale_x_datetime(date_breaks=selBreak(),date_labels = "%Y-%m-%d %H:%M")+
      theme(axis.text.x = element_text(size=10,angle=45,color="Black"))
    ggplotly(TR) %>% layout(margin = list(b = 160))
  })
  ### MathFacts----  
  output$LinPlot <- renderPlot({
    OBJ <- TotalData %>% filter(Adress==selSESEmf())
    LinPlot <- ggplot(data=OBJ, aes(x=lm.pred, y=OBJ[,selSELOmf()]))+
      geom_point(alpha=0.2, color="black")+
      geom_smooth(aes(x=lm.pred, y=OBJ[,selSELOmf()]), color="black",method="lm")+
      geom_line(aes(x=lm.pred, y=lm.pred), color="blue", linetype=2)+
      ggtitle(paste(selSESEmf(),"naar",input$featureInput1))+
      labs(list(y = "GGD ground", x="Lineaire voorspelling"))
      LinPlot
  })
  output$GAMPlot <- renderPlot({
    OBJ2 <- TotalData %>% filter(Adress==selSESEmf())
    GAMPlot <- ggplot(data=OBJ2, aes(x=gam.pred, y=OBJ2[,selSELOmf()]))+
      geom_point(alpha=0.2, color="black")+
      geom_smooth(aes(x=gam.pred, y=OBJ2[,selSELOmf()]), color="black")+
      geom_line(aes(x=gam.pred, y=gam.pred), color="blue", linetype=2)+
      ggtitle(paste(selSESEmf(),"naar",input$featureInput1))+
      labs(list(y = "GGD ground", x="GAM voorspelling"))
    GAMPlot
  })
  output$modelfit = renderDataTable({
    Model.Fit
  })
  output$performance = renderDataTable({
    RMSE
  })
  
  
  ### FEEDBACK PAGE-----
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")

  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE

  # When a session is ended, remove the user and note that they left the room.
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
    })
  })

  # Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user

    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }

        # Updating username
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]

        # Note the change in the chat log
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-change",
                                                    paste0("\"", sessionVars$username, "\""),
                                                    " -> ",
                                                    paste0("\"", input$user, "\""))))

        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })

  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user",
                    value=sessionVars$username)
  })

  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })

  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat,
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })

  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")

    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
}
###RUN APP----
shinyApp(ui, server)

# windroos plot:
# output$NOXplot <- renderPlotly({
#   p <- plot_ly(plotly::wind, t = ~selDat()$Windrichting, r = ~(selDat()$Windsnelheid/10),
#                type = 'area',color=I("Darkred"))
#   layout(p, radialaxis = list(ticksuffix="m/s"),orientation = 270)
# })