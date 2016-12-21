setwd("C:/Users/Win7/Desktop/HVA TWK/JAAR 3/Minor/Project de waag")
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
# STN      LON(east)   LAT(north)     ALT(m)  NAME
# 240:         4.774       52.301      -4.40  SCHIPHOL
# 
# YYYYMMDD = datum (YYYY=jaar,MM=maand,DD=dag); 
# HH       = tijd (HH=uur, UT.12 UT=13 MET, 14 MEZT. Uurvak 05 loopt van 04.00 UT tot 5.00 UT; 
# DD      = Windrichting(in graden)gemiddeld over de laatste 10 minuten van het afgelopen uur 
#           (360=noord, 90=oost, 180=zuid, 270=west, 0=windstil 990=veranderlijk.
# FH       = Uurgemiddelde windsnelheid (in 0.1 m/s). 
# T        = Temperatuur (in 0.1 graden Celsius) op 1.50 m hoogte tijdens de waarneming; 
# DR       = Duur van de neerslag (in 0.1 uur) per uurvak; 
# RH       = Uursom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm); 

# STN,YYYYMMDD,   HH,   DD,   FH,    T,   DR,   RH,    R,    S
#
rm(list = ls())
# sf2016 <- readOGR("./Kaarten/2016/shape 2016 versie 10/Uitvoer_shape", "gem_2016")
# sf2016 <- spTransform(sf2016,CRS("+proj=longlat +datum=WGS84"))
weerDat <- read.csv("weerDat.csv")
weerDat$YYYYMMDD <- as.Date(as.character(weerDat$YYYYMMDD), format="%Y%m%d")
colnames(weerDat) <- c("STN","Date","Time","Windrichting","Windsnelheid","Temp",
                       "NeerslagDuur","Neerslag","Regen","Sneeuw")
weerDat$direct <- factor(sapply(weerDat$Windrichting, function(x){
  if(is.na(x)){return(NA)}
  if(x<=45|x>=315 && x!=0 && x!=990){return("Noord")}
  else if(x<=135&&x>45){return("Oost")}
  else if(x<=225&&x>135){return("Zuid")}
  else if(x<=315&&x>225){return("West")}
  else if(x==0){return("Stil")}
  else if(x==990){return("Wissel")}
}),levels=c("Noord","Oost","Zuid","West","Stil","Wissel"))
weerDat$Neerslag[weerDat$Neerslag==-1]<-0
write.csv(weerDat, "weerdata.csv", row.names=FALSE)
DATA <- read.csv("sensormeasures.csv")
DATA <- DATA[DATA$message=="",]
# DATA$srv_ts <- as.POSIXct(DATA$srv_ts, format="%Y-%m-%d %H:%M")
DATA <- separate(DATA, col= "srv_ts",into=c("Date","Time"),sep=" ",remove=FALSE)
DATA$Time <- format(trunc(strptime(DATA$Time, format="%H:%M:%S"), units="hours"),
                    format="%H:%M")
# DATA$Time <- as.numeric(DATA$Time)

DATA <- DATA %>% 
  group_by(id, Date, Time) %>%
  summarise(temp=mean(temp, na.rm=TRUE), count=n(),rssi=mean(rssi, na.rm=TRUE),pm10=mean(pm10, na.rm=TRUE),
            pm25=mean(pm25, na.rm=TRUE),no2a=mean(no2a, na.rm=TRUE),no2b=mean(no2b, na.rm=TRUE),
            humidity=mean(humidity, na.rm=TRUE))
DATA <- DATA %>% filter(count>=36)
SensLoc <- read.csv2("Senslonglat.csv",header=FALSE,sep=";",stringsAsFactors = FALSE)
colnames(SensLoc) <- c("id","lat","long","Adress")
DATA <- merge(DATA,SensLoc,by="id")
SensLoc$id <- as.factor(SensLoc$id)



FINALdata <- read.csv("./Github DATA/urbanairq_no2_final.csv")
FINALdata <- separate(FINALdata, col= "localTime",into=c("Date","Time"),sep=" ",remove=FALSE)
FINALdata$Time <- format(trunc(strptime(FINALdata$Time, format="%H:%M:%S"), units="hours"),
                    format="%H")
FINALdata$Date <- ymd(FINALdata$Date)
FINALdata$Time <- as.numeric(FINALdata$Time)

write.csv(DATA,"DATAtot.csv",row.names=FALSE)
DATA <- read.csv("DATAtot.csv")
weerDat <- read.csv("weerdata.csv")
DATA$Date <- ymd(DATA$Date)
weerDat$Date <- ymd(weerDat$Date)
DATA$id <- as.factor(DATA$id)
TOTALdata <- merge(DATA,FINALdata[,c(2,3,4,5,6,7)],by=c("Date","Time"),all.x=T)
write.csv(TOTALdata,"TOTALdata.csv",row.names=F)
FINALdata <- read.csv("TOTALdata.csv")
# for(i in 1:length(levels(DATA$id))){
#   assign(paste("df",i,sep=""),DATA[DATA$id==levels(DATA$id)[i],])
# }

ui <- dashboardPage(
  ### DASHBOARD UI------
  dashboardHeader(title = "Gewaagd Dashboard", titleWidth = 250),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Advanced", tabName = "Advanced", icon = icon("th")),
    menuItem("MathFact", tabName = "MathFACT", icon = icon("dashboard")),
    menuItem("Feedback", tabName = "Feedback", icon = icon("dashboard")))),
  dashboardBody( 
    #####BASIC PAGE------
    tabItems(
      tabItem(tabName="Overview",
        fluidRow(splitLayout(
          valueBoxOutput("WindRBoxBA",width="16.6%"),
          valueBoxOutput("WindKBoxBA",width="16.6%"),
          valueBoxOutput("TempBoxBA",width="16.6%"),
          valueBoxOutput("RainBoxBA",width="16.6%"),
          valueBoxOutput("NO2BoxBA",width="16.6%"),
          valueBoxOutput("PMBoxBA",width="16.6%"))),
        sidebarLayout(
          mainPanel(
            verticalLayout(
              leafletOutput("leafBA"))
          ),
       sidebarPanel(
         verticalLayout(
          selectInput("stofBA", "Toon stof:", 
                      choices = list("StikstofDioxide","Fijnstof 10","Fijnstof 2,5")),
          dateInput("anaBA", "Kies datum:", value = as.Date("2016-06-02"), language = "nl",
                             min = as.Date("2016-06-01"),max = as.Date("2016-08-31")),
          sliderInput("timeBA", "Tijdlijn",
                          min = 1, max = 24, 
                          value = 12, step=1,
                          animate = animationOptions(loop = TRUE, interval = 10))
       )
      )
    )
),
####ADVANCED PAGE------
tabItem(tabName="Advanced",
          sidebarLayout(
            mainPanel(
              verticalLayout(
                timevisOutput("weatherlineAD"),
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
                selectInput("stofAD", "Toon stof:", 
                            choices = list("StikstofDioxide","Fijnstof 10","Fijnstof 2,5")),
                dateRangeInput("ana", "Kies periode:",
                               start = as.Date("2016-06-01"), end= as.Date("2016-06-02")),
                sliderInput("time", "Tijdlijn",
                            min = 1, max = 24, 
                            value = c(1,24), step=1,
                            animate = animationOptions(loop = TRUE, interval = 1)),
                checkboxGroupInput("weekdays", "Selecteer dagen:",
                                   choices=c("Maandag","Dinsdag","Woensdag","Donderdag",
                                   "Vrijdag","Zaterdag","Zondag"),
                                   selected=c("Maandag","Dinsdag","Woensdag","Donderdag",
                                              "Vrijdag","Zaterdag","Zondag")),
              tags$textarea(id="foo", rows=3, cols=40, 
                            "Heeft u iets waargenomen? Plaats het in de tijdlijn!"),
              actionButton("readCom",label="Plaats opmerking"))
            )
          )
),
#### MATHFACTS PAGE-----
tabItem(tabName="MathFACT",fluidPage()),
#### FEEDBACK PAGE--------
tabItem(tabName="Feedback",fluidPage()))))

server <- function(input, output, session) {
  ####FUNCTIONS------
    selDAT <- function(){
      return(
        DATA %>% filter(Date==input$anaBA) %>% filter(Time==input$timeBA))
        }
    werDat <- function(){
      return(
        weerDat %>% filter(Date==input$anaBA) %>% filter(Time==input$timeBA))
    }
    selSTOFba <- reactive({switch(input$stofBA,
                                StikstofDioxide="no2a",
                                `Fijnstof 10`="pm10",
                                `Fijnstof 2,5`="pm25")})
    selSTOFad <- reactive({switch(input$stofBA,
                                StikstofDioxide="no2a",
                                `Fijnstof 10`="pm10",
                                `Fijnstof 2,5`="pm25")})
    selDatAD <- function(){
      return(
        DATA %>% filter(Date>=input$ana[1]) %>% filter(Date<=input$ana[2]) %>% 
          filter(Time>=input$time[1])) %>% filter(Time<=input$time[2])
    }
    werDatAD <- function(){
      return(
        weerDat %>% filter(Date>=input$ana[1]) %>% filter(Date<=input$ana[2]) %>% 
          filter(Time>=input$time[1])) %>% filter(Time<=input$time[2])
    }
    #####COMMENTTIMEVIS FUNCITONS----------
    loadData <- function() {
      comdata <- read.csv("commDat.csv",stringsAsFactors = FALSE,header=TRUE,sep=";")
      comdata$start <- as.Date(comdata$start)
      comdata$end <- as.Date(comdata$end)
      return(data.frame(comdata))
    }
    comText <- eventReactive(input$readCom, {
      input$foo
      })
    #updateTextInput(session, "inText", value = paste("New text", x))
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
      leafletProxy("leafBA",data=selDAT()) %>%
        clearShapes() %>% clearPopups() %>%
        addCircles(radius=~temp/10, fill=FALSE,
                   popup=~htmlEscape(paste(id,"\n",no2a,"\n",pm10,"\n",temp,"\n")))
    })
    ###valueboxesBA----------
    output$WindRBoxBA <- renderValueBox({
      valueBox(
        paste0(werDat()$direct), "Windrichting",
        icon = icon("location-arrow", lib = "font-awesome"),
        color = "blue")
    })
    output$WindKBoxBA <- renderValueBox({
      valueBox(
        paste0(werDat()$Windsnelheid/10," m/s"), "Windkracht",
        icon = icon("fa", lib = "font-awesome"),
        color = "blue")
    })
    output$TempBoxBA <- renderValueBox({
      valueBox(
        paste0(werDat()$Temp/10," °C"), "Temperatuur",
        icon = icon("thermometer-three-quarters"),
        color = "blue")
    })
    output$RainBoxBA <- renderValueBox({
      valueBox(
        paste0(werDat()$Neerslag," mm"), "Regen",
        icon = icon("tint", lib = "glyphicon"),
        color = "blue")
    })
    output$NO2BoxBA <- renderValueBox({
      valueBox(
        paste0(round(selDAT()$no2a,2)," ug/m3"), "Stikstof",
        icon = icon("tint", lib = "glyphicon"),
        color = "blue")
    })
    output$PMBoxBA <- renderValueBox({
      valueBox(
        paste0(round(selDAT()$pm10,2)," ug/m3"), "Fijnstof",
        icon = icon("tint", lib = "glyphicon"),
        color = "blue")
    })
    
    ##### ADVANCED PAGE--------
    #### valueboxesAD-----
    output$WindRBox <- renderValueBox({
      valueBox(
        paste0(werDat()$direct), "Windrichting",
        icon = icon("location-arrow", lib = "font-awesome"),
        color = "blue")
    })
    output$WindKBox <- renderValueBox({
      valueBox(
        paste0(round(mean((werDat()$Windsnelheid/10), na.rm = TRUE),2)), "Windkracht",
        icon = icon("fa", lib = "font-awesome"),
        color = "blue")
    })
    output$TempBox <- renderValueBox({
      valueBox(
        paste0(round(mean((werDat()$Temp/10),na.rm = TRUE),2)), "Temperatuur",
        icon = icon("thermometer-three-quarters"),
        color = "blue")
    })
    output$RegenBox <- renderValueBox({
      valueBox(
        paste0(round(mean((werDat()$Neerslag), na.rm = TRUE),2)), "Regen",
        icon = icon("tint", lib = "glyphicon"),
        color = "blue")
    })
    #### timevissesAD------
    output$weatherlineAD <- renderTimevis({
      timevis()
    })
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
      leafletProxy("leafAD",data=selDAT()) %>%
        clearShapes() %>% clearPopups() %>%
        addCircles(radius=~temp/10, fill=FALSE,
                   popup=~htmlEscape(paste(id,"\n",no2a,"\n",pm10,"\n",temp,"\n")))
    })
    output$TR <- renderPlotly({
      TR <- ggplot(selDatAD(), aes(x = paste(Date,Time,sep=" "), y = temp, color=factor(id)))+
        geom_line()
      ggplotly(TR)
    })
  }
  
shinyApp(ui, server)

# windroos plot:
# output$NOXplot <- renderPlotly({
#   p <- plot_ly(plotly::wind, t = ~werDat()$Windrichting, r = ~(werDat()$Windsnelheid/10),
#                type = 'area',color=I("Darkred"))
#   layout(p, radialaxis = list(ticksuffix="m/s"),orientation = 270)
# })

###TO DO LIST:---------
###Actief maken van selectinput van stof (BA en AD)-----
###Kleur schaal op kaart basic page naar gehalte----
###Popup juiste invulling geven(straat en waarde naar selectie)(BA en AD)----
###Punten plotten en selectie kunnen maken op leaflet(AD)----
###Plot netjes, assen en titels juiste weergave (AD)----
###Interface maken voor weergegevens(AD)----
