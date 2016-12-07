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
SensLoc <- read.csv("Senslonglat.csv")
colnames(SensLoc) <- c("id","lat","long")
DATA <- merge(DATA,SensLoc,by="id")

# info <- data.frame(content=NULL,start=NULL,end=NULL)
write.csv(DATA,"DATAtot.csv",row.names=FALSE)
DATA <- read.csv("DATAtot.csv")
weerDat <- read.csv("weerdata.csv")
DATA$Date <- ymd(DATA$Date)
weerDat$Date <- ymd(weerDat$Date)
DATA$id <- as.factor(DATA$id)
for(i in 1:length(levels(DATA$id))){
  assign(paste("df",i,sep=""),DATA[DATA$id==levels(DATA$id)[i],])
}

ui <- dashboardPage(
  dashboardHeader(title = "Gewaagd Dashboard", titleWidth = 250),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Advanced", icon = icon("th"), tabName = "Advanced",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("MathFact", tabName = "MathFACT", icon = icon("dashboard")),
    menuItem("Feedback", tabName = "Feedback", icon = icon("dashboard")))),
  dashboardBody(    
    tabItems(
      tabItem(tabName="Overview",
        sidebarLayout(
          mainPanel(
            verticalLayout(
              splitLayout(
                valueBoxOutput("WindRBox",width="25%"),
                valueBoxOutput("WindKBox",width="25%"),
                valueBoxOutput("TempBox",width="25%"),
                valueBoxOutput("RegenBox",width="25%")),
              leafletOutput("leaf"),
              timevisOutput("timeline")
              )
          ),
       sidebarPanel(
         verticalLayout(
          sliderInput("slider", "Size of Dot:", 1, 10, 1),
          dateRangeInput("ana", "Kies periode:",
                             start = as.Date("2016-06-01"), end= as.Date("2016-06-02")),
          sliderInput("time", "Tijdlijn",
                          min = 0, max = 24, 
                          value = c(0,24), step=1,
                          animate = animationOptions(loop = TRUE, interval = 1)),
          tags$textarea(id="foo", rows=3, cols=40, 
                            "Heeft u iets waargenomen? Plaats het in de tijdlijn!"),
          actionButton("readCom",label="Plaats opmerking")
       )
      )
    )
),tabItem(tabName="Advanced",fluidPage()),
tabItem(tabName="MathFACT",fluidPage()),
tabItem(tabName="Feedback",fluidPage()))))

server <- function(input, output, session) {
  loadData <- function() {
    comdata <- read.csv("commDat.csv",stringsAsFactors = FALSE,header=TRUE,sep=";")
    comdata$start <- as.Date(comdata$start)
    comdata$end <- as.Date(comdata$end)
    return(data.frame(comdata))
  }
  selDAT <- function(){
    return(
      DATA %>% filter(Date>=input$ana[1]) %>% filter(Date<=input$ana[2]) %>% 
        filter(Time>=input$time[1]) %>% filter(Time<=input$time[2]))
  }
  werDat <- function(){
    return(
      weerDat %>% filter(Date>=input$ana[1]) %>% filter(Date<=input$ana[2]) %>% 
        filter(Time>=input$time[1]) %>% filter(Time<=input$time[2]))
  }
  comText <- eventReactive(input$readCom, {
    input$foo
    })
  obs <- observe({    
    cat(comText(),";",as.character(input$ana[1]),";",as.character(input$ana[2]),
        '\n', file = "commDat.csv", append = TRUE)
  })
  output$weather <- renderPlot({
    ggplot(data=werDat())+
      geom_point(aes(x=paste(Date,Time),y=Temp))
  })
  output$leaf <- renderLeaflet({
      leaflet(selDAT()) %>%
      addTiles() %>%
      addCircles(radius = ~temp*input$slider, weight = 1, color = "Darkred") %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  output$WindRBox <- renderValueBox({
    valueBox(
      paste0(round(mean((weerDat$Windrichting/10), na.rm = TRUE),2)), "Windrichting", icon = icon("direction", lib = "glyphicon"),
      color = "maroon"
    )
  })
  output$WindKBox <- renderValueBox({
    valueBox(
      paste0(round(mean((weerDat$Windsnelheid/10), na.rm = TRUE),2)), "Windkracht", icon = icon("f0b2", lib = "font-awesome"),
      color = "maroon"
    )
  })
  output$TempBox <- renderValueBox({
    valueBox(
      paste0(round(mean((weerDat$Temp/10),na.rm = TRUE),2)), "Temperatuur", icon = icon("thermometer-three-quarters",lib = "font-awesome"),
      color = "maroon"
    )
  })
  output$RegenBox <- renderValueBox({
    valueBox(
      paste0(round(mean((weerDat$Neerslag), na.rm = TRUE),2)), "Regen", icon = icon("tint", lib = "glyphicon"),
      color = "maroon"
    )
  })
  output$timeline <- renderTimevis({
    timevis(loadData())
  })
}

shinyApp(ui, server)

# TO DO
# weerdata downl+ visualiseren


# tests <- DATA %>% filter(Date<="2016-06-02") %>% filter(Time<=15)
# fn <- fivenum(tests$no2a)
# jitwid <- position_jitter(0.5)
# ggplot(data=tests, mapping = aes(x = 0))+
#   geom_boxplot(aes(y=no2a))+
#   coord_flip()
# ggplot(data=tests, mapping = aes(x = 0))+
#   geom_boxplot(aes(y=pm10))+
#   coord_flip()
# TODODO
# 3 niveaus - basic dash, complex dash, model voor drift (eindproducten)
#
# 
# output$NOXplot <- renderPlotly({
#   p <- plot_ly(plotly::wind, t = ~werDat()$Windrichting, r = ~(werDat()$Windsnelheid/10),
#                type = 'area',color=I("Darkred"))
#   layout(p, radialaxis = list(ticksuffix="m/s"),orientation = 270)
# })
