

library(shiny)
setwd("C:/Users/Thom/Documents/HvA/Jaar 3/Minor/Case study")

ui<-
  bootstrapPage(
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
          
          # Creëer de header
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
        )
      )
    )
  )
)
  
library(shiny)
library(stringr)
  
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
  
  server<- function(input, output, session) {
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
  
  shinyApp(ui, server)
  