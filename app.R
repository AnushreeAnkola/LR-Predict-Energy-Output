rm(list = ls())

# Install the Shiny package from R library before. 
# install.pavkages("shiny")
library(shiny)

# Increase file upload size
options(shiny.maxRequestSize=30*1024^2)
# Install the packages before use
library(shinydashboard)
library(RMySQL)
library(xlsx)

# get the path of current directory
# getwd() 
# set the path of working directory
# setwd("C:/Users/ehass/Desktop")

# power_Data <- read.xlsx('Power.xlsx', 1)

Logged = FALSE;
my_username <- "adbms1"
my_password <- "admin1"


# con = dbConnect(MySQL(), user='', password='', dbname="test", host='localhost')
# # myquery <- "select * from user_details"
# # datazika <- dbGetQuery(con, myquery)
# rs = dbSendQuery(con, "select * from virus")
# print("connection")
# # View(rs)
# data = fetch(rs, n=-1)
# # View(data)
# print(ncol(data))
# print(colnames(data))



ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Upload", tabName = "upload", icon = icon("dashboard"))
   
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "upload",
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                # Input: Select a file ----
                fileInput("file1", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
                
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(

                        # Output: Data file ----
                        textOutput("selected_var"),
                        textOutput("value"),
                        plotOutput("plot")
              )
           )
    )
  )
)



ui2 <- function(){tagList(tabPanel(my_username), dashboardHeader(title = "Predicting Energy Output"), 
                       sidebar,body   )
  }

ui = (htmlOutput("page"))
server = (function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Contratulations you got in!",ui2())))
      })
    }
  })
  
  
  output$selected_var <- renderText({ 
    "The Graph to compare Actual and Predicted value of our Net hourly electrical energy output PE"
  })
  
  output$plot <- renderPlot({
    req(input$file1)
    
    
    
    # #adding data
    inFile <- input$file1
    data3<-read.csv(inFile$datapath)
    assign('datazika',data3,envir=.GlobalEnv)
  
    output$myFileName <- renderText({ file_name() })
    #to connect to the database
    con = dbConnect(MySQL(), user='', password='', dbname="test", host='localhost')
    print("connection")
    dbWriteTable(con, "power_Data", data3,  row.names=T,append=T )

    myquery <- "select * from power_Data"
    power_Data <- dbGetQuery(con, myquery)
    # View(power_Data)
    
    #delete the record
    # myquery2 <- "drop table power_Data"
    # dbGetQuery(con, myquery2)
    
    #close the db connection
    dbDisconnect(con)
    
   
    
    #The dataset contains five columns, namely, Ambient Temperature (AT), 
    #Ambient Pressure (AP), Relative Humidity (RH), Exhaust Vacuum (EV), 
    #and net hourly electrical energy output (PE) of the plant. 
    #The first four are the attributes, and are used to predict the output, PE
    
    #Display top 10 data values
    head(power_Data)
    
    #First, we set the seed so that the results are reproducible.
    set.seed(123)
    
    #create a sequence whose length is equal to the number of rows of the dataset
    split <- sample(seq_len(nrow(power_Data)), size = floor(0.8 * nrow(power_Data)))
    
    #create a sequence whose length is equal to the number of rows of the dataset
    training_Pow <- power_Data[split, ]
    testing_Pow <- power_Data[-split, ]
    
    #Print first 10 values
    head(training_Pow)
    
    #Print first 10 values
    head(testing_Pow)
    
    #building the prediction model;  predict PE based on the variables AT, V, AP, and RH
    prediction_Model <- lm(PE ~ ï..AT + V + AP + RH, data = power_Data)
    
    #Very useful inferences can be made from the output:
    #1. we can see that our R-squared value is 0.9284, which is very high.
    #2. #check the accuracy of a model is by looking at the R-squared value. 
    #The summary provides two R-squared values, namely Multiple R-squared, 
    #and Adjusted R-squared. The Multiple R-squared is calculated as follows:
    #Multiple R-squared = 1 â€“ SSE/SST
    
    summary(prediction_Model)
    
    
    #apply the prediction model to the test data; 
    test_prediction <- predict(prediction_Model, newdata = testing_Pow)
    
    #Actual values of PE from the testing part of dataset
    actual_PE <- testing_Pow$PE
    
    #View both the prediction value; analyse how close they actually are.
    # View(test_prediction)
    # View(actual_PE)
    
    #To compare values of R-sqaured (one found in summary vs one in one found below)
    #calculate the value of R-squared for the prediction model on the test data set 
    SSE <- sum((testing_Pow$PE - test_prediction) ^ 2)
    SST <- sum((testing_Pow$PE - mean(testing_Pow$PE)) ^ 2)
    R_sq <- ((1 - SSE/SST)*100)
    
    print("The R-squared value of testing dataset is ")
    print(R_sq)
    
    output$value <- renderText({ 
      # Horizontal line ----
      tags$hr()
      "The % of relation between the actual and predicted data as found to be"
      R_sq
    })
    #An R-squared value of 1 means that it is a perfect prediction model;
                                                                                                                                                                
    
    #output graph - how close is the predicted data (from model) with the tested - predicted data
    scatter.smooth(x=test_prediction, y=testing_Pow$PE, col = c("red","green"))
    
  })
  
  
  
})



runApp(list(ui = ui, server = server))
