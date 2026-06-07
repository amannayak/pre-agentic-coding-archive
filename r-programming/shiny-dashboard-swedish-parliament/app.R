library(shiny)
library(shinythemes)
library(shinyjs)
devtools::install_github("https://github.com/amannayak/swedishParlimentAPI.git" , upgrade = "always")
library("swedishParlimentAPI")
source("./Politician.R")
dataContext = parliamentAPI()#calling API outside in order to download all required data
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
shinyApp(
  ui = tagList(
   # shinythemes::themeSelector(),
    navbarPage(	  
      theme = "cyborg", 
      "shinythemes",
     # tabPanel("Select Tab for different Results",
               sidebarPanel(
                 dateRangeInput(inputId ="DateRange",
                                label = "Choose Date to see the Comparission of Male Vs Female in Area",
                                start = "2000-01-01",
                                end = "2010-01-01",
                                min = "1990-01-01",
                              #  max = "2100-01-01",
                                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                language = "en", separator = " to ", width = NULL
                 )
                 ,         
                 useShinyjs(),
                 extendShinyjs(text = jscode, functions = c("closeWindow")),
                 actionButton("close", "Close window")
               ),#sidebarPanel(
               mainPanel(
                 tabsetPanel(
                   tabPanel("Graph",
                            h4("Comparission Graph"),
                            plotOutput("RatioPie"),
                            h4("Header 4")
                   )
                   #,
                   #tabPanel("Details", "List of all people")
                 )
               )#mainPanel(
      #),#tabPanel("Select Tab for different Results",
    )#navbarPage
  ),#ui = tagList(
  server = function(input, output) {
    observeEvent(input$close, {
      js$closeWindow()
      stopApp()
    })#observeEvent
  output$RatioPie = renderPlot(
    #calling class object to make calculation based on user inputs
    {
    opList = dataContext$CalData(input$DateRange[1],input$DateRange[2])
    slices = c(opList$CountFemale , opList$CountMan)
    lbs = c("Female" , "Male")
    pct = round((slices/sum(slices))* 100)
    lbs = paste(lbs,pct)
    lbs = paste(lbs,"%",sep = "")
    pie(slices, labels = lbs,col = rainbow(length(lbs)),main="Male to Female")
    }
  )#output$RatioPie = renderPlot(
    
  }#server	
)#shinyApp
