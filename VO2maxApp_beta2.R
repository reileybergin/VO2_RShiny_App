library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  shinyUI(navbarPage("Aerobic Power Trainer",
                     theme = shinytheme("flatly"),
                   
#tab 1 ----------------------------------------------------
tabPanel("Enter Warmup Results",
  
     #Inputs
      sidebarPanel(
        
        #Warm up values
        wellPanel(
      
          #enter in values for warmUPdf
          
          h4(tags$strong("Stage 1")),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "s1", 
              label = h5("Speed (mph):"), 
              value = 6.0)),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "v1", 
              label = h5("VO2 (ml/kg/min):"), 
              value = 29.3)),
          
          h4(tags$strong("Stage 2")),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "s2", 
                           label = h5("Speed (mph):"), 
                           value = 6.5)),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "v2", 
                           label = h5("VO2 (ml/kg/min):"), 
                           value = 33.8)),
          
          h4(tags$strong("Stage 3")),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "s3", 
                           label = h5("Speed (mph):"), 
                           value = 7.0)),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "v3", 
                           label = h5("VO2 (ml/kg/min):"), 
                           value = 36.9)),
          
          h4(tags$strong("Stage 4")),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "s4", 
                           label = h5("Speed (mph):"), 
                           value = 7.5)),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "v4", 
                           label = h5("VO2 (ml/kg/min):"), 
                           value = 40.9))
        ),
        
        # Built with Shiny by RStudio
        br(),
        h5("Created by Reiley.Bergin@gmail.com", style = "color:blue")
        
        ),

      # Ouputs 
      mainPanel(
        
        DT::dataTableOutput(outputId = "warmUPtb")
        
        )
      ), 

#tab 2--------------------------------------------------
tabPanel("Calculate Aerobic Training Zones",
         
         sidebarPanel(
 
           wellPanel(
      
             numericInput(inputId = "vo2max", 
                          label = h4(tags$strong("Enter your VO2max (ml/kg/min):")), 
                          value = 52),
             # Ouputs
             htmlOutput("MAStxt")
             
             )
            ),
         
         # Ouputs
         mainPanel(
           DT::dataTableOutput(outputId = "MAStb")
           )
      )  
    )
  )     
)     

 
# Define server logic
server <- function(input, output) {
  
  # Create reactive data frame
  warmUPdf <- reactive({
    req(input$s1,input$v1,input$s2,input$v2, input$s3,input$v3, input$s4,input$v4)# ensure input$selected_var is available
    data.frame(row.names = c("Stage 1","Stage 2", "Stage 3", "Stage 4"),"Speed mph" = c(input$s1,input$s2,input$s3,input$s4),"VO2 ml/kg/min" = c(input$v1,input$v2,input$v3,input$v4))
    })
  
  
  # Create data table for warm up values
  output$warmUPtb <- renderDataTable({
    DT::datatable(data = warmUPdf(), 
                  options = list(pageLength = 5, dom = 't'),
                  class = 'cell-border stripe',
                  rownames = TRUE)
  })
  
  # Create model
  linearmodel <- reactive({
   lm(warmUPdf()$VO2 ~ warmUPdf()$Speed, data = warmUPdf())
  })
  

  # MAS value
  
  # MAS text output
  
  output$MAStxt <- renderText({paste("<h4>Your Maximal Aerobic Speed (MAS) is <b> <font color=blue>", input$vo2max,"mph")})
  
  # MAS Table 
  vo2maxdf <- reactive({
    req(input$vo2per,input$speed)
    data.frame("Speed(mph)" = c(round(((((input$vo2per/100)*input$vo2max) - linearmodel()$coefficients[1])/linearmodel()$coefficients[2]),digits = 1)),
              "PercentVO2max" = c(input$vo2per))
    
  })
  


  # Create vo2 data table
  output$MAStb <- renderDataTable({
    DT::datatable(data = vo2maxdf(), 
                  options = list(pageLength = 5, dom = 't'),
                  class = 'cell-border stripe',
                  rownames = FALSE)
    
  })
  
  # model output
  output$data <- renderPrint({
   })
  
  output$summary <- renderPrint({
   linearmodel()$coefficients[1]
  })

}


# Run the application 
shinyApp(ui = ui, server = server)

