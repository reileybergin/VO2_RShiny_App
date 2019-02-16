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
  shinyUI(navbarPage("VO2max Trainer",
                     theme = shinytheme("flatly"),
                   
#tab 1 ----------------------------------------------------
tabPanel("Enter Warmup",
  
     #Inputs
      sidebarPanel(
        
        #stage 1 values
        wellPanel(
          h4("Stage 1"),
          
          #enter in values for warmUPdf
          
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "s1", 
              label = h5("Speed (mph):"), 
              value = 6.0)),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "v1", 
              label = h5("VO2 (ml/kg/min):"), 
              value = 29.3))
        ),
        
        #stage 2 values
        wellPanel(
          h4("Stage 2"),
          
          #enter in values for warmUPdf
          
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "s2", 
              label = h5("Speed (mph):"), 
              value = 6.5)),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "v2", 
              label = h5("VO2 (ml/kg/min):"), 
              value = 33.8))
          ),
        
        #stage 3 values
        wellPanel(
          h4("Stage 3"),
          
          #enter in values for warmUPdf
          
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "s3", 
              label = h5("Speed (mph):"), 
              value = 7.0)),
          div(style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "v3", 
              label = h5("VO2 (ml/kg/min):"), 
              value = 36.9))
        ),
        
        #stage 4 values
        wellPanel(
          h4("Stage 4"),
          
          #enter in values for warmUPdf
          
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
        br(), br(),
        h5("Created by Reiley.Bergin@gmail.com")
        
        ),

      # Ouputs 
      mainPanel(
        
        plotOutput(outputId = "scatterplot"), 
        DT::dataTableOutput(outputId = "warmUPtb")
        
        )
      ), 

#tab 2--------------------------------------------------
tabPanel("Calculate Training Intensity",
         
         sidebarPanel(
 
           wellPanel(
             h4("Enter training parameters"),
             
             numericInput(inputId = "vo2max", 
                          label = h5("Enter your VO2max (ml/kg/min):"), 
                          value = 52),
             numericInput(inputId = "speed", 
                          label = h5("Speed (mph):"), 
                          value = 7.5),
             numericInput(inputId = "vo2per", 
                          label = h5("%VO2max (ml/kg/min):"), 
                          value = 80)
             )
            ),
         
         # Ouputs
         mainPanel(
           DT::dataTableOutput(outputId = "speedtb"),
           DT::dataTableOutput(outputId = "vo2tb")
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
    data.frame(row.names = c("Stage 1","Stage 2", "Stage 3", "Stage 4"),"Speed" = c(input$s1,input$s2,input$s3,input$s4),"VO2" = c(input$v1,input$v2,input$v3,input$v4))
    })
  
  
  # Create scatter plot
  output$scatterplot <- renderPlot({
    ggplot(data = warmUPdf(), aes_string(x = 'Speed', y = 'VO2')) +
      geom_point() +
      theme_bw() +
      geom_smooth(method = lm, se = FALSE, linetype = "dashed",
      color = "blue", fill = "blue")
    
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
  

  # Speed Table (enter speed to calculate vO2)
  speeddf <- reactive({
    req(input$vo2per,input$speed)
    data.frame("Speed(mph)" = c(input$speed),
               "PercentVO2max" = c(round(((input$speed*linearmodel()$coefficients[2] + linearmodel()$coefficients[1])/input$vo2max),digits = 2)*100))
    
  })
  
  
  # Vo2 Table (enter speed to calculate vO2)
  vo2maxdf <- reactive({
    req(input$vo2per,input$speed)
    data.frame("Speed(mph)" = c(round(((((input$vo2per/100)*input$vo2max) - linearmodel()$coefficients[1])/linearmodel()$coefficients[2]),digits = 1)),
              "PercentVO2max" = c(input$vo2per))
    
  })
  

  # Create speed % data table
  output$speedtb <- renderDataTable({
    DT::datatable(data = speeddf(), 
                  options = list(pageLength = 5, dom = 't'),
                  selection = 'none',
                  class = 'cell-border stripe',
                  rownames = FALSE)
    
  })
  
  # Create vo2 data table
  output$vo2tb <- renderDataTable({
    DT::datatable(data = vo2maxdf(), 
                  options = list(pageLength = 5, dom = 't'),
                  class = 'cell-border stripe',
                  rownames = FALSE)
    
  })
  

  
  # model output
  #output$datatype <- renderPrint({
    #VO2()
 # })
  
  #output$summary <- renderPrint({
   # linearmodel()$coefficients[1]
    #})
  
}


# Run the application 
shinyApp(ui = ui, server = server)

