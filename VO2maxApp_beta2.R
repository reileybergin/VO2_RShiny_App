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
                          label = h5(tags$strong("Enter your VO2max (ml/kg/min):")), 
                          value = 52),
             # Ouputs
             htmlOutput("MAStxt")
             
             )
          
            ),
         
         # Mainpanel
         htmlOutput("MASinfo"),
         
         
         #Outputs
         mainPanel(
           DT::dataTableOutput(outputId = "MAStb")
           )
      ),

#tab 3--------------------------------------------------
tabPanel("Calculate VO2max Training Intensity",
         
         sidebarPanel(
           
           wellPanel(
             
             numericInput(inputId = "vo2max", 
                          label = h5("Enter your VO2max (ml/kg/min):"), 
                          value = 52),
             numericInput(inputId = "speed", 
                          label = h5("Enter Speed (mph):"), 
                          value = 7.5),
             numericInput(inputId = "vo2per", 
                          label = h5("Enter %VO2max (ml/kg/min):"), 
                          value = 80)
           )
         ),
         
         # Ouputs
         mainPanel(
           htmlOutput("Ifspeed"),
           htmlOutput("If%Vo2max")

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
                  caption = 'Warm Up Values',
                  class = 'cell-border stripe',
                  rownames = TRUE)
  })
  
  # Create model
  linearmodel <- reactive({
   lm(warmUPdf()$VO2 ~ warmUPdf()$Speed, data = warmUPdf())
  })
  

  # MAS value
  masval <- reactive({round((input$vo2max - linearmodel()$coefficients[1])/linearmodel()$coefficients[2],digits = 1)
    
  })
  
  # MAS text output
  output$MAStxt <- renderText({paste("<h5>Your Maximal Aerobic Speed (MAS) is <b> <font color=blue>", masval(),"mph")})
  
  # MAS information text
  output$MASinfo <- renderText({paste("<h3> <font color=blue> Maximal Aerobic Speed (MAS) </h3> </font color=blue>
                                      <br> Your MAS is the lowest running speed at which maximum oxygen uptake (VO2max) occurs. 
                                      The amount of time you spend training at or above 100% MAS is a critical factor for improving aerobic power.
                                      <br> <br> In the table below are training zones based on your MAS. These training zones are derived from <i> Baker et al Recent trends in high intensity aerobic training for field sports.")
    
    })
  
  
  # Build MAS dataframe
  masdf <- reactive({
    req(input$vo2max)
    data.frame(row.names = c("Zone1", 
                             "Zone2", 
                             "Zone3", 
                             "Zone4",
                             "Zone5",
                             "Zone6"
              ),
              "AerobicTraining" = c("Aerobic Recovery", 
                                    "Aerobic Threshold", 
                                    "Aerobic #2",
                                    "Anaerobic Threshold", 
                                    "Maximal Aerobic",
                                    "Supra-Maximal Aerobic"
              ),
              
              "MAS" = c("<70% MAS", 
                        "70-77%MAS", 
                        "78-85%MAS",
                        "86-92%MAS", 
                        "93-100%MAS",
                        ">100%MAS"
              ),
              
              "Speed LowerLimit" = c(NA,
                                         round(masval()*.70,1),
                                         round(masval()*.78,1),
                                         round(masval()*.86,1),
                                         round(masval()*.93,1),
                                         round(masval(),1)
              ),
                              
              "Speed UpperLimit" = c(round(masval()*.70,1), 
                                         round(masval()*.77,1),
                                         round(masval()*.85,1),
                                         round(masval()*.92,1),
                                         round(masval(),1),
                                         NA
                                         )
              )
    
  })
  


  # Ouput MAS data table
  output$MAStb <- renderDataTable({
    DT::datatable(data = masdf(), 
                  options = list(pageLength = 6, dom = 't'),
                  class = 'cell-border stripe',
                  rownames = TRUE)
    
  })
  
  # %Vo2max value
  pervo2max <- reactive({
    req(input$speed)
    c(round(((input$speed*linearmodel()$coefficients[2] + linearmodel()$coefficients[1])/input$vo2max),digits = 2)*100)
    
  })
  
  # MAS information text
  output$Ifspeed <- renderText({paste("<h4> Running at <b>", input$speed, "mph </b> is equal to <b> <font color=blue>", pervo2max() ,"% </b> </font color=blue> of your 
                                      VO2max")})
    
  
  # Vo2 Table (enter speed to calculate vO2)
  vo2maxdf <- reactive({
    req(input$vo2per,input$speed)
    data.frame("Percent.VO2max" = c(input$vo2per),
      "Speed(mph)" = c(round(((((input$vo2per/100)*input$vo2max) - linearmodel()$coefficients[1])/linearmodel()$coefficients[2]),digits = 1))
    )
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

