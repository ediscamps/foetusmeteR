library(shiny)



#### A CHANGER
# faire un bouton pour ajouter des mesures ? avec un dataframe qui s'incrémente au fur et à mesure




### Before launching shiny

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)

df <- read_excel("RangiferFoetusFinland.xlsx", sheet="KeyData")

df_mes <-  data.frame(bone = c(rep("Femur",10),rep("Humerus",10),rep("Metacarpal",8),rep("Metatarsal",8),
                               rep("Radius",9),rep("Tibia",10)),                  mes = colnames(df[,2:56]))


lm_function_age <- function(mes)
{
  df_lm <- subset(df, select = c(mes,"EstimAgeWeek"))
  #removing NAs
  df_lm <- df_lm[complete.cases(df_lm),]
  lm <- lm(as.formula(paste0("EstimAgeWeek ~ poly(",mes,",2)")), data = df_lm)
}

lm_function_2mes <- function(mes1, mes2)
{
  df_lm <- subset(df, select = c(mes1,mes2))
  lm <- lm(as.formula(paste(mes2,"~",mes1)), data = df_lm)
}

# Define UI
ui <- fluidPage(

    # Application title
  titlePanel(title = span(img(src = "logo.png", height = 100), "foetusmeteR")),
    
    
    tabsetPanel(
     
     #tab1
      tabPanel("Introduction",
               br(),
               p(strong("Welcome to foetusmeteR")),
               br(),
               "This R Shiny app is intended to be used following the protocol described in Discamps & Soulier (2024) for",
               em("Rangifer tarandus"), "foetal bones.",
               br(),
               br(),
               p("Use the first panel (Estimating age) to estimate the age and season of death of a foetal bone from a single measurement."),
               p("Use the second panel (Predicting another measurement) to test, with two measurements, if two bones might correspond to the same individual (e.g. to estimate the Minimal Number of Individuals)."),
               ),
      
      #tab2
      tabPanel("Estimating age",
               h2("Estimating foetal age"),
               p(strong("Measurement used for estimation")),
               fluidRow(
                     column(2,               
                            selectInput(inputId = "boneAge", "Bone", c("Humerus", "Radius", "Metacarpal","Femur", "Tibia",  "Metatarsal"))
                            ),
                    column(2,
                     uiOutput("mesAge_ID")
                          ),
                     column(4, offset = 1,
                   numericInput(inputId = "mesAge_input", "Measurement (mm)", value = 60)
                     )
                   ),
               span(textOutput("warningtext"), style="color:red"),
               br(),
               
               fluidRow(
                 column(5,
                        p("Select type of interval (prediction is recommended):")
                 ),
                 column(3,
                        selectInput(inputId = "interval_type", NULL, c("confidence", "prediction"), selected = "prediction")
                 )
               ),
               
               
               p(strong("Predicted age (weeks), using Roine et al. 1982 data, with lower and upper intervals")),
                   verbatimTextOutput("age_lmoutput"),
               p("Caution: no foetus younger than 21 weeks were observed, hence predicted age below 21 weeks is more prone to error"),
               p("Similarly, estimations after 30 weeks are better considered simply as 'newborn?'."),
               p(strong("Predicted season(s) of death, with predictions for lower and upper intervals")),
                  verbatimTextOutput("EstimAgeMonth"),
                   p(strong("Keep in mind that these are raw estimations, subject to error!")),
                   br(),
                   h2("Polynomial regression (degree 2)"),
                   p(strong("Scatterplot of reference dataset for the selected measurement with estimated age")),
                   plotOutput("scatterplot_age", height = 300),
               fluidRow(
                 column(2,
                        numericInput(inputId = "x_breaksAge", "Break width (X axis)", value = 10)
                        ),
                 column(2,
                   numericInput(inputId = "y_breaksAge", "Break width (Y axis)", value = 1)
                 )),
                   p(strong("Coefficients of polynomial regression (degree 2)")),
                   verbatimTextOutput("summary_age")

               ),

      tabPanel("Predicting another measurement",
               
               sidebarLayout(
                 sidebarPanel(
                   p(strong("Known measurement")),
                   selectInput(inputId = "bone1", "Bone", c("Humerus", "Radius", "Metacarpal","Femur", "Tibia",  "Metatarsal"), selected = "Humerus"),
                            uiOutput("mes1_ID"),
                   numericInput(inputId = "mes1_input", "Measurement (mm)", value = 60),
                   br(),
                   p(strong("Estimated measurement")),
                
                   selectInput(inputId = "bone2", "Bone", c("Humerus", "Radius", "Metacarpal","Femur", "Tibia",  "Metatarsal"), selected = "Femur"),
                   uiOutput("mes2_ID")
                   # ,
                   # 
                   # 
                   # selectInput(inputId = "mes1_ID", "Name of knwon measurement",
                   #             colnames(df[,-1])),
                   # selectInput(inputId = "mes2_ID", "Name of predicted measurement",
                   #             colnames(df[,-1]))
                 ),
                 mainPanel(
                   h2("Predicting values"),
                   
                   fluidRow(
                     column(5,
                            p("Select type of interval (prediction is recommended):")
                     ),
                     column(3,
                            selectInput(inputId = "interval_type_2mes", NULL, c("confidence", "prediction"), selected = "prediction")
                     )
                   ),
                   
  
                   p(strong("Predicted measurement (mm) with lower and upper intervals")),
                   verbatimTextOutput("mes2_lmoutput"),
                   br(),
                   h2("Linear regression"),
                   p(strong("Scatterplot of reference dataset for the two selected measurements")),
                   plotOutput("scatterplot_2mes"),
                   fluidRow(
                     column(3,
                            numericInput(inputId = "x_breaks2mes", "Break width (X axis)", value = 10)
                     ),
                     column(3,
                            numericInput(inputId = "y_breaks2mes", "Break width (Y axis)", value = 10)
                     )),
                   p(strong("Coefficients of linear regression")),
                   verbatimTextOutput("summary_2mes")

                 )
               )
               
               
      )
      ))
    
    

 
       



server <- function(input, output) {

  ### slider inputs for mes

  # Get available categories
  var_mesAge <- reactive({
    filter(df_mes, bone %in% input$boneAge) %>% 
      pull(mes) %>% 
      unique()
  })
  
  var_mes1 <- reactive({
    filter(df_mes, bone %in% input$bone1) %>%
      pull(mes) %>%
      unique()
  })

  var_mes2 <- reactive({
    filter(df_mes, bone %in% input$bone2) %>%
      pull(mes) %>%
      unique()
  })
  

  output$mesAge_ID <- renderUI({
    selectInput(inputId = "mesAge_ID", "Measurement ID", choices = var_mesAge(), multiple = F)
  })

  output$mes1_ID <- renderUI({
    selectInput(inputId = "mes1_ID", "Measurement ID", choices = var_mes1(), multiple = F)
  })

  output$mes2_ID <- renderUI({
    selectInput(inputId = "mes2_ID", "Measurement ID", choices = var_mes2(), multiple = F)
  })
  
  # output$interval <- renderUI({
  #   selectInput(inputId = "interval_type", "Type of interval", choices = c("confidence","prediction"), multiple = F)
  # })
  
  # text message for max input
  #Create the warning text - warning if TRUE, empty string if FALSE

  output$warningtext <- renderText(ifelse(TRUE %in% (input$mesAge_input > max(select(df, input$mesAge_ID), na.rm=T)),
                                          'Warning: your measurement is higher than the maximum present in the reference dataset for that measurement!',
                                          '')) 
  
  # lm summary output
    output$summary_2mes <- renderPrint({
      fit <-    lm_function_2mes(input$mes1_ID, input$mes2_ID)
    fit$coefficients
  })
  
    output$summary_age <- renderPrint({
      fit <-    lm_function_age(input$mesAge_ID)
      fit$coefficients
    })
    
    
    # Scatterplot output
    output$scatterplot_2mes <- renderPlot({
      ggplot(df, aes_string(paste0(input$mes1_ID), input$mes2_ID)) +
        geom_point() +
        geom_smooth(method = lm)+
        scale_x_continuous(breaks=breaks_width(input$x_breaks2mes))+
        scale_y_continuous(breaks=breaks_width(input$y_breaks2mes))
    })
    
    output$scatterplot_age <- renderPlot({
      ggplot(df, aes_string(paste0(input$mesAge_ID), "EstimAgeWeek")) +
        geom_point() +
        geom_smooth(method = lm, formula = y ~ poly(x,2))+
        scale_x_continuous(breaks=breaks_width(input$x_breaksAge))+
        scale_y_continuous(breaks=breaks_width(input$y_breaksAge))
    })
    
 output$mes2_lmoutput <- renderPrint({
      fit <-    lm_function_2mes(input$mes1_ID, input$mes2_ID)
      newdataInput <- data.frame(input$mes1_input)
      colnames(newdataInput) <- input$mes1_ID
    predict(fit, newdata = newdataInput, interval = input$interval_type_2mes)
    x <- predict(fit, newdata = newdataInput, interval = input$interval_type_2mes)
    x[,1:3] #just to remove the "1" in front of the numbers
 })
 
 # output$age_lmoutput <- renderPrint({
 #   fit <-    lm_function_age(input$mesAge_ID)
 #   newdataInput <- data.frame(input$mesAge_input)
 #   colnames(newdataInput) <- input$mesAge_ID
 #   predict(fit, newdata = newdataInput, interval = "confidence")
 #   x <- predict(fit, newdata = newdataInput, interval = "confidence")
 #   x[,1:3] #just to remove the "1" in front of the numbers
 # })
  
 
 age_lm <- reactive({
   fit <-    lm_function_age(input$mesAge_ID)
   newdataInput <- data.frame(input$mesAge_input)
   colnames(newdataInput) <- input$mesAge_ID
   predict(fit, newdata = newdataInput, interval = input$interval_type)
 })

 output$age_lmoutput <- renderPrint({
   y <- age_lm()
   y[,1:3] #just to remove the "1" in front of the numbers
   row.names(y) <- "age (weeks)"
   y
 })
 
output$EstimAgeMonth <- renderPrint({
  x <- as.double(age_lm()*7)
  y <- case_when(
    (x <= 21) ~ "October?",
    (x > 21 & x <= 51) ~ "November?",
    (x > 51 & x <= 82) ~ "December?",
    (x > 82 & x <= 113) ~ "January?",
    (x > 113 & x <= 141) ~ "February?",
    (x > 141 & x <= 173) ~ "March?",
    (x > 173 & x <= 203) ~ "April?",
    (x > 203 ) ~ "May?")
  y <- as.data.frame(t(y))
 colnames(y) <- c("fit","lwr","upr")
 row.names(y) <- "season of death"
 y
})


}

# Run the application 
shinyApp(ui = ui, server = server)

