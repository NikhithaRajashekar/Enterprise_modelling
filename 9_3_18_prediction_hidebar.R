library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)
options(max.print = 99999999)

ui <- tagList(
  useShinyjs(),
  
  navbarPage(
    "Prediction model(Caret Lab)", theme = shinytheme("flatly"),
    id = "navbar",
    tabPanel(
      title = "Upload dataset",
      sidebarPanel(
        fileInput("file1", "Upload preprocessed data"),
        br(),
        actionButton("button", "Upload"),
        
        HTML("<hr>"),
        actionButton("button11", "Proceed to prediction")
      ),
      mainPanel(tabsetPanel(tabPanel("Table",dataTableOutput("data1"))
                            
      ))
    ),
    
    tabPanel(
      title = "Community Evaluat",
      value = "mytab2",
      sidebarLayout(
        sidebarPanel(
          h4("Preprocessing"),
          checkboxInput("centerscale", "Center and Scale Variables", TRUE),
          br(),
          sliderInput("trainpercent",
                      "Fraction of data that goes to training",
                      value = 0.75, step = 0.05, min = 0.50, max = 0.80),
          
          br(),
          HTML("<hr>"),
          radioButtons("resample", h4("Train method"),
                       c("cross validation (cv)" = "cv",
                         "repeated cross validation (repeatedcv)"="repeatedcv",
                         "Default"="none")),
          br(),
          HTML("<hr>"),
          radioButtons("sampling", h4("Resample method(Class imbalance)"),
                       c("Over sampling(up)" = "up",
                         "Under sampling(down)"="down",
                         "smote"="smote")),
          br(),
          
          numericInput("randomseed", "Random Seed", 200, min=7, max=3000),
          HTML("<hr>"),
          radioButtons("method", h4("Caret Model"),
                       c("Classification and Regression Trees (rpart)" = "rpart",
                         "C5.0(c50)"                                   = "C5.0",
                         "Stochastic Gradient Boosting (gbm)"          = "gbm",
                         "Random Forest (rf)"                          = "rf",
                         "Support Vector Machines with Polynomial Kernel (svmPoly)"  = "svmPoly"
                       )),
          
          HTML("<hr>"),
          actionButton("button2", "Proceed")
          
        ),
        mainPanel(tabsetPanel(tabPanel("Fit",              verbatimTextOutput("fit")),
                              tabPanel("ConfusionMatrix",  verbatimTextOutput("confusion")),
                              tabPanel("DotPlot",          plotOutput("dotplot"))
                              
        )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    hide(selector = "#navbar li a[data-value=mytab2]")
  })
  
  values <- reactiveValues(df_data1 = NULL)
  
  observeEvent(input$button, {
    values$df_data1 <- read.csv(input$file1$datapath)
  })
  
  training<- reactive({
    set.seed(3033)
    inTrainSet <- createDataPartition(y=values$df_data1$Type, p=input$trainpercent, list=FALSE)
    training <- values$df_data1[inTrainSet,]
    training
    
  })
  
  validation<- reactive({
    set.seed(3033)
    inTrainSet <- createDataPartition(y=values$df_data1$Type, p=input$trainpercent, list=FALSE)
    validation <- values$df_data1[-inTrainSet,]
    validation
  })
  
  
  val<- eventReactive(input$button2,{
    
    preprocess.methods <- NULL
    if (input$centerscale) preprocess.methods = c("center", "scale")
    set.seed(input$randomseed)
    fit <- train(Type ~ ., data = training(),
                 method=input$method,tuneLength = 10, preProcess=preprocess.methods,
                 trControl=trainControl(method = input$resample, number = 10,
                                        repeats= 3,sampling = input$sampling))
    fit
    
    
  })
  
  
  val2<- reactive({
    OutOfSample  <- predict(val(), newdata=validation())
    confusion <- confusionMatrix(validation()$Type, OutOfSample)
    confusion
  })
  
  
  
  
  
  output$data1 <- renderDataTable({
    values$df_data1
  },
  filter='top',
  rownames=FALSE)
  
  output$fit<- renderPrint({
    val()
  })
  
  output$confusion<- renderPrint({
    val2()
  })
  
  output$dotplot <- renderPlot({
    dotPlot(varImp(val()), main="Dotplot of variable importance values")
  })
  
  
  
  observeEvent(input$button11, {
    toggle(selector = "#navbar li a[data-value=mytab2]")
  })
}
shinyApp(ui = ui, server = server)