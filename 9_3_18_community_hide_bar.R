library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)
options(max.print = 99999999)

ui <- tagList(
  useShinyjs(),
  
  navbarPage(
    "Community detection evaluation", theme = shinytheme("united"),
    id = "navbar",
    tabPanel(
      title = "upload data",
      sidebarPanel(
        fileInput("file1", "Upload element file"),
        br(),
        fileInput("file2","upload relation file"),
        actionButton("button", "Combine the data"),
        br(),
        HTML("<hr>"),
      actionButton("button1", "Proceed to Community evaluation")
      ),
      mainPanel(tabsetPanel(tabPanel("elements",dataTableOutput("data1")), 
                            tabPanel("relations",dataTableOutput("data2")),
                            tabPanel("Combined data",dataTableOutput("combined"))
    ))
    ),
    
    tabPanel(
      title = "Community Evaluat",
      value = "mytab2",
      sidebarLayout(
        sidebarPanel(
          selectInput("algorithm", "Select algorithm", c("edge betweeness", 
                                                         "infomap",
                                                         "Label propagation",
                                                         "Leading eigen",
                                                         "spinglass",
                                                         "walktrap")),
          br(),
          actionButton("button2", "Proceed")
        ),
        mainPanel(tabsetPanel(tabPanel("community detection",verbatimTextOutput("community")), 
                              tabPanel("Modularity Score",verbatimTextOutput("modularity")),
                              tabPanel("Membership",dataTableOutput("membership"))
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
  
  observeEvent(input$file1, {
    values$df_data1 <- read.csv(input$file1$datapath)
  })
  
  values2<- reactiveValues(df_data2 = NULL)
  
  observeEvent(input$file2, {
    values2$df_data2 <- read.csv(input$file2$datapath)
  })
  
  val<-eventReactive(input$button,{
    h<- values$df_data1[match(values2$df_data2$Source, values$df_data1$ID), 3, drop=F]
    g<- values$df_data1[match(values2$df_data2$Target, values$df_data1$ID), 3, drop=F] 
    j<-cbind(h, g)
    #change column names
    colnames(j)[1] <- "new_source"
    colnames(j)[2] <- "new_target"
    
    
    
    #combine with the relation table
    dataset<-cbind(values2$df_data2, j)
    dataset$Source<-NULL
    dataset$Target<-NULL
    dataset
    
  })
  
  ma=reactive({
    g<- graph_from_data_frame(val()[,5:6], directed = TRUE)
    g
    
  })
  
  val2<- eventReactive(input$button2,{
    switch(input$algorithm,
           "edge betweeness" = cluster_edge_betweenness(ma()),
           "infomap" = cluster_infomap(ma()),
           "Label propagation"=cluster_label_prop(ma()),
           "Leading eigen"=cluster_leading_eigen(ma()),
           "spinglass"=cluster_spinglass(ma()),
           "walktrap"=cluster_walktrap(ma())
    )
    
  })
  
  
  
  
  output$data1 <- renderDataTable({
    values$df_data1
  },
  filter='top',
  rownames=FALSE)
  
  output$data2 <- renderDataTable({
    values2$df_data2
  },
  filter='top',
  rownames=FALSE)
  
  output$combined<- renderDataTable({
    val()
  },
  filter='top',
  rownames=FALSE)
  
  output$community <- renderPrint({
    dataset <- val2()
    communities(dataset)
  })
  
  output$modularity <- renderPrint({
    dataset <- val2()
    print(paste0("Modularity score is: ", modularity(dataset)))
    #modularity(dataset)
  })  
  output$membership <- renderDataTable({
    dataset <- val2()
    #convert communities to data frame
    com <- cbind(V(ma())$name,dataset$membership)
  },
  filter='top',
  rownames=FALSE)
  
  output$testOut <- renderText({
    paste("Your choice is", input$test)
  })
  observeEvent(input$button1, {
    toggle(selector = "#navbar li a[data-value=mytab2]")
  })
}
shinyApp(ui = ui, server = server)