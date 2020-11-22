library(shinydashboard)
library(shiny)
library(leaflet)
library(DT)
source("utils/creando_mapas.R")
source("utils/seleccionar_vars.R")
source("utils/def_aporte_var.R")
source("utils/df_calif_atms.R")
source("utils/proceso_atms.R")
par(mar=c(1,1,1,1))
catalogo_direccion_vars<-readRDS("datasource/catalogo_vars_positivas.rds")

camposllave<-c( "atm","Division","Giro","Estado","Ciudad","CP","Del.Muni","Colonia","Latitud","Longitud","cvemun")
vars_excluir<-NULL
numcols_sliders00<-3
## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file", label = h3("Cargar base ATM's")),
    menuItem("Pesos variables", tabName = "pesos_variables", icon = icon("dashboard")),
    p(),
    actionButton("calificar_atms", "Calificar ATM's"),
    p(),
    p(),
    p(),
    menuItem("Aporte & Calificación", icon = icon("th"), tabName = "aporte_y_calif_atms"#,badgeLabel = "new", badgeColor = "green"
    ),
    numericInput('clusters', 'Número de clústers', 3, min = 1, max = 9),
    p(),
    menuItem("Grupos de ATM's", icon = icon("th"), tabName = "df_clusters_atms"),
    # actionButton("clustering_atms", " ATM's"),
    
    
    menuItem("Con Cluster por Distancias", icon = icon("th"), tabName = "leafl_distancia_cajeros"#,badgeLabel = "new", badgeColor = "green"
    ),
    menuItem("Con marcas de colores", icon = icon("th"), tabName = "leafl_wcolor_labls"#, badgeLabel = "new", badgeColor = "green"
             )
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pesos_variables",
            # h2("Dashboard tab content"),
            fluidRow(
              column(width = 4,
                     uiOutput('my_inputs01')
              ),

              column(width = 4,
                     uiOutput('my_inputs02')
              ),

              column(width = 4,
                     uiOutput('my_inputs03')
              )
            )
            # uiOutput('my_inputs')
    ),
    
    tabItem(tabName = "aporte_y_calif_atms",
            h2("Aporte por variable"),
            DTOutput('df_aporte_vars'),
            p(),
            h2("Calificación de ATM's"),
            DTOutput('df_calif_atms')
    ),
    
    tabItem(tabName = "df_clusters_atms",
            h2("Semáforo de ATM's"),
            DTOutput('df_clusters_atms')
    ),
    
    tabItem(tabName = "leafl_distancia_cajeros",
            h2("Mapa Clústers"),
            leafletOutput("leafl_distancia_cajeros")
    ),
    
    tabItem(tabName = "leafl_wcolor_labls",
            h2("Mapa puntos"),
            leafletOutput("leafl_wcolor_labls")
    )
  )
)

# Put them together into a dashboardPage
ui<-dashboardPage(
  dashboardHeader(title = "BBVA's Antifraud Challenge"),
  sidebar,
  body
)

server <- function(input, output, session) {
  
  reac_df<- eventReactive(input$file, {
    df<-readRDS(input$file$datapath)
  })
  
  camposvariables<- eventReactive(input$file, {
    seleccionar_vars(reac_df(),camposllave = camposllave,vars_excluir = vars_excluir,numcols=numcols_sliders00)
  })
  
  
  atms <- eventReactive(input$calificar_atms, {
    proceso_atms(reac_df(),camposllave,camposvariables(),input=input,catalogo_direccion_vars=catalogo_direccion_vars)
  })
  
  # df_campos <- eventReactive(input$calificar_atms, {
  #   def_aporte_var(camposvariables(),input,catalogo_direccion_vars=catalogo_direccion_vars)
  # })
  
  # clusters <- reactive({
  #   kmeans(selectedData(), input$clusters)
  # })
  
  mapas <- eventReactive(input$file, {
    creando_mapas(reac_df())
  })
  
  output$df_aporte_vars = renderDT(
    atms()$df_campos
  )
  
  output$df_calif_atms = renderDT(
    atms()$df_calif
  )
  
  output$df_clusters_atms = renderDT(
    atms()$df_clusters%>%
      group_by(clusters_kmeans,orden_cluster_kmenas_calif,orden_cluster_kmenas_califgiro)%>%
      summarise(
        casos=n(),
        calificacion_promedio_kmeans=mean(calif)
        )
  )
  
  output$my_inputs01 <- renderUI({
    lapply(camposvariables()$campos[1:camposvariables()$vec_nums[1]], function(x){
      sliderInput(paste0('input_',x), label = paste0(x), min = 0, 
                  max = 100, value = 1)
    })
  })
  
  output$my_inputs02 <- renderUI({
    lapply(camposvariables()$campos[(sum(camposvariables()$vec_nums[1:1]) + 1):(sum(camposvariables()$vec_nums[1:2]))], function(x){
      sliderInput(paste0('input_',x), label = paste0(x), min = 0, 
                  max = 100, value = 1)
    })
  })
  
  
  
  output$my_inputs03 <- renderUI({
    lapply(camposvariables()$campos[(sum(camposvariables()$vec_nums[1:2]) + 1):(sum(camposvariables()$vec_nums[1:3]))], function(x){
      sliderInput(paste0('input_',x), label = paste0(x), min = 0, 
                  max = 100, value = 1)
    })
  })
  
  output$leafl_distancia_cajeros <- renderLeaflet({
    mapas()$leafl_distancia_cajeros
    
  })
  
  output$leafl_wcolor_labls <- renderLeaflet({
    mapas()$leafl_wcolor_labls
    
  })
}

shinyApp(ui, server)