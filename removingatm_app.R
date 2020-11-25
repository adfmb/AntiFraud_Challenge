library(shinydashboard)
library(shiny)
library(leaflet)
library(leaflet.minicharts)
library(DT)
source("utils/creando_mapas.R")
source("utils/seleccionar_vars.R")
source("utils/def_aporte_var.R")
source("utils/df_calif_atms.R")
source("utils/proceso_atms.R")
source("utils/proceso_clusters.R")
source("utils/mapas_clusters.R")
source("utils/enlistar_edos_cluster.R")
source("utils/filtrar_atms_cluster_edo.R")

par(mar=c(1,1,1,1))
catalogo_direccion_vars<-readRDS("datasource/catalogo_vars_positivas.rds")
df_nas<-readRDS("datasource/df_nas.rds")
# catalogo_campos_escalados<-data_frame("variable_esc"=camposvariables$campos)%>%
#   left_join(
#     df_nas%>%
#       select(variable_esc,variable)
#   )

# camposllave<-c( "atm","Division","Giro","Estado","Ciudad","CP","Del.Muni","Colonia","Latitud","Longitud","cvemun")
camposllave<-c( "atm","Division","Giro","Estado","Ciudad","CP",#"Del.Muni","Del/Muni",
                "Municipio","Colonia","Latitud","Longitud","cvemun")
vars_excluir<-NULL
numcols_sliders00<-3
## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file", label = h3("Cargar base ATM's")),
    p(),
    checkboxGroupInput("checkGroup", label = h3("Niveles a incluir"), 
                       choices = list("Nacional" = 1, "Estatal" = 2, "Municipal" = 3),
                       selected = 1),
    menuItem("Pesos variables", tabName = "pesos_variables", icon = icon("dashboard")),
    p(),
    actionButton("calificar_atms", "Calificar ATM's"),
    p(),
    p(),
    p(),
    menuItem("Aporte & Calificación", icon = icon("th"), tabName = "aporte_y_calif_atms"#,badgeLabel = "new", badgeColor = "green"
    ),
    numericInput('numclusters', 'Número de clústers', 3, min = 1, max = 9, step = 0.5),
    p(),
    menuItem("Semáforo de ATM's", icon = icon("th"), tabName = "df_clusters_atms", badgeLabel = "new", badgeColor = "green"),
    # actionButton("clustering_atms", "ATM's"),
    
    menuItem("Peores ATM's", icon = icon("th"), tabName = "peores_atms"),
    
    
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
            fluidRow(
              # column(width = 3,
                h2("Resumen x clúster"),
                # DTOutput('df_clusters_atms'),
                dataTableOutput('df_clusters_atms')
                # )
              ),
            # p(),
            fluidRow(
              # column(width = 8,
                     h2("Clústers en Mapa"),
                     leafletOutput("leafl_colors_kmeans")
              # )
            )
            
    ),
    tabItem(tabName = "peores_atms",
            fluidRow(
              column(3, 
                        uiOutput('radiobutton_cluster01')
                     ),
              column(3,
                     uiOutput('select_edos_cluster01')
              #        ),
              # column(5,
              #        dataTableOutput('df_peores_atms')
                     )
              ),
            fluidRow(
              leafletOutput("leafl_peores_atms")
              ),
            fluidRow(
              dataTableOutput('df_peores_atms')
              )
            )#,
    
    # tabItem(tabName = "leafl_distancia_cajeros",
    #         h2("Mapa Clústers"),
    #         leafletOutput("leafl_distancia_cajeros")
    # ),
    # 
    # tabItem(tabName = "leafl_wcolor_labls",
    #         h2("Mapa puntos"),
    #         leafletOutput("leafl_wcolor_labls")
    # )
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
  
  camposvariables<- eventReactive(req(input$file, input$checkGroup), {
    seleccionar_vars(reac_df(),camposllave = camposllave,vars_excluir = vars_excluir,numcols=numcols_sliders00,niveles_vars = input$checkGroup,df_nas=df_nas)
  })
  
  
  atms <- eventReactive(req(input$calificar_atms, input$checkGroup), {
    proceso_atms(reac_df(),camposllave,camposvariables(),input=input,catalogo_direccion_vars=catalogo_direccion_vars,df_nas=df_nas)
  })
  
  
  # df_campos <- eventReactive(input$calificar_atms, {
  #   def_aporte_var(camposvariables(),input,catalogo_direccion_vars=catalogo_direccion_vars)
  # })
  
  clusters <- eventReactive(req(input$calificar_atms, input$numclusters, input$checkGroup),{
    proceso_clusters(atms()$df_calif,input)
  })
  
  # observe({
  #   req(input$calificar_atms, input$numclusters)
  #   clusters()
  # })
  
  # mapas <- eventReactive(input$file, {
  #   creando_mapas(reac_df())
  # })
  
  estados_cluster<- eventReactive(input$radio01,{
    enlistar_edos_cluster(clusters()$df_cluster,input$radio01)
  })
  
  atms_estados_cluster<- eventReactive(input$select_edos,{
    filtrar_atms_cluster_edo(clusters()$df_cluster,reac_df(),input$radio01,
                             input$select_edos,estados_cluster()$df_estados_cluster,camposllave)
  })
  
  mapa_cluster_peores_atm <- eventReactive(input$select_edos,{
    mapas_clusters(atms_estados_cluster()$bd_edo_clust)
  })
  
  # output$value_edos_clust <- renderPrint({ estados_cluster() })
  
  output$df_aporte_vars = renderDT(
    atms()$df_campos
  )
  
  output$df_calif_atms = renderDT(
    atms()$df_calif
  )
  
  output$df_clusters_atms = DT::renderDataTable(#renderDT(
    DT::datatable(clusters()$df_clusters%>%
      group_by(clusters_kmeans,orden_cluster_kmenas_calif,orden_cluster_kmenas_califgiro)%>%
      summarise(
        casos=n(),
        calificacion_promedio_kmeans=mean(calif)
        ), 
      options = list(lengthMenu = c(2,3,6), pageLength = 3)
    )
  )
  
  output$leafl_colors_kmeans <- renderLeaflet({
    clusters()$lista_mapas_clusters$mapa_colors_kmeans
    
  })
  
  output$leafl_peores_atms <- renderLeaflet({
    mapa_cluster_peores_atm()$mapa_distancia_cajeros
    
  })
  
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
  
  
  output$radiobutton_cluster01 <- renderUI({
    radioButtons("radio01", label = h3("Elija Clúster a Analizar"),
                 choices = clusters()$lista_clusters, 
                 selected = 1)
  })
  
  
  output$select_edos_cluster01 <- renderUI({
    selectInput("select_edos", label = h3("Elija Estado"), 
                choices = estados_cluster()$lista_edos_cluster, 
                selected = 1)
  })
  
  
  output$df_peores_atms <-DT::renderDataTable(
    
    DT::datatable(atms_estados_cluster()$show_bd_edo_clust, 
                  options = list(lengthMenu = c(2,3,6), pageLength = 3)
    )
    
  )
  
  
  
  # output$leafl_distancia_cajeros <- renderLeaflet({
  #   mapas()$leafl_distancia_cajeros
  #   
  # })
  # 
  # output$leafl_wcolor_labls <- renderLeaflet({
  #   mapas()$leafl_wcolor_labls
  #   
  # })
}

shinyApp(ui, server)