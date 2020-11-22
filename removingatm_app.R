library(shinydashboard)
## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file", label = h3("Cargar base ATM's")),
    menuItem("Pesos variables", tabName = "pesos_variables", icon = icon("dashboard")),
    menuItem("Con Cluster por Distancias", icon = icon("th"), tabName = "leafl_distancia_cajeros",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Con marcas de colores", icon = icon("th"), tabName = "leafl_wcolor_labls",
             badgeLabel = "new", badgeColor = "green")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pesos_variables",
            h2("Dashboard tab content"),
            uiOutput('my_inputs')
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
    seleccionar_vars(reac_df(),camposllave = camposllave,vars_excluir = vars_excluir)
  })
  
  mapas <- eventReactive(input$file, {
    creando_mapas(reac_df())
  })
  
  
  output$my_inputs <- renderUI({
    lapply(camposvariables(), function(x){
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