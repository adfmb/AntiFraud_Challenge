library(shiny)
library(leaflet)
source("utils/creando_mapas.R")
source("utils/seleccionar_vars.R")

# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()

# ui <- fluidPage(
#   leafletOutput("mymap"),
#   p(),
#   actionButton("recalc", "New points")
# )

camposllave<-c( "atm","Division","Giro","Estado","Ciudad","CP","Del.Muni","Colonia","Latitud","Longitud","cvemun")
vars_excluir<-NULL
ui <- fluidPage(
  
  titlePanel("Mapas"),
  
  sidebarLayout(
    
    sidebarPanel(
      # Inputs excluded for brevity
      # actionButton("recalc", "New points"),
      # actionButton("recalc_mapas", "New mapas"),
      fileInput("file", label = h3("Cargar base ATM's")),
      
      # Copy the line below to make a slider bar 
      
      # # create some select inputs
      # lapply(1:5, function(i) {
      #   selectInput(paste0('a', i), paste0('SelectA', i),
      #               choices = sample(LETTERS, 5))
      # })
      
      
      uiOutput('my_inputs')
      
    ),
    
    mainPanel(
      tabsetPanel(
        # tabPanel("Mapa", p(), leafletOutput("mymap")), 
        tabPanel("Con Cluster por Distancias", leafletOutput("leafl_distancia_cajeros")),
        tabPanel("Con marcas de colores", leafletOutput("leafl_wcolor_labls"))
        # tabPanel("Table", tableOutput("table"))
      )
    )
  )
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
