library(leaflet)
library('sp')
library(dplyr)

#Corrdinate reference system
#proj4 (recomended)
myCRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


creando_mapas<-function(df){
  df$Latitud <- as.numeric(df$Latitud)
  df$Longitud <- as.numeric(df$Longitud)
  
  ## Crear un SpatialPointsDataFrame 
  #https://signatura21.wordpress.com/2016/10/19/crear-un-spatialpointsdataframe-con-r/
  
  #coordinate
  
  df_cord = df%>%
    select('Longitud','Latitud')
  
  #--------------------------------------
  #built SpatialPointsDataFrame
  #--------------------------------------
  
  df_sp_points=SpatialPointsDataFrame(coords=df_cord,
                                      data=df,
                                      proj4string = myCRS)
  
  plot01<-plot(df_sp_points, axes=TRUE)
  
  #--------------------------------------
  # Manipulación de la data
  #--------------------------------------
  
  df_sp_points$type <- ifelse( df_sp_points$rentabilidad >0, 
                               'green',
                               'red')
  
  
  ## Agrupa por distancia entre cajeros
  leafl_distancia_cajeros<-leaflet(df_sp_points) %>% addTiles() %>%
    addMarkers(clusterOptions = markerClusterOptions())
  
  pal <- colorFactor(c("green", "red"), domain = c("green", "red"))
  
  
  leafl_wcolor_labls<-leaflet() %>% 
    addTiles() %>%
    addCircles(data= df_sp_points,
               weight = 1,
               radius = abs(df_sp_points$ataques_mpio_banca*100), # trabajar Radio
               color = ~pal(type),
               opacity = 0.7,
               fillOpacity = 0.7,
               popup = ~ sprintf("ATM:%s<br/> Del: %s<br/> Est: %s<br/>
                                Score: %s<br/>
                               ",
                                 df_sp_points$atm,
                                 df_sp_points$`Del/Muni`,
                                 df_sp_points$Estado,
                                 df_sp_points$rentabilidad
               )
               
    )

  return(list("plot01"=plot01,"leafl_distancia_cajeros"=leafl_distancia_cajeros,"leafl_wcolor_labls"=leafl_wcolor_labls))
  
  
}