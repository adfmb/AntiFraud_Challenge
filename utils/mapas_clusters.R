library(leaflet)
library('sp')
library(dplyr)
myCRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


mapas_clusters<-function(df_clusters){
  df_clusters$Latitud <- as.numeric(df_clusters$Latitud)
  df_clusters$Longitud <- as.numeric(df_clusters$Longitud)
  
  df_cord = df_clusters%>%
    select('Longitud','Latitud')
  
  df_sp_points=SpatialPointsDataFrame(coords=df_cord,
                                      data=df_clusters,
                                      proj4string = myCRS)
  
  # pal <- colorFactor(c("green", "red"), domain = c("green", "red"))
  # pal <- colorNumeric(c("red", "green", "blue"), domain = c("red", "green", "blue"))
  # pal <- colorFactor(c("green", "red"), domain = c("green", "red"))
  # RColorBrewer::display.brewer.all()
  pal <- colorFactor(
    palette = 'RdYlBu',
    domain = df_sp_points$orden_cluster_kmenas_calif
  )
  
  
  
  leafl_wcolor_labls<-leaflet() %>% 
    addTiles() %>%
    addCircles(data= df_sp_points,
               weight = 1,
               radius = abs(df_sp_points$prom_kmeans_calif/1000000000), # trabajar Radio
               color = ~pal(orden_cluster_kmenas_calif),
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
  
  return(list("mapa_colors_kmeans"=leafl_wcolor_labls))
  
  
}

# pal <- colorNumeric(c("red", "green", "blue"))
# previewColors(pal, 1:10)
