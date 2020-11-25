source("utils/calcular_clusters.R")
source("utils/mapas_clusters.R")
proceso_clusters<-function(df_calif,input=input){
  
  lista_clusters<-list()
  for(numclus in 1:input$numclusters){
    lista_clusters[[paste0("Clúster ",numclus)]]<-numclus
  }
  
  df_clusters<-calcular_clusters(df_calif,input)
  
  lista_mapas_clusters<-mapas_clusters(df_clusters)
   
  return(list("df_clusters"=df_clusters,"lista_mapas_clusters"=lista_mapas_clusters,"lista_clusters"=lista_clusters))
  
}