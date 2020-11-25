enlistar_edos_cluster<-function(df_clusters,id_cluster){
  estados_cluster<-df_clusters%>%
    filter(orden_cluster_kmenas_calif==id_cluster)%>%
    distinct(Estado)%>%
    arrange(Estado)%>%
    pull(Estado)
  
  lista_edos_cluster<-list()
  for(id_edo in 1:length(estados_cluster)){
    lista_edos_cluster[[paste0(estados_cluster[id_edo])]]<-id_edo
  }
  
  df_estados_cluster<-data_frame("estados_cluster"=estados_cluster,"id_estados_cluster"=1:length(estados_cluster))
  return(list("df_estados_cluster"=df_estados_cluster,"lista_edos_cluster"=lista_edos_cluster))
}