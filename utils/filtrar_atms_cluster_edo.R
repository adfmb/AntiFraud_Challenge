# id_cluster<-1
# estado<-"AGUASCALIENTES"
# df<-readRDS("data/bd_z_rel_02a.rds")
filtrar_atms_cluster_edo<-function(df_clusters,df,id_cluster,id_estado,df_estados_cluster,camposllave){
  estado<-df_estados_cluster%>%
    filter(id_estados_cluster==id_estado)%>%
    pull(estados_cluster)
  
  bd_edo_clust<-df_clusters%>%
    # rename(Municipio=Del.Muni)%>%
    filter(orden_cluster_kmenas_calif==id_cluster & Estado==estado)%>%
    # select(-dplyr::starts_with("z_"))%>%
    select(orden_cluster_kmenas_calif,Latitud,Longitud,calif,atm,Division,Giro,Ciudad,Municipio)%>%#,dplyr::starts_with("orig_"))%>%
    left_join(
      df%>%
        select(- dplyr::starts_with("z_"), - dplyr::starts_with("Giro_"), -one_of(camposllave[!camposllave%in%c("atm")]))
    )%>%
    arrange(calif)
  
  names(bd_edo_clust)<-gsub("orig_","",names(bd_edo_clust))
  
  return(
    list("bd_edo_clust"=bd_edo_clust,
         "show_bd_edo_clust"=bd_edo_clust%>%
                              select(-orden_cluster_kmenas_calif,-Latitud,-Longitud)%>%
                              rename(bondad_atm=calif)
         )
    )
}