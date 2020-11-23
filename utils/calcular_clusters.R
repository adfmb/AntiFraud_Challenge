calcular_clusters<-function(df_calif,input){
  set.seed(input$numclusters)
  df_clusters<-df_calif%>%
  # df_clusters<-atms$df_calif%>%
    mutate(
      clusters_kmeans=kmeans(select(.,calif,starts_with("Giro_")),input$numclusters)$cluster
    )%>%
    group_by(Giro,clusters_kmeans)%>%
    mutate(
      prom_kmeans_califgiro=mean(calif)#,
      # orden_cluster_kmenas_califgiro
    )%>%
    ungroup()%>%
    group_by(Giro)%>%
    mutate(
      orden_cluster_kmenas_califgiro=as.numeric(as.factor(prom_kmeans_califgiro))
    )%>%
    ungroup()%>%
    group_by(clusters_kmeans)%>%
    mutate(
      prom_kmeans_calif=mean(calif)
    )%>%
    ungroup()%>%
    mutate(
      orden_cluster_kmenas_calif=as.numeric(as.factor(prom_kmeans_calif))
    )%>%
    select(atm,calif,clusters_kmeans,starts_with("orden_cluster_"),one_of(names(.)))
  
  return(df_clusters)
}

