source("utils/calcular_clusters.R")
proceso_clusters<-function(df_calif,input=input){
  
   df_clusters<-calcular_clusters(df_calif,input)
  
  return(list("df_clusters"=df_clusters))
  
}