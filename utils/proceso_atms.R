source("utils/def_aporte_var.R")
source("utils/df_calif_atms.R")
# source("utils/calcular_clusters.R")
proceso_atms<-function(df,camposllave,camposvariables,input=input,catalogo_direccion_vars=catalogo_direccion_vars){
  
  df_campos<-def_aporte_var(camposvariables,input=input,catalogo_direccion_vars=catalogo_direccion_vars)
  
  df_calif<-df_calif_atms(df,df_campos,camposllave)
  
  # df_clusters<-calcular_clusters(df_calif,input)
  
  return(list("df_campos"=df_campos,"df_calif"=df_calif))#,"df_clusters"=df_clusters))
  
}