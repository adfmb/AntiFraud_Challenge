proceso_atms<-function(df,camposllave,camposvariables,input=input,catalogo_direccion_vars=catalogo_direccion_vars){
  
  df_campos<-def_aporte_var(camposvariables,input=input,catalogo_direccion_vars=catalogo_direccion_vars)
  
  df_calif<-df_calif_atms(df,df_campos,camposllave)
  
  return(list("df_campos"=df_campos,"df_calif"=df_calif))
  
}