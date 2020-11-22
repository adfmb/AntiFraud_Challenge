# camposllave<-c("atm","Division","Giro","Estado","Ciudad","CP","Del.Muni","Colonia","Latitud","Longitud","cvemun")
# camposvariables<-seleccionar_vars(df,camposllave,numcols = 3)
# campos<-camposvariables$campos
# numcampos<-length(campos)
# set.seed(numcampos)
# pesos_random<-rnorm(numcampos,50,20)
# input<-as.list(pesos_random)
# names(input)<-paste0("input_",campos)



def_aporte_var<-function(camposvariables,input=input,catalogo_direccion_vars=NULL){
  if(is.null(catalogo_direccion_vars)){
    catalogo_direccion_vars<-readRDS("data/catalogo_vars_positivas.rds")
  }
  
  df_campos_direccion<-data_frame(
    variable=camposvariables$campos
  )%>%
    left_join(
      catalogo_direccion_vars%>%
        select(Variable,direccion_variable)%>%
        rename(variable=Variable)
    )
  
  df_campos_pesos<-data_frame()
  for(campotmp in campos){
    df_campos_pesos<-df_campos_pesos%>%
      bind_rows(
        data_frame("variable"=campotmp,"peso"=input[[paste0("input_",campotmp)]])
      )
  }
  
  df_campos<-df_campos_direccion%>%
    left_join(
      df_campos_pesos
    )%>%
    mutate(
      aporte_var=direccion_variable*peso
    )
  
  return(df_campos)
  
}


