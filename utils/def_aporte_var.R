# df<-readRDS("data/bd_z_rel_00.rds")
# df_nas<-readRDS("data/df_nas.rds")
# camposllave<-c("atm","Division","Giro","Estado","Ciudad","CP","Del.Muni","Del/Muni","Colonia","Latitud","Longitud","cvemun")
# camposvariables<-seleccionar_vars(df,camposllave,numcols = 3)
# catalogo_campos_escalados<-data_frame("variable_esc"=camposvariables$campos_esc)%>%
#   left_join(
#     df_nas%>%
#       select(variable_esc,variable)
#   )#
# campos<-catalogo_campos_escalados%>%
#   distinct(variable)%>%
#   pull(variable)
#   
# 
# campos<-camposvariables$campos
# numcampos<-length(campos)
# set.seed(numcampos)
# pesos_random<-rnorm(numcampos,50,20)
# input<-as.list(pesos_random)
# names(input)<-paste0("input_",campos)



def_aporte_var<-function(camposvariables,input=input,catalogo_direccion_vars=NULL,df_nas=NULL){
  if(is.null(catalogo_direccion_vars)){
    catalogo_direccion_vars<-readRDS("datasource/catalogo_vars_positivas.rds")
  }
  if(is.null(df_nas)){
    df_nas<-readRDS("datasource/df_nas.rds")
     } 
    catalogo_campos_escalados<-data_frame("variable_esc"=camposvariables$campos_esc)%>%
      left_join(
        df_nas%>%
          select(variable_esc,agrupacion,variable)
      )

  
  # df_campos_direccion<-data_frame(
  #   variable=camposvariables$campos
  # )%>%
  #   left_join(
  #     catalogo_direccion_vars%>%
  #       select(Variable,direccion_variable)%>%
  #       rename(variable=Variable)
  #   )
  
  df_campos_direccion<-catalogo_campos_escalados%>%
    left_join(
      catalogo_direccion_vars%>%
        select(Variable,direccion_variable)%>%
        rename(variable=Variable)
    )
  
  df_campos_pesos<-data_frame()
  # for(campoesctmp in df_campos_direccion$variable){
  #   df_campos_pesos<-df_campos_pesos%>%
  #     bind_rows(
  #       data_frame("variable_esc"=campotmp,"peso"=input[[paste0("input_",campotmp)]])
  #     )
  # }
  for(campoesctmp in df_campos_direccion$variable_esc){
    # campoesctmp <- df_campos_direccion$variable[1]
    campotmp<-df_campos_direccion%>%
      filter(variable_esc==campoesctmp)%>%
      pull(variable)
    
    df_campos_pesos<-df_campos_pesos%>%
      bind_rows(
        data_frame("variable_esc"=campoesctmp,"peso"=input[[paste0("input_",campotmp)]])
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


