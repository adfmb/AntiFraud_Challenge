# df_niveles_vars<-data_frame("niveles_vars"=c("1","2","3"),"niveles"=c("Nacional","Estatal","Municial"))
seleccionar_vars<-function(df,camposllave,vars_excluir=NULL,numcols=3,niveles_vars,df_nas=NULL){
  if(is.null(df_nas)){
    df_nas<-readRDS("datasource/df_nas.rds")
  }
  niveles_vars<-as.numeric(as.character(niveles_vars))
  if(!1%in%niveles_vars){
    # vars_excluir<-c(vars_excluir,grepl("z_cve_",names(df)))
    vars_excluir<-c(vars_excluir,df_nas%>%
                      filter(agrupacion=="nacional" & !variable%in%
                               c("indisponibilidad","ataques_mpio_banca","ataques_edo_banca",
                                 "delitos_mpio_secretariado","delitos_edo_secretariado","ataques_prom_anio")
                      )%>%
                      pull(variable_esc)
    )
  }
  if(!2%in%niveles_vars){
    # vars_excluir<-c(vars_excluir,grepl("z_est_",names(df)))
    vars_excluir<-c(vars_excluir,df_nas%>%
                      filter(agrupacion=="estado" & !variable%in%
                               c("indisponibilidad","ataques_mpio_banca","ataques_edo_banca",
                                 "delitos_mpio_secretariado","delitos_edo_secretariado","ataques_prom_anio")
                             )%>%
                      pull(variable_esc)
    )
  }
  if(!3%in%niveles_vars){
    # vars_excluir<-c(vars_excluir,grepl("z_cve_",names(df)))
    vars_excluir<-c(vars_excluir,df_nas%>%
                      filter(agrupacion=="municipio")%>%
                      pull(variable_esc)
    )
  }
  # if(niveles_vars==3){
  #   vars_excluir<-c(vars_excluir,grepl("z_est_",names(df)))
  #   vars_excluir<-c(vars_excluir,grepl("z_cve_",names(df)))
  # }
  campos01<-names(df)[!names(df)%in%camposllave]
  campos02<-campos01[!campos01%in%vars_excluir]
  campos03<-campos02[!grepl("_p_20",campos02)]
  campos03<-campos03[!grepl("_p_30",campos03)]
  campos03<-campos03[!grepl("_p_40",campos03)]
  campos03<-campos03[!grepl("_p_60",campos03)]
  campos03<-campos03[!grepl("_p_70",campos03)]
  campos03<-campos03[!grepl("_p_80",campos03)]
  campos03<-campos03[!grepl("_p_90",campos03)]
  campos03<-campos03[!grepl("_p_96",campos03)]
  campos03<-campos03[!grepl("_p_97",campos03)]
  campos03<-campos03[!grepl("_p_98",campos03)]
  campos03<-campos03[!grepl("_p_99",campos03)]
  campos03<-campos03[!grepl("_p_100",campos03)]
  campos03<-campos03[!grepl("Giro_",campos03)]
  
  
  campos03<-campos03[!grepl("orig_",campos03)]
  
  
  df_campos_esc<-data_frame("variable_esc"=campos03)%>%
    left_join(
      df_nas%>%
        select(variable_esc,variable)
    )
  
  # campos<-campos03[!grepl("Giro_",campos03)]
  campos<-df_campos_esc%>%
    distinct(variable)%>%
    pull(variable)
  num_campos03<-length(campos)

  vec_nums<-c()
  for(i in 1:numcols){
    # i<-3
    numer<-ifelse(length(vec_nums)==0,num_campos03,num_campos03-sum(vec_nums))
    denom<-numcols-length(vec_nums)
    num_i<-ifelse(i<numcols,ceiling(numer/denom),numer)
    vec_nums<-c(vec_nums,num_i)
  }
  

    
  return(list("campos_esc"=campos03,"campos"=campos,"vec_nums"=vec_nums))
  
}

# df<-readRDS("data/bd_z_rel_02a.rds")
# # camposllave<-c("atm","Division","Giro","Estado","Ciudad","CP","Del/Muni","Colonia","Latitud","Longitud","cvemun")
# camposllave<-c( "atm","Division","Giro","Estado","Ciudad","CP","Municipio","Colonia","Latitud","Longitud","cvemun")
# camposvariables<-seleccionar_vars(df,camposllave,numcols = 3,niveles_vars = 1)
# camposvariables$campos_esc
# camposvariables$campos












