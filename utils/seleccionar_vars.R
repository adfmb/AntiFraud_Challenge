seleccionar_vars<-function(df,camposllave,vars_excluir=NULL,numcols=3){
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
  
  campos<-campos03[!grepl("Giro_",campos03)]
  
  num_campos03<-length(campos)
  vec_nums<-c()
  for(i in 1:numcols){
    # i<-3
    numer<-ifelse(length(vec_nums)==0,num_campos03,num_campos03-sum(vec_nums))
    denom<-numcols-length(vec_nums)
    num_i<-ifelse(i<numcols,ceiling(numer/denom),numer)
    vec_nums<-c(vec_nums,num_i)
  }
  

    
  return(list("campos"=campos,"vec_nums"=vec_nums))
  
}

# camposllave<-c("atm","Division","Giro","Estado","Ciudad","CP","Del.Muni","Colonia","Latitud","Longitud","cvemun")
# camposvariables<-seleccionar_vars(df,camposllave,numcols = 3)
