df_calif_atms<-function(df,df_campos,camposllave){
  df_00<-df%>%
    select(one_of(c(camposllave,df_campos$variable)))
  df_calif<-df_00%>%
    mutate(
      calif=as.numeric(t(as.matrix(select(.,one_of(df_campos$variable)))%*%matrix(df_campos$aporte_var,nrow = nrow(df_campos))))
    )%>%
    select(atm,calif)%>%
    left_join(
      df
      )%>%
    select(calif,one_of(camposllave),dplyr::starts_with("Giro_"))
  
  # length(unique(df$atm))
  # nrow(df)
  return(df_calif)
}