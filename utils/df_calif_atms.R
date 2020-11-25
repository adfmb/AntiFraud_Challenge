# df<-readRDS("data/bd_z_rel_01.rds")
# df_campos
# camposllave
df_calif_atms<-function(df,df_campos,camposllave){
  df_00<-df%>%
    # select(one_of(c(camposllave,df_campos$variable)))
    select(one_of(c(camposllave,df_campos$variable_esc)))
  
  # sum(is.na(df_00))
  df_calif<-df_00%>%
    # mutate_at(vars(df_campos$variable),function(x){x[is.na(x)]<-0; return(x)}) %>%# sum(is.na(df_calif[df_campos$variable]))
    mutate_at(vars(df_campos$variable_esc),function(x){x[is.na(x)]<-0; return(x)}) %>%# sum(is.na(df_calif[df_campos$variable]))
    mutate(
      # calif=as.numeric(t(as.matrix(select(.,one_of(df_campos$variable)))%*%matrix(df_campos$aporte_var,nrow = nrow(df_campos))))
      calif=as.numeric(t(as.matrix(select(.,one_of(df_campos$variable_esc)))%*%matrix(df_campos$aporte_var,nrow = nrow(df_campos))))
    )%>%
    select(atm,calif)%>% # sum(is.na(df_calif))
    left_join(
      df
      )%>%
    select(calif,one_of(camposllave),dplyr::starts_with("Giro_"))
  
  # length(unique(df$atm))
  # nrow(df)
  return(df_calif)
}