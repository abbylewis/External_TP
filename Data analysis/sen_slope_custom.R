sen_slope_custom <- function(df,var){
  output = df%>%
    group_by(LakeID)%>%
    dplyr::summarize(n = n(),
                     trend = NA,
                     sig = NA,
                     min_year = NA,
                     max_year = NA)
  for(lake in unique(df$LakeID)){
      filt = df%>%
        ungroup() %>%
        mutate(date = as.Date(paste0(Year, "-01-01"))) %>%
        filter(LakeID==lake)
      if(length(unique(year(filt$date)))>=10){
        sen = trend::sens.slope(filt[[var]])
        output$trend[output$LakeID==lake]<-sen$estimates[1]
        output$sig[output$LakeID==lake]<-sen$p.value[1]
        output$min_year[output$LakeID==lake]<-min(year(filt$date))
        output$max_year[output$LakeID==lake]<-max(year(filt$date))
      }
      #}
    #}
  }
  return(output)
}
