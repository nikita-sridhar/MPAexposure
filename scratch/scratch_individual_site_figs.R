individual_site_figs <- function(site){
  
  #sev-int-dur figs - historic
  make_sev_int_dur(temp_all_events, "historic", site)
  make_sev_int_dur(pH_all_events, "historic", site)
  make_sev_int_dur(DO_all_events, "historic", site)
  #sev-int-dur figs - midcen
  make_sev_int_dur(temp_all_events, "midcen", site)
  make_sev_int_dur(pH_all_events, "midcen", site)
  make_sev_int_dur(DO_all_events, "midcen", site)
  #sev-int-dur figs - endcen
  make_sev_int_dur(temp_all_events, "endcen", site)
  make_sev_int_dur(pH_all_events, "endcen", site)
  make_sev_int_dur(DO_all_events, "endcen", site)
  
  #time series - historic
  make_temp_time_series(mpa, "historic",site) 
  make_pH_time_series(mpa, "historic",site) 
  make_DO_time_series(mpa, "historic",site)
  #time series - midcen
  make_temp_time_series(mpa, "midcen",site) 
  make_pH_time_series(mpa, "midcen",site) 
  make_DO_time_series(mpa, "midcen",site)
  #time series - endcen
  make_temp_time_series(mpa, "endcen",site) 
  make_pH_time_series(mpa, "endcen",site) 
  make_DO_time_series(mpa, "endcen",site) 
  
}

individual_site_figs("Ano Nuevo SMR")