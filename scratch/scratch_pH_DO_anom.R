
#all event categorization for pH and DO
events <- mpa %>%
  group_by(period) %>%
  select(File, Date, Year,NAME, pH, DO, period, degx, degy) %>%
  mutate(is_pH_low = pH < 7.75,
         pH_event = label_events(is_pH_low),
        
         is_DO_low = DO < 4.6,
         DO_event = label_events(is_DO_low),
         
         is_pH_and_DO_low = ifelse(is_pH_low == TRUE & is_DO_low == TRUE, TRUE, FALSE),
         pH_and_DO_event = label_events(is_pH_and_DO_low)) 

pH_event_summary_test <- events %>%
  filter(is_pH_low == TRUE) %>% #this works for just pH, but if want to apply this to all variables
  #need something more temporary than filter. like a casewhen within mutate or something.
  select(-DO_event, -DO, -is_DO_low, -is_pH_and_DO_low, -pH_and_DO_event) %>%
  group_by(File, pH_event) %>% #don't think period here makes a difference - at event scale
  mutate(duration_days = n(),
         event_begin = min(Date),
         event_mean = mean(pH), #mean during the individual event 
         intensity = 7.75 - event_mean,
         severity = intensity*duration_days) %>%
  ungroup() %>%

  #mpa summary (across periods)
  group_by(File, period) %>%
  mutate(num_event = n_distinct(pH_event), 
         mean_event_duration = mean(duration_days),  
         max_event_duration = max(duration_days),
         mean_event_mean_pH = mean(event_mean), #each event has a mean. take the mean of those per MPA, per time period 
         mean_event_intensity = mean(intensity),
         mean_event_severity = mean(severity)) %>%
  ungroup() %>%

  #mpa summary (annual)
  group_by(File, Year) %>%
  mutate(annual_days_belowthresh = n(),
         annual_avg_ph_belowthresh = mean(pH)) %>%
  ungroup() %>%
  group_by(File) %>%
  mutate(meanannual_days_belowthresh = mean(annual_days_belowthresh),
         meanannual_avg_pH_belowthresh = mean(annual_avg_ph_belowthresh)) %>%
  
  #now removing event-scale variables not needed to condense to mpa scale
  select(-duration_days, -event_begin, -event_mean, -intensity, -severity, -pH_event, -is_pH_low) %>%
  distinct(File, .keep_all = TRUE)
  
 


#temp:
mpa_with_histclimsd <- mpa %>%
  filter(period == "historic") %>%
  group_by(File, Month) %>%
  summarise(hist_T_clim = mean(Temp), hist_T_clim_sd = sd(Temp)) %>%
  merge(mpa, by = c("File", "Month"))

temp_events <- mpa_with_histclimsd %>% 
  group_by(period) %>%
  select(File, Date, Year,NAME, Temp, hist_T_clim, hist_T_clim_sd, period, degx, degy) %>%
  mutate(is_temp_high = Temp > (hist_T_clim + 2*hist_T_clim_sd),
         temp_event = label_events(is_temp_high)) %>%
  
  filter(is_temp_high == TRUE) %>%
  group_by(File, temp_event) %>% 
  mutate(duration_days = n(),
         event_begin = min(Date),
         event_mean = mean(Temp),  
         intensity = Temp - (hist_T_clim + 2*hist_T_clim),
         severity = intensity*duration_days) %>%
  ungroup() %>%

  #mpa summary (across periods)
  group_by(File, period) %>%
  mutate(num_event = n_distinct(temp_event), 
         mean_event_duration = mean(duration_days),  
         max_event_duration = max(duration_days),
         mean_event_mean_temp = mean(event_mean), 
         mean_event_intensity = mean(intensity),
         mean_event_severity = mean(severity)) %>%
  ungroup() %>%
  
  #mpa summary (annual)
  group_by(File, Year) %>%
  mutate(annual_days_abovethresh = n(), #is this right?
         annual_avg_temp_abovethresh = mean(Temp)) %>%
  ungroup() %>%
  group_by(File) %>%
  mutate(meanannual_days_abovethresh = mean(annual_days_abovethresh),
         meanannual_avg_temp_abovethresh = mean(annual_avg_temp_abovethresh)) %>%
  
  #now removing event-scale variables not needed to condense to mpa scale
  select(-Temp,-duration_days, -event_begin, -event_mean, -intensity, -severity, -is_temp_high) %>%
  group_by(period, temp_event) %>%
  distinct(period, temp_event, .keep_all = TRUE)
  

#figuring out which regions to choose for sev/int/dur analysis

individual_site_figs <- function(site){

#sev-int-dur figs
make_sev_int_dur(temp_all_events, "historic", site)
make_sev_int_dur(pH_all_events, "historic", site)
make_sev_int_dur(DO_all_events, "historic", site)

make_sev_int_dur(temp_all_events, "midcen", site)
make_sev_int_dur(pH_all_events, "midcen", site)
make_sev_int_dur(DO_all_events, "midcen", site)

make_sev_int_dur(temp_all_events, "endcen", site)
make_sev_int_dur(pH_all_events, "endcen", site)
make_sev_int_dur(DO_all_events, "endcen", site)

#time series
make_temp_time_series(mpa, "historic",site) 
make_pH_time_series(mpa, "historic",site) 
make_DO_time_series(mpa, "historic",site)

make_temp_time_series(mpa, "midcen",site) 
make_pH_time_series(mpa, "midcen",site) 
make_DO_time_series(mpa, "midcen",site)

make_temp_time_series(mpa, "endcen",site) 
make_pH_time_series(mpa, "endcen",site) 
make_DO_time_series(mpa, "endcen",site) 

}

individual_site_figs("Ano Nuevo SMR")





#trying again to make a function for pH/DO events




events <- mpa %>%
  group_by(period) %>%
  select(File, Date, Year,NAME, pH, DO, period, degx, degy, julianday) %>%
  mutate(is_pH_low = pH < 7.75,
         pH_event = label_events(is_pH_low),
         
         is_DO_low = DO < 4.6,
         DO_event = label_events(is_DO_low),
         
         is_pH_and_DO_low = ifelse(is_pH_low == TRUE & is_DO_low == TRUE, TRUE, FALSE),
         pH_and_DO_event = label_events(is_pH_and_DO_low)) 


event_summary <- events %>%
  

  filter(is_pH_low == TRUE) %>% 
  select(-DO_event, -DO, -is_DO_low, -is_pH_and_DO_low, -pH_and_DO_event) %>%
  group_by(File, pH_event) %>% 
  mutate(duration_days = n(),
         event_begin = min(Date),
         event_mean = mean(pH), #mean during the individual event 
         intensity = 7.75 - event_mean,
         severity = intensity*duration_days) %>%
  ungroup() %>%
  
  #mpa summary (across periods)
  group_by(File, period) %>%
  mutate(num_event = n_distinct(pH_event), 
         mean_event_duration = mean(duration_days),  
         max_event_duration = max(duration_days),
         mean_event_mean_pH = mean(event_mean), 
         mean_event_intensity = mean(intensity),
         mean_event_severity = mean(severity)) %>%
  ungroup() %>%
  
  #mpa summary (annual)
  group_by(File, Year) %>%
  mutate(annual_days_belowthresh = n(), annual_avg_ph_belowthresh = mean(pH)) %>%
  ungroup() %>%
  group_by(File) %>%
  mutate(meanannual_days_belowthresh = mean(annual_days_belowthresh),
         meanannual_avg_pH_belowthresh = mean(annual_avg_ph_belowthresh)) 

#now removing event-scale variables not needed to condense to mpa scale
pH_mpa_summary <- pH_event_summary %>%
  select(-duration_days, -event_begin, -event_mean, -intensity, 
         -severity, -is_pH_low,-pH_event, -julianday) %>%
  distinct(period, File, .keep_all = TRUE) 

#used later
pH_all_events <- left_join(events, pH_event_summary,
                           by = c("File", "Date", "Year", "NAME", "pH", "period", 
                                  "degx", "degy", "julianday", "is_pH_low", "pH_event"))



####################################################################
#7/24/26: trying to make no events a 0 id so it shows up on the map

ph_do_events <- mpa %>%
  group_by(period) %>%
  select(File, Date, Year,NAME, pH, DO, period, degx, degy, julianday) %>%
  mutate(is_pH_low = pH < 7.75,
         pH_event_id = ifelse(is_pH_low == "FALSE", NA, label_events(is_pH_low)),
         is_DO_low = DO < 4.6,
         DO_event_id = ifelse(is_DO_low == "FALSE", NA, label_events(is_DO_low))) 

#pH event summary
ph_do_event_summary <- ph_do_events %>%

  #ph events (only calculate summary stats when an event took place)
  group_by(File, pH_event_id, period) %>% 
  mutate(pH_duration_days = if_else(is_pH_low == "TRUE", n(), NA),
         pH_duration_days = if_else(pH_duration_days == 1, NA, pH_duration_days), #want 1 day event to register as no event
         pH_event_id = if_else(pH_duration_days == 0, NA, pH_event_id),#want to just make non events a 0 (incl 2 day events w duration days)
         pH_event_mean = if_else(pH_duration_days > 1, mean(pH, na.rm = TRUE), NA)) %>%
  ungroup() %>%
  
  #do events
  group_by(File, DO_event_id, period) %>% 
  mutate(DO_duration_days = if_else(is_DO_low == "TRUE", n(), NA),
         DO_duration_days = if_else(DO_duration_days == 1, NA, DO_duration_days), #want 1 day event to register as no event
         DO_event_id = if_else(DO_duration_days == 0, NA, DO_event_id),#want to just make non events an NA
         DO_event_mean = if_else(DO_duration_days > 1, mean(DO, na.rm = TRUE), NA)) %>%
  ungroup() 

#since an event can span multiple years, annual sum is n() in that year (not duration days since that can show duration spanning multiple years)
ph_do_annual_event_counts <- ph_do_event_summary %>%
  #per mpa per year
  group_by(File, Year, period) %>%
  mutate(annual_days_below_pH_thresh = sum(!is.na(pH_event_id)),
         annual_avg_pH_belowthresh = mean(pH_event_mean, na.rm = TRUE),
         annual_days_below_DO_thresh = sum(!is.na(DO_event_id)),
         annual_avg_DO_belowthresh = mean(DO_event_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  #per mpa
  group_by(File, period) %>%
  mutate(avg_annual_days_below_pH_thresh = mean(annual_days_below_pH_thresh, na.rm=TRUE),
         avg_annual_ph_belowthresh = mean(annual_avg_pH_belowthresh, na.rm= TRUE),
         avg_annual_days_below_DO_thresh = mean(annual_days_below_DO_thresh, na.rm=TRUE),
         avg_annual_DO_belowthresh = mean(annual_avg_DO_belowthresh, na.rm= TRUE)) %>%
  ungroup() %>%
  distinct(File, NAME, degx, degy, period, avg_annual_days_below_pH_thresh, 
           avg_annual_ph_belowthresh, avg_annual_days_below_DO_thresh, avg_annual_DO_belowthresh)


#temperature events: 2+ days where daily temp > (historical climatology's temp for that month) + 2* (historical climatology SD)

temp_events <- mpa %>%
  
  #first calculating reference historic climatology
  filter(period == "historic") %>%
  group_by(File, Month) %>%
  summarise(histclim_Temp_mean = mean(Temp), 
            histclim_Temp_sd = sd(Temp)) %>% #this sd is temp over all days in Jan - rather than average of each jan's SD - does it make a difference? 
  #bringing back to scale of julian days and all time periods
  merge(mpa, by = c("File", "Month")) %>%
  select(File, Date, Year, NAME, Temp, period, degx, degy, julianday, histclim_Temp_mean, histclim_Temp_sd) %>%
  
  #labelling events
  group_by(period) %>%
  mutate(is_temp_high = Temp > (histclim_Temp_mean + 2*histclim_Temp_sd),
         temp_event_id = ifelse(is_temp_high == "FALSE", NA, label_events(is_temp_high))) %>%
  select(-histclim_Temp_mean, -histclim_Temp_sd) %>%
         
  #temp event summary 
  group_by(File, temp_event_id, period) %>% 
  mutate(temp_duration_days = if_else(is_temp_high == "TRUE", n(), NA),
         temp_duration_days = if_else(temp_duration_days == 1, NA, temp_duration_days), #want 1 day event to register as no event
         temp_event_id = if_else(temp_duration_days == 0, NA, temp_event_id),#want to just make non events an NA
         temp_event_mean = if_else(temp_duration_days > 1, mean(Temp, na.rm = TRUE), NA)) %>%
  ungroup() 

#putting it all together
annual_events <- temp_events %>%
  #first finding annual averages for temp
  #per mpa per year
  group_by(File, Year, period) %>%
  mutate(annual_days_above_temp_thresh = sum(!is.na(temp_event_id)),
         annual_avg_temp_abovethresh = mean(temp_event_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  #per mpa
  group_by(File, period) %>%
  summarise(avg_annual_days_above_temp_thresh = mean(annual_days_above_temp_thresh, na.rm=TRUE),
         avg_annual_temp_belowthresh = mean(annual_avg_temp_abovethresh, na.rm= TRUE)) %>%
  ungroup() %>%
  right_join(ph_do_annual_event_counts, by = c("File", "period"))



  


  
  
  





