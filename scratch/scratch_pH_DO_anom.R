
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
  





