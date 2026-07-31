
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

pH_DO_event <- function(variable){

event_summary <- events %>%
  
  ifelse(variable = "pH"){
  
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
         pH_event_id = ifelse(is_pH_low == "FALSE", 0, label_events(is_pH_low)),
         is_DO_low = DO < 4.6,
         DO_event_id = ifelse(is_DO_low == "FALSE", 0, label_events(is_DO_low)),
         is_pH_and_DO_low = ifelse(is_pH_low == TRUE & is_DO_low == TRUE, TRUE, FALSE),
         pH_and_DO_event_id = label_events(is_pH_and_DO_low)) 

#pH event summary
pH_event_summary <- ph_do_events %>%
  select(-DO_event_id, -DO, -is_DO_low, -is_pH_and_DO_low, -pH_and_DO_event_id) %>%
  
  #event scale (only calculate summary stats when an event took place)
  group_by(File, pH_event_id, period) %>% 
  mutate(duration_days = if_else(is_pH_low == "TRUE", n(), 0),
         event_mean = if_else(duration_days > 1, mean(pH), NA), 
         intensity = if_else(duration_days > 1, 7.75 - event_mean, NA),
         severity = if_else(duration_days > 1, intensity*duration_days, NA)) %>%
  ungroup() 

#need to pick up here !!!! basically, want to change events w one day to look like 0 days, 
#and calculate annual event days for 2+ day events. then do same for other summary stats.

#since an event can span multiple years, annual sum is n() in that year (not duration days since that can show duration spanning multiple years)
annual_event_counts <- pH_event_summary %>%
  
  #changing events with 1 day to 0 bc we are counting event as 2 days +
  mutate()
  group_by(File, Year, period) %>%
  summarise(annual_days_belowthresh = n(), 
         annual_avg_ph_belowthresh = mean(pH, na.rm = TRUE),
         .groups = "drop") %>%
  
  #add years w no events
  complete(sum %>% distinct(File, period),
             fill = list(annual_days_belowthresh = 0)) %>%

  group_by(File, period) %>%
  summarise(avg_annual_days_below_pH_thresh = mean(annual_days_below_pH_thresh, na.rm=TRUE),
            avg_annual_ph_belowthresh = mean(annual_avg_ph_belowthresh, na.rm= TRUE),
            .groups = "drop")




#used later
pH_all_events <- left_join(events, pH_event_summary,
                           by = c("File", "Date", "Year", "NAME", "pH", 
                                  "period", "degx", "degy", "julianday",
                                  "is_pH_low", "pH_event"))

##########################################################################################
#7/31
#this is the previous final version of the anom event map code before above ^^ edits put in place
##########################################################################################

label_events <- function(is_event) {
  event_rle <- rle(is_event)
  labels <- ifelse(event_rle$values, cumsum(event_rle$values), NA)
  rep(labels, event_rle$lengths)
}

#all event categorization for pH and DO 
events <- mpa %>%
  group_by(period) %>%
  select(File, Date, Year,NAME, pH, DO, period, degx, degy, julianday) %>%
  mutate(is_pH_low = pH < 7.75,
         pH_event = ifelse(is_pH_low == "FALSE", 0, label_events(is_pH_low)),
         
         is_DO_low = DO < 4.6,
         DO_event = ifelse(is_DO_low == "FALSE", 0, label_events(is_DO_low)),
         
         is_pH_and_DO_low = ifelse(is_pH_low == TRUE & 
                                     is_DO_low == TRUE, TRUE, FALSE),
         pH_and_DO_event = label_events(is_pH_and_DO_low)) 

#pH event summary
pH_event_summary <- events %>%
  filter(is_pH_low == TRUE) %>% 
  select(-DO_event, -DO, -is_DO_low, -is_pH_and_DO_low, -pH_and_DO_event) %>%
  group_by(File, pH_event) %>% 
  mutate(duration_days = n(),
         event_begin = min(Date),
         event_mean = mean(pH), #mean during the individual event 
         intensity = 7.75 - event_mean,
         severity = intensity*duration_days) %>%
  filter(duration_days > 1) %>%
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
  group_by(File, Year, period) %>%
  mutate(annual_days_belowthresh = n(), annual_avg_ph_belowthresh = mean(pH)) %>%
  ungroup() %>%
  group_by(File, period) %>%
  mutate(meanannual_days_belowthresh = mean(annual_days_belowthresh),
         meanannual_avg_pH_belowthresh = mean(annual_avg_ph_belowthresh)) 

#now removing event-scale variables not needed to condense to mpa scale
pH_mpa_summary <- pH_event_summary %>%
  select(-duration_days, -event_begin, -event_mean, -intensity, 
         -severity, -is_pH_low,-pH_event, -julianday, -annual_days_belowthresh, -annual_avg_ph_belowthresh) %>%
  distinct(period, File, .keep_all = TRUE) 

#used later
pH_all_events <- left_join(events, pH_event_summary,
                           by = c("File", "Date", "Year", "NAME", "pH", 
                                  "period", "degx", "degy", "julianday",
                                  "is_pH_low", "pH_event"))

#DO event summary
DO_event_summary <- events %>%
  filter(is_DO_low == TRUE) %>% 
  select(-pH_event, -pH, -is_pH_low, -is_pH_and_DO_low, -pH_and_DO_event) %>%
  group_by(File, DO_event) %>% 
  mutate(duration_days = n(),
         event_begin = min(Date),
         event_mean = mean(DO), #mean during the individual event 
         intensity = 7.75 - event_mean,
         severity = intensity*duration_days) %>%
  filter(duration_days > 1) %>%
  ungroup() %>%
  
  #mpa summary (across periods)
  group_by(File, period) %>%
  mutate(num_event = n_distinct(DO_event), 
         mean_event_duration = mean(duration_days),  
         max_event_duration = max(duration_days),
         mean_event_mean_pH = mean(event_mean), 
         mean_event_intensity = mean(intensity),
         mean_event_severity = mean(severity)) %>%
  ungroup() %>%
  
  #mpa summary (annual)
  group_by(File, Year, period) %>%
  mutate(annual_days_belowthresh = n(), annual_avg_DO_belowthresh = mean(DO)) %>%
  ungroup() %>%
  group_by(File, period) %>%
  mutate(meanannual_days_belowthresh = mean(annual_days_belowthresh),
         meanannual_avg_DO_belowthresh = mean(annual_avg_DO_belowthresh)) 

#now removing event-scale variables not needed to condense to mpa scale
DO_mpa_summary <- DO_event_summary %>% 
  select(-duration_days, -event_begin, -event_mean, -intensity, 
         -severity, -is_DO_low, -DO_event, -julianday, -annual_days_belowthresh, -annual_avg_DO_belowthresh) %>%
  distinct(File, period, .keep_all = TRUE) 

#used later
DO_all_events <- left_join(events, DO_event_summary,
                           by = c("File", "Date", "Year", "NAME", "DO", 
                                  "period", "degx", "degy", "julianday",
                                  "is_DO_low", "DO_event"))

#temp:
mpa_with_histclimsd <- mpa %>%
  filter(period == "historic") %>%
  group_by(File, Month) %>%
  summarise(hist_T_clim = mean(Temp), hist_T_clim_sd = sd(Temp)) %>% #this sd is temp over all days in Jan - rather than average of each jan's SD - does it make a difference? 
  merge(mpa, by = c("File", "Month"))

temp_event_summary <- mpa_with_histclimsd %>% 
  group_by(period) %>%
  select(File, Date, Year,NAME, Temp, hist_T_clim, hist_T_clim_sd, period, 
         degx, degy, julianday) %>%
  mutate(is_temp_high = Temp > (hist_T_clim + 2*hist_T_clim_sd),
         temp_event = label_events(is_temp_high)) %>%
  
  filter(is_temp_high == TRUE) %>%
  group_by(File, temp_event, period) %>% 
  mutate(duration_days = n(),
         event_begin = min(Date),
         event_mean = mean(Temp),  
         intensity = Temp - (hist_T_clim + 2*hist_T_clim_sd),
         severity = intensity*duration_days) %>%
  filter(duration_days > 1) %>%
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
  group_by(File, Year, period) %>%
  mutate(annual_days_abovethresh = n(), 
         annual_avg_temp_abovethresh = mean(Temp)) %>%
  ungroup() %>%
  group_by(File, period) %>%
  mutate(meanannual_days_abovethresh = mean(annual_days_abovethresh),
         meanannual_avg_temp_abovethresh = mean(annual_avg_temp_abovethresh)) 

#now removing event-scale variables not needed to condense to mpa scale
temp_mpa_summary <- temp_event_summary %>% 
  select(-Temp,-duration_days, -event_begin, -event_mean, -intensity, 
         -severity, -is_temp_high, -temp_event, -julianday, -annual_days_abovethresh, -annual_avg_temp_abovethresh) %>%
  distinct(period, File, .keep_all = TRUE)

#used later
temp_all_events <- left_join(mpa, temp_event_summary,
                             by = c("File", "Date", "Year", "NAME", "Temp", "period", 
                                    "degx", "degy", "julianday"))











