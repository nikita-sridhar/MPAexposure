#Max code for temp anmmalous event analysis

#| label: temp_anom
historic_T_climatology <- mpa %>% filter(period == "historic") %>%
  group_by(File) %>% 
  # sliding: column, size of window. for first and last, over 19 yrs instaed of 20
  mutate(sliding_temp_sd = roll_sd(Temp, 30, fill = NA)) %>% 
  group_by(File, julianday) %>%
  summarise(T_clim_mean = mean(Temp, na.rm = TRUE),
            T_clim_sd = mean(sliding_temp_sd, na.rm = TRUE),
            T_clim_thr = T_clim_mean + 2 * T_clim_sd,
            .groups = "drop")

#makin figs for ano nuevo - historic temp anomaly
T_clim_1mpa <- historic_T_climatology %>% 
  filter(File == "tphdo_mpa_1") %>% 
  mutate(T_clim_lwr = T_clim_mean - 2 * T_clim_sd,
         T_clim_upr = T_clim_mean + 2 * T_clim_sd)
T_event_1mpa <- mpa_historic %>% 
  semi_join(T_clim_1mpa, by = "File")

ggplot(T_clim_1mpa, aes(julianday)) +
  geom_ribbon(aes(ymin = T_clim_lwr, ymax = T_clim_upr),
              fill = "black", alpha = 0.75) +
  geom_line(aes(y = Temp, color = Year, group = Year),
            data = T_event_1mpa,
            alpha = 0.5) +
  geom_line(aes(y = T_clim_mean), size = 1, color = "blue") +
  scale_color_viridis_c() +
  labs(title = "Historic IPSL T Ano Nuevo") +
  theme_classic()




#function
  mpa %>% 
  filter(period == "historic", 
         NAME == "Ano Nuevo SMR") %>%
  mutate(sliding_sd = roll_sd(Temp, 30, fill = NA)) %>% 
  group_by(julianday) %>%
  mutate(clim_mean = mean(Temp, na.rm = TRUE),
         clim_sd = mean(sliding_sd, na.rm = TRUE),
         clim_upr = clim_mean + 2 * clim_sd,
         clim_lwr = clim_mean - 2 * clim_sd) %>% #but this we decided is not how we wan't to categorize anom for temp.
  ungroup() %>%
  
  ggplot(aes(julianday)) +
  geom_ribbon(aes(ymin = clim_lwr, ymax = clim_upr),
              fill = "black", alpha = 0.75) +
  geom_line(aes(y = Temp, color = Year, group = Year),
            alpha = 0.5) +
  geom_line(aes(y = clim_mean), size = 1, color = "blue") +
  scale_color_viridis_c() +
  theme_classic()

  #from above getting a numeric error - think its bc im passing a column as an argument into the function - need to call using sq bracket?
 
  func <- function(df, periodt, site, var){ #introduce one by one to below to see where prob is
   mpa %>% 
    filter(period == periodt, 
           NAME == site) %>%
    mutate(sliding_sd = roll_sd(mpa[[var]], 30, fill = NA)) %>% 
    group_by(julianday) %>%
    mutate(clim_mean = mean(mpa[[var]], na.rm = TRUE),
           clim_sd = mean(sliding_sd, na.rm = TRUE),
           clim_upr = clim_mean + 2 * clim_sd,
           clim_lwr = clim_mean - 2 * clim_sd) %>% 
    ungroup() %>%
    
    ggplot(aes(julianday)) +
    geom_ribbon(aes(ymin = clim_lwr, ymax = clim_upr),
                fill = "black", alpha = 0.75) +
    geom_line(aes(y = mpa[[var]], color = Year, group = Year),
              alpha = 0.5) +
    geom_line(aes(y = clim_mean), size = 1, color = "blue") +
    scale_color_viridis_c() +
    theme_classic() 
}
  
  func(mpa, "historic","Ano Nuevo SMR", "Temp") 
  
testing <- sum %>%
  mutate(thing = roll_sd(sum[[Temp]], 30, fill = NA))

roll_sd(sum[["Temp"]], 30, fill = NA) #this is prob im getting in function.

#function for time series but w categorizing anom based on hist clim
  make_temp_time_series <- function(df, periodt, site){
      
      mpa_with_histclimsd %>% 
      filter(period == periodt, 
             NAME == site) %>%
      group_by(julianday) %>% #are u sure?
      mutate(clim_mean = mean(Temp, na.rm = TRUE), #clim mean of period in question
             clim_upr = hist_T_clim + 2 * hist_T_clim_sd, #upper threshold of envelope based on historical clim
             clim_lwr = hist_T_clim - 2 * hist_T_clim_sd) %>% #lower threshold of envelope based on historical clim
      ungroup() %>%
      
      ggplot(aes(julianday)) +
      geom_ribbon(aes(ymin = clim_lwr, ymax = clim_upr),
                  fill = "black", alpha = 0.7) +
      geom_line(aes(y = Temp, color = Year, group = Year),
                alpha = 0.5) +
      geom_line(aes(y = clim_mean), size = 1, color = "blue") + #should this be mean of given period? or hist
      geom_line(aes(y = hist_T_clim), size = 1, color = "black") +
      scale_color_viridis_c() +
      labs(title = paste(periodt, site)) +
      theme_classic()
  }
  
  make_temp_time_series(mpa, "historic","Ano Nuevo SMR")  
  
#function similar to above, except using roll sd for historical so ribbon doesn't look jagged
  #once you fix this, do same for other variables. and apply to anom temp event analysis.
  make_temp_time_series <- function(df, periodt, site)
    
    hist_mpa_roll <- mpa %>% 
      filter(period == "historic", 
             NAME == "Ano Nuevo SMR") %>% #make sure all NAME is unique - bc you group by or merge by File in some cases - can be applied throughout doc
      select(File, julianday, Temp) %>%
      group_by(julianday) %>% 
      rollapply(width = 30, FUN = sd, fill = NA) #change fill to add a vector with first 15 days and last 15 days for beginning and end
    
  
      mutate(sliding_sd = roll_sd(Temp, 30, fill = NA))   #troubleshoot here! 
      mutate(hist_clim_mean = mean(Temp, na.rm = TRUE), #clim mean of period in question
             hist_clim_sd = mean(sliding_sd, na.rm = TRUE), #sd within 30 day window of historical period
             hist_clim_upr = hist_clim_mean + 2 * hist_clim_sd,
             hist_clim_lwr = hist_clim_mean - 2 * hist_clim_sd) %>% #lower threshold of envelope based on historical clim
      ungroup() 
      
    mpa_test <- mpa %>% #doing this bc sometimes period won't be historic - will need to merge that w histclim
      filter(period == "historic",
             NAME == "Ano Nuevo SMR") %>%
      left_join(hist_mpa_roll, by = c("File", "julianday", "Temp")) %>%
      group_by(julianday) %>%
      mutate(period_clim_mean = mean(Temp, na.rm =TRUE)) %>%
      ungroup() %>% #unpipe here to make sure mpa_test works
      
      ggplot(aes(julianday)) +
      geom_ribbon(aes(ymin = hist_clim_lwr, ymax = hist_clim_upr),
                  fill = "black", alpha = 0.7) +
      geom_line(aes(y = Temp, color = Year, group = Year),
                alpha = 0.5) +
      geom_line(aes(y = period_clim_mean), size = 1, color = "blue") + #trend line for period
      geom_line(aes(y = hist_clim_mean), size = 1, color = "black") + #trend line for histclim (reference for anom)
      scale_color_viridis_c() +
      labs(title = paste(periodt, site)) +
      theme_classic()
  }
  
  make_temp_time_series(mpa, "historic","Ano Nuevo SMR") 


  
#anom event analysis  
historic_heat_events <- mpa_historic %>% 
  left_join(historic_T_climatology, by = c("File", "julianday")) %>% 
  mutate(is_hot = T_surf > T_clim_thr,
         heat_event = label_events(is_hot))

historic_heat_event_summary <- historic_heat_events %>% 
  drop_na(heat_event) %>% 
  group_by(File, heat_event) %>% 
  summarize(duration_days = n(),
            mean_T = mean(T_surf),
            mean_severity = mean((T_surf - T_clim_mean) / T_clim_sd),
            intensity = duration_days * mean_severity,
            .groups = "drop")
historic_heat_mpa_summary <- historic_heat_event_summary %>%
  group_by(File) %>%
  summarise(historic_heat_num_event = n(),
            historic_heat_mean_duration = mean(duration_days),
            historic_heat_max_duration = max(duration_days),
            historic_heat_mean_T = mean(mean_T),
            historic_heat_mean_severity = mean(mean_severity),
            historic_heat_mean_intensity = mean(intensity)) %>%
  merge(mpa_centroids, by = "File")
write.csv(historic_heat_mpa_summary, file="IPSL_historic_heat_mpa_events.csv")

#visualizing events in one mpa per region - historic

historic_heat_events %>% 
  left_join(historic_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(NAME == "Ano Nuevo SMR") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()
