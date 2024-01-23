#Max code for temp anmmalous event analysis

#| label: temp_anom
historic_T_climatology <- mpa_historic %>% 
  group_by(File) %>% 
  # sliding: column, size of window. for first and last, over 19 yrs instaed of 20
  mutate(sliding_temp_sd = roll_sd(T_surf, 30, fill = NA)) %>% 
  group_by(File, julianday) %>%
  summarise(T_clim_mean = mean(T_surf, na.rm = TRUE),
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
  geom_line(aes(y = T_surf, color = Year, group = Year),
            data = T_event_1mpa,
            alpha = 0.5) +
  geom_line(aes(y = T_clim_mean), size = 1, color = "blue") +
  scale_color_viridis_c() +
  labs(title = "Historic IPSL T Ano Nuevo") +
  theme_classic()

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
historic_heat_events %>% 
  left_join(historic_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(NAME == "South La Jolla SMR") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()
historic_heat_events %>% 
  left_join(historic_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(NAME == "Van Damme SMCA") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()


#midcen
midcen_T_climatology <- mpa_midcen %>% 
  group_by(File) %>% 
  mutate(sliding_temp_sd = roll_sd(T_surf, 30, fill = NA)) %>% 
  group_by(File, julianday) %>%
  summarise(T_clim_mean = mean(T_surf, na.rm = TRUE),
            T_clim_sd = mean(sliding_temp_sd, na.rm = TRUE),
            T_clim_thr = T_clim_mean + 2 * T_clim_sd,
            .groups = "drop")
midcen_heat_events <- mpa_midcen %>% 
  left_join(midcen_T_climatology, by = c("File", "julianday")) %>% 
  mutate(is_hot = T_surf > T_clim_thr,
         heat_event = label_events(is_hot))
midcen_heat_event_summary <- midcen_heat_events %>% 
  drop_na(heat_event) %>% 
  group_by(File, heat_event) %>% 
  summarize(duration_days = n(),
            mean_T = mean(T_surf),
            mean_severity = mean((T_surf - T_clim_mean) / T_clim_sd),
            intensity = duration_days * mean_severity,
            .groups = "drop")
midcen_heat_mpa_summary <- midcen_heat_event_summary %>%
  group_by(File) %>%
  summarise(midcen_heat_num_event = n(),
            midcen_heat_mean_duration = mean(duration_days),
            midcen_heat_max_duration = max(duration_days),
            midcen_heat_mean_T = mean(mean_T),
            midcen_heat_mean_severity = mean(mean_severity),
            midcen_heat_mean_intensity = mean(intensity)) %>%
  merge(mpa_centroids, by = "File")
write.csv(midcen_heat_mpa_summary, file="IPSL_midcen_heat_mpa_events.csv")

#visualizing events in one mpa per region - midcen
midcen_heat_events %>% 
  left_join(midcen_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(File == "Ano Nuevo SMR") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()
midcen_heat_events %>% 
  left_join(midcen_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(File == "South La Jolla SMR") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()
midcen_heat_events %>% 
  left_join(midcen_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(File == "Van Damme SMCA") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()

#endcen
endcen_T_climatology <- mpa_endcen %>% 
  group_by(File) %>% 
  mutate(sliding_temp_sd = roll_sd(T_surf, 30, fill = NA)) %>% 
  group_by(File, julianday) %>%
  summarise(T_clim_mean = mean(T_surf, na.rm = TRUE),
            T_clim_sd = mean(sliding_temp_sd, na.rm = TRUE),
            T_clim_thr = T_clim_mean + 2 * T_clim_sd,
            .groups = "drop")
endcen_heat_events <- mpa_endcen %>% 
  left_join(endcen_T_climatology, by = c("File", "julianday")) %>% 
  mutate(is_hot = T_surf > T_clim_thr,
         heat_event = label_events(is_hot))
endcen_heat_event_summary <- endcen_heat_events %>% 
  drop_na(heat_event) %>% 
  group_by(File, heat_event) %>% 
  summarize(duration_days = n(),
            mean_T = mean(T_surf),
            mean_severity = mean((T_surf - T_clim_mean) / T_clim_sd),
            intensity = duration_days * mean_severity,
            .groups = "drop")
endcen_heat_mpa_summary <- endcen_heat_event_summary %>%
  group_by(File) %>%
  summarise(endcen_heat_num_event = n(),
            endcen_heat_mean_duration = mean(duration_days),
            endcen_heat_max_duration = max(duration_days),
            endcen_heat_mean_T = mean(mean_T),
            endcen_heat_mean_severity = mean(mean_severity),
            endcen_heat_mean_intensity = mean(intensity)) %>%
  merge(mpa_centroids, by = "File")
write.csv(endcen_heat_mpa_summary, file="IPSL_endcen_heat_mpa_events.csv")


#visualizing events in one mpa per region - endcen
endcen_heat_events %>% 
  left_join(endcen_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(File == "Ano Nuevo SMR") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()
endcen_heat_events %>% 
  left_join(endcen_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(File == "South La Jolla SMR") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()
endcen_heat_events %>% 
  left_join(endcen_heat_event_summary, by = c("File", "heat_event")) %>% 
  filter(File == "Van Damme SMCA") %>% 
  ggplot(aes(julianday, Year, fill = intensity)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  theme_classic()