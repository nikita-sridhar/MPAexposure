#'for some reason the multi stressor function for southern CA midcen and channel 
#'islands midcen producing strange output (one big orange rectanglle)
#'doing some troubleshooting for that here.

multi_event <- function(periodt,regionn){
  
  all_events <- events %>%
    merge(temp_event_summary, 
          by = c("File", "NAME", "Date","Year", "degx", "degy","period","julianday")) %>%  
    merge(sum[,c("File","region")], by = c("File")) %>%
    mutate(multi_stressor = case_when(is_pH_low == TRUE & is_DO_low == TRUE ~ TRUE,
                                      is_pH_low == TRUE & is_temp_high == TRUE ~ TRUE,
                                      is_DO_low == TRUE & is_temp_high == TRUE ~ TRUE,
                                      is_pH_low == TRUE & is_DO_low & is_temp_high == TRUE ~ TRUE)) 
  
  
  #finding num of MPAs within each region
  num_mpa <- all_events %>%
    group_by(period, region) %>%
    mutate(num_mpa = n_distinct(File)) %>%
    ungroup() %>%
    distinct(region, num_mpa) 
  
  #plot 

  #setting y range for plot
ifelse(periodt == "historical", yrange <- c(2000,2020), 
  ifelse(periodt == "midcen", yrange <- c(2040,2060), 
         yrange <- c(2080,2100)))
  
 #trying to remove filter arg, instead index, to retain NAs for multistressor prob
#when theres no event - for the figs later.
#still no NA
all_events3 <- all_events %>%
  merge(num_mpa, by = "region") %>%
  group_by(julianday, period, Year, region) %>%
  mutate(multi_prob = mean(n_distinct(File[multi_stressor[[TRUE]]]), na.rm=TRUE)/num_mpa) %>%
  ungroup()
  
  filter(period == periodt, region == regionn) %>%
  ggplot(aes(julianday, Year, fill = multi_prob)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "skyblue") +
  ylim(yrange) +
  xlim(1,365) +
  theme_classic() +
  ggtitle(paste("Frequency of MPAs with 2+ stressor events","in", periodt, "period", "in", regionn)) +
  theme(panel.background = element_rect(fill = "skyblue"))
}

multi_event("endcen","socal")
multi_event("midcen","socal")

