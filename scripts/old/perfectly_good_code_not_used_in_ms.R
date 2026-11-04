##########
#heatmaps
#########

#Create heatmaps per summary stat showing time periods on x-axis, and all MPAs 
#on y-axis to pick up on finer scale geographical patterns in change of summary stats over time.

col <- colorRampPalette(brewer.pal(9,"YlOrRd"))(256)
invert_col <- colorRampPalette(rev(brewer.pal(9,"YlOrRd")))(256)


make_heatmap <- function(sumstat, color_palette = invert_col, title){
  
  #make matrix for variable of interest
  matrix <- sum %>%
    mutate(color = case_when((region == "centralca") ~ "#ffff99", 
                             (region == "norca") ~ "#beaed4",
                             (region == "socal") ~ "#fdc086",
                             (region == "channel") ~ "#7fc97f")) %>%
    select(!!sym(sumstat), NAME, degy, period, region, color) %>%
    pivot_wider(names_from = period, values_from = sumstat) %>%
    arrange(degy)
  
  
  
  #making matrix numeric for heatmap to work
  matrix_numeric <- matrix %>%
    arrange(degy) %>%
    select(-NAME, -region, -degy, -color) %>%
    select(historic, midcen, endcen) %>%
    as.matrix()
  
  row.names(matrix_numeric) <- matrix$NAME
  
  #jpeg(file=here(paste("./figs/heatmap/heatmap",sumstat,".png", 
  #sep = "_")))
  
  heatmap <- heatmap.2(matrix_numeric, Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
                       main = title, tracecol=NA, revC= TRUE,
                       margins = c(3,5), col= color_palette, srtCol = 360, 
                       labRow = matrix$NAME,cexRow = 0.5, cexCol = 1, keysize = 0.95, 
                       key.title = sumstat, key.xlab = NA, key.ylab = NA, par(cex.main=0.5)) #+
  
  #RowSideColors = matrix$color - add this within above - but it is inverted need to fix.
  
  #legend("left",      
  #legend = c("Central CA", "Northern CA", "Southern CA", "Channel Isl"),
  #col = unique(matrix$color), 
  #lty= 1,             
  #lwd = 5,           
  #cex=.7
  #)
  
  #ggsave(here(paste("./figs/heatmap/heatmapTEST.png")), plot)
  
  #dev.off()
  
}

#Temp heatmaps
make_heatmap("Temp_mean", col, title = "Temp (C) mean across periods" )
make_heatmap("Temp_high10", col, title = "Temp highest 10% across periods")
make_heatmap("Temp_low10", col, title = "Temp lowest 10% across periods")
make_heatmap("T_eventSD", col, title = "Temp event SD across periods")
make_heatmap("T_seasonalSD", col, title = "Temp seasonal SD across periods")

#pH heatmaps
make_heatmap("pH_mean", title = "pH mean across periods")
make_heatmap("pH_high10",title = "pH highest 10% across periods" )
make_heatmap("pH_low10", title = "pH lowest 10% across periods")
make_heatmap("pH_eventSD",col, title = "pH event SD across periods")
make_heatmap("pH_seasonalSD",col, title = "pH seasonal SD across periods")

#DO heatmaps
make_heatmap("DO_mean", title = "DO mean across periods")
make_heatmap("DO_high10",title = "DO highest 10% across periods" )
make_heatmap("DO_low10", title = "DO lowest 10% across periods")
make_heatmap("DO_eventSD",col, title = "DO event SD across periods")
make_heatmap("DO_seasonalSD",col, title = "DO seasonal SD across periods")


#-----------------------------------------------------------------------------

#############
#Regressions
#############

#Supplements heatmaps by looking at the correlation between historic and future 
#time periods for a given summary stat. Each point is an MPA, and values for given 
#summary stat within specified time periods are on the axes.

make_regression <- function(sumstat, period1, period2){
  
  plot_matrix <- sum %>%
    filter(period == period1 | period == period2) %>%
    select(sumstat, period, NAME, File, OBJECTID, region) %>%
    pivot_wider(names_from = period, values_from = sumstat) 
  
  ggplot(data = plot_matrix, aes_string(x = period1, y = period2)) +
    stat_poly_line(colour = "black") +
    stat_poly_eq(use_label(c("eq", "R2"))) +
    geom_point(aes(colour = region), alpha = 0.7) +
    theme_classic()
  
}

#temp
make_regression("Temp_mean", "historic", "midcen")
make_regression("Temp_mean", "historic", "endcen")
make_regression("T_seasonalSD", "historic", "midcen")
make_regression("T_seasonalSD", "historic", "endcen")
make_regression("T_eventSD", "historic", "midcen")
make_regression("T_eventSD", "historic", "endcen")

#pH
make_regression("pH_mean", "historic", "midcen")
make_regression("pH_mean", "historic", "endcen")
make_regression("pH_seasonalSD", "historic", "midcen")
make_regression("pH_seasonalSD", "historic", "endcen")
make_regression("pH_eventSD", "historic", "midcen")
make_regression("pH_eventSD", "historic", "endcen")

#DO
make_regression("DO_mean", "historic", "midcen")
make_regression("DO_mean", "historic", "endcen")
make_regression("DO_seasonalSD", "historic", "midcen")
make_regression("DO_seasonalSD", "historic", "endcen")
make_regression("DO_eventSD", "historic", "midcen")
make_regression("DO_eventSD", "historic", "endcen")


# -------------------------------------------------------------------------

#############
#anom maps
############

#there are some errors w not accounting 0 event days accounted for in new version

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

CA_shp <- st_read(here("./data/rawdata/shp/CA_Counties/CA_Counties_TIGER2016.shp"))


make_anom_map <- function(mpa_summary_df, sumstat, periodt, 
                          palette = "YlOrRd", diff = FALSE){
  
  #converting df to spatial data format
  anom_points <- st_as_sf(
    mpa_summary_df %>% filter(period == periodt),
    coords = c("degx","degy"),
    crs = 4326) %>%
    st_transform(st_crs(CA_shp))
  
  #creating period diff df: calculating diff btwn time points for given sumstat
  period_diff <- mpa_summary_df %>%
    pivot_wider(names_from = period,
                values_from = sumstat,
                id_cols = c(File, degx, degy)) %>%
    mutate(mid_hist_diff = midcen - historic,
           end_hist_diff = endcen - historic,
           end_mid_diff = endcen - midcen)
  #converting period diff df to spatial data format
  period_diff_anom_points <- st_as_sf(period_diff,coords = c("degx","degy"), crs = 4326) %>%
    st_transform(st_crs(CA_shp))
  
  #finding equal interval breaks based on all time periods
  all_breaks <- classInt::classIntervals(mpa_summary_df[[sumstat]], 8, "equal")
  all_diff_breaks <- pivot_longer(period_diff, c(mid_hist_diff,end_hist_diff,end_mid_diff))
  all_diff_breaks <- classInt::classIntervals(all_diff_breaks$value, 10, "equal")
  
  #if diff parameter is false: create regular maps of sumstat
  if (diff == FALSE){
    tm_shape(CA_shp) + #basemap
      tm_polygons(fill = "#ccebc5", lwd = 0) +
      
      tm_shape(anom_points) +
      
      tm_dots(fill = sumstat, size = 0.2,
              fill.scale = tm_scale_intervals(
                values = palette,
                breaks = all_breaks[["brks"]])) +
      
      tm_layout(bg.color = "#a6cee3",
                legend.title.size = 1,
                legend.text.size = .8) 
    
    #if diff parameter is true: create 3 maps of diff between each time period
  } else {
    #mid-hist diff
    mid_hist <- tm_shape(CA_shp) + #basemap
      tm_polygons(fill = "#ccebc5", lwd = 0) +
      
      tm_shape(period_diff_anom_points) +
      tm_dots(fill = "mid_hist_diff", size = 0.4,
              fill.scale = tm_scale_intervals(
                values = palette,
                breaks = all_diff_breaks[["brks"]])) +
      
      tm_layout(bg.color = "#a6cee3",
                legend.title.size = 1,
                legend.text.size = .8) 
    
    #end-hist diff
    end_hist <- tm_shape(CA_shp) + #basemap
      tm_polygons(fill = "#ccebc5", lwd = 0) +
      
      tm_shape(period_diff_anom_points) +
      tm_dots(fill = "end_hist_diff", size = 0.4,
              fill.scale = tm_scale_intervals(
                values = palette,
                breaks = all_diff_breaks[["brks"]])) +
      
      tm_layout(bg.color = "#a6cee3",
                legend.title.size = 1,
                legend.text.size = .8)
    
    #end-mid diff
    end_mid <- tm_shape(CA_shp) + #basemap
      tm_polygons(fill = "#ccebc5", lwd = 0) +
      
      tm_shape(period_diff_anom_points) +
      tm_dots(fill = "end_mid_diff", size = 0.4, 
              fill.scale = tm_scale_intervals(
                values = palette,
                breaks = all_diff_breaks[["brks"]])) +
      
      tm_layout(bg.color = "#a6cee3",
                legend.title.size = 1,
                legend.text.size = .8) 
    
    return(list(mid_hist = mid_hist, end_mid = end_mid))
  }
}

#TEMP-----------------------------------------------------
#mean annual num events for all periods for temp 
make_anom_map(temp_mpa_summary, sumstat = "meanannual_days_abovethresh",periodt =  "historic")
make_anom_map(temp_mpa_summary, sumstat = "meanannual_days_abovethresh",periodt = "midcen") 
make_anom_map(temp_mpa_summary, sumstat = "meanannual_days_abovethresh",periodt = "endcen")

#mean temp during event for all periods
make_anom_map(temp_mpa_summary, "meanannual_avg_temp_abovethresh", "historic")
make_anom_map(temp_mpa_summary, "meanannual_avg_temp_abovethresh","midcen") 
make_anom_map(temp_mpa_summary, "meanannual_avg_temp_abovethresh", "endcen")

make_anom_map(temp_mpa_summary, "meanannual_days_abovethresh", "historic",diff = TRUE)

#pH---------------------------------------------------------
make_anom_map(pH_mpa_summary, "meanannual_days_belowthresh", "historic")
make_anom_map(pH_mpa_summary, "meanannual_days_belowthresh","midcen") 
make_anom_map(pH_mpa_summary, "meanannual_days_belowthresh","endcen")

#mean pH during event for all periods 
make_anom_map(pH_mpa_summary, "meanannual_avg_pH_belowthresh", "historic",palette = "-YlOrRd")
make_anom_map(pH_mpa_summary, "meanannual_avg_pH_belowthresh","midcen",palette = "-YlOrRd") 
make_anom_map(pH_mpa_summary, "meanannual_avg_pH_belowthresh","endcen",palette = "-YlOrRd")

make_anom_map(pH_mpa_summary, sumstat = "meanannual_days_belowthresh",period = "historic",diff = TRUE)

#DO---------------------------------------------------------
make_anom_map(DO_mpa_summary, "meanannual_days_belowthresh", "historic")
make_anom_map(DO_mpa_summary, "meanannual_days_belowthresh","midcen") 
make_anom_map(DO_mpa_summary, "meanannual_days_belowthresh","endcen")

#mean DO during event for all periods 
make_anom_map(DO_mpa_summary, "meanannual_avg_DO_belowthresh", "historic",palette = "-YlOrRd")
make_anom_map(DO_mpa_summary, "meanannual_avg_DO_belowthresh","midcen",palette = "-YlOrRd") 
make_anom_map(DO_mpa_summary, "meanannual_avg_DO_belowthresh","endcen",palette = "-YlOrRd")

make_anom_map(DO_mpa_summary, sumstat = "meanannual_days_belowthresh",period = "historic",diff = TRUE)





# ---------------------------------------------------------------------

###############
#sev, int, dur
###############


#temp:
temp_sev_int_dur <- function(periodt, site){
  
  temp_all_events %>%
    filter(period == periodt, NAME == site) %>%
    ggplot(aes(julianday, Year, fill = intensity)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                         na.value = "skyblue") +
    theme_classic() +
    ggtitle(paste(periodt, "Heat events at", site)) 
  
}

temp_sev_int_dur_regional <- function(periodt, regionn){
  
  temp_all_events %>%
    #this find mean intensity of a given julian day from all MPAs in a region
    group_by(period, region, Year, julianday) %>%
    summarise(mean_intensity = mean(intensity, na.rm=TRUE)) %>%
    
    filter(period == periodt, region == regionn) %>%
    ggplot(aes(julianday, Year, fill = mean_intensity)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                         na.value = "skyblue") +
    theme_classic() +
    ggtitle(paste("Mean intensity of heat events in", regionn, "during", periodt, "period")) 
  
}

#pH:
pH_sev_int_dur <- function(periodt, site){
  
  pH_all_events %>%
    filter(period == periodt, NAME == site) %>%
    ggplot(aes(julianday, Year, fill = intensity)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                         na.value = "skyblue") +
    theme_classic() +
    ggtitle(paste(periodt, "pH events at", site)) 
  
  
}

pH_sev_int_dur_regional <- function(periodt, regionn){
  
  pH_all_events %>%
    merge(sum[,c("File","region")], by = c("File")) %>% #to get the region
    #this find mean intensity of a given julian day from all MPAs in a region
    group_by(period, region, Year, julianday) %>%
    summarise(mean_intensity = mean(intensity, na.rm=TRUE)) %>%
    
    filter(period == periodt, region == regionn) %>%
    ggplot(aes(julianday, Year, fill = mean_intensity)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                         na.value = "skyblue") +
    theme_classic() +
    ggtitle(paste("Mean intensity of pH events in", regionn, "during", periodt, "period")) 
  
}

#DO
DO_sev_int_dur <- function(periodt, site){
  
  DO_all_events %>%
    filter(period == periodt, NAME == site) %>%
    ggplot(aes(julianday, Year, fill = intensity)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                         na.value = "skyblue") +
    theme_classic() +
    ggtitle(paste(periodt, "DO events at", site)) 
  
  
}

DO_sev_int_dur_regional <- function(periodt, regionn){
  
  DO_all_events %>%
    merge(sum[,c("File","region")], by = c("File")) %>% #to get the region
    #this find mean intensity of a given julian day from all MPAs in a region
    group_by(period, region, Year, julianday) %>%
    summarise(mean_intensity = mean(intensity, na.rm=TRUE)) %>%
    
    filter(period == periodt, region == regionn) %>%
    ggplot(aes(julianday, Year, fill = mean_intensity)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                         na.value = "skyblue") +
    theme_classic() +
    ggtitle(paste("Mean intensity of DO events in", regionn, "during", periodt, "period")) 
  
}

#central CA - all sites within region
temp_sev_int_dur_regional("historic", "centralca")
temp_sev_int_dur_regional("midcen", "centralca")
temp_sev_int_dur_regional("endcen", "centralca")

temp_sev_int_dur_regional("historic", "norca")
temp_sev_int_dur_regional("midcen", "norca")
temp_sev_int_dur_regional("endcen", "norca")

temp_sev_int_dur_regional("historic", "socal")
temp_sev_int_dur_regional("midcen", "socal")
temp_sev_int_dur_regional("endcen", "socal")

temp_sev_int_dur_regional("historic", "channel")
temp_sev_int_dur_regional("midcen", "channel")
temp_sev_int_dur_regional("endcen", "channel")

pH_sev_int_dur_regional("historic", "centralca")
pH_sev_int_dur_regional("midcen", "centralca")
pH_sev_int_dur_regional("endcen", "centralca")


pH_sev_int_dur_regional("historic", "norca")
pH_sev_int_dur_regional("midcen", "norca")
pH_sev_int_dur_regional("endcen", "norca")


#southern CA
pH_sev_int_dur_regional("historic", "socal")
pH_sev_int_dur_regional("midcen", "socal")
pH_sev_int_dur_regional("endcen", "socal")

pH_sev_int_dur_regional("historic", "channel")
pH_sev_int_dur_regional("midcen", "channel")
pH_sev_int_dur_regional("endcen", "channel")

#central CA
DO_sev_int_dur_regional("historic", "centralca")
DO_sev_int_dur_regional("midcen", "centralca")
DO_sev_int_dur_regional("endcen", "centralca")

#northern CA
DO_sev_int_dur_regional("historic", "norca")
DO_sev_int_dur_regional("midcen", "norca")
DO_sev_int_dur_regional("endcen", "norca")

#southern CA
DO_sev_int_dur_regional("historic", "socal")
DO_sev_int_dur_regional("midcen", "socal")
DO_sev_int_dur_regional("endcen", "socal")

#channel
DO_sev_int_dur_regional("historic", "channel")
DO_sev_int_dur_regional("midcen", "channel")
DO_sev_int_dur_regional("endcen", "channel")


# -------------------------------------------------------------------------

####################
#multiple stressor
####################

#Frequency an MPA encounters multiple events. Note: an event is 1+ days, whereas previously it was defined as 2+ days. Thus, total number of possible events for one stressor in a year is 365.

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
  
  #setting y range for plot
  ifelse(periodt == "historical", yrange <- c(2000,2020), 
         ifelse(periodt == "midcen", yrange <- c(2040,2060), 
                yrange <- c(2080,2100)))
  
  #plot
  all_events %>%
    merge(num_mpa, by = "region") %>%
    group_by(julianday, period, Year, region) %>%
    filter(multi_stressor == TRUE) %>%
    reframe(multi_prob = mean(n_distinct(File), na.rm=TRUE)/num_mpa) %>%
    distinct(julianday, period, Year, region, multi_prob) %>%
    ungroup() %>%
    
    filter(period == periodt, region == regionn) %>%
    ggplot(aes(julianday, Year, fill = multi_prob)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    theme_classic() +
    ylim(yrange) +
    xlim(1,365) +
    ggtitle(paste("Frequency of MPAs with 2+ stressor events","in", periodt, "period", "in", regionn)) +
    theme(panel.background = element_rect(fill = "skyblue"))
  
}
multi_event("historic","centralca")
multi_event("midcen","centralca")
multi_event("endcen","centralca")

multi_event("historic","norca")
multi_event("midcen","norca")
multi_event("endcen","norca")

multi_event("historic","socal")
multi_event("midcen","socal")
multi_event("endcen","socal")

multi_event("historic","channel")
multi_event("midcen","channel")
multi_event("endcen","channel")

make_time_series <- function(periodt, site, variable){
  
  mpa %>% 
    filter(period == periodt, 
           NAME == site) %>%
    mutate(sliding_sd = roll_sd(!!sym(variable), 30, fill = NA)) %>% 
    group_by(julianday) %>%
    mutate(clim_mean = mean(!!sym(variable), na.rm = TRUE),
           clim_sd = mean(sliding_sd, na.rm = TRUE),
           clim_upr = clim_mean + 2 * clim_sd,
           clim_lwr = clim_mean - 2 * clim_sd) %>% 
    ungroup() %>%
    
    ggplot(aes(julianday)) +
    geom_ribbon(aes(ymin = clim_lwr, ymax = clim_upr),
                fill = "black", alpha = 0.75) +
    geom_line(aes(y = !!sym(variable), color = Year, group = Year),
              alpha = 0.5) +
    geom_line(aes(y = clim_mean), size = 1, color = "blue") +
    scale_color_viridis_c() +
    theme_classic() +
    ggtitle(paste(periodt, variable, "at", site))
}




#this fig shows time series in julian days w/ envelope wrt given period's SD (NOT historic, as it should be for calculating anomalous event!)


make_temp_time_series <- function(periodt, site, title){
  
  mpa %>% 
    filter(period == periodt, 
           NAME == site) %>%
    mutate(sliding_sd = roll_sd(Temp, 30, fill = NA)) %>% 
    group_by(julianday) %>%
    mutate(clim_mean = mean(Temp, na.rm = TRUE),
           clim_sd = mean(sliding_sd, na.rm = TRUE),
           clim_upr = clim_mean + 2 * clim_sd,
           clim_lwr = clim_mean - 2 * clim_sd) %>% 
    ungroup() %>%
    
    ggplot(aes(julianday)) +
    geom_ribbon(aes(ymin = clim_lwr, ymax = clim_upr),
                fill = "black", alpha = 0.75) +
    geom_line(aes(y = Temp, color = Year, group = Year),
              alpha = 0.5) +
    geom_line(aes(y = clim_mean), size = 1, color = "blue") +
    scale_color_viridis_c() +
    theme_classic() +
    ggtitle(title)
}

make_pH_time_series <- function(periodt, site, title){
  
  mpa %>% 
    filter(period == periodt, 
           NAME == site) %>%
    mutate(sliding_sd = roll_sd(pH, 30, fill = NA)) %>% 
    group_by(julianday) %>%
    mutate(clim_mean = mean(pH, na.rm = TRUE),
           clim_sd = mean(sliding_sd, na.rm = TRUE),
           clim_upr = clim_mean + 2 * clim_sd,
           clim_lwr = clim_mean - 2 * clim_sd) %>% 
    ungroup() %>%
    
    ggplot(aes(julianday)) +
    geom_ribbon(aes(ymin = clim_lwr, ymax = clim_upr),
                fill = "black", alpha = 0.75) +
    geom_line(aes(y = pH, color = Year, group = Year),
              alpha = 0.5) +
    geom_line(aes(y = clim_mean), size = 1, color = "blue") +
    scale_color_viridis_c() +
    theme_classic() +
    ggtitle(title)
}

make_DO_time_series <- function(periodt, site, title){
  
  mpa %>% 
    filter(period == periodt, 
           NAME == site) %>%
    mutate(sliding_sd = roll_sd(DO, 30, fill = NA)) %>% 
    group_by(julianday) %>%
    mutate(clim_mean = mean(DO, na.rm = TRUE),
           clim_sd = mean(sliding_sd, na.rm = TRUE),
           clim_upr = clim_mean + 2 * clim_sd,
           clim_lwr = clim_mean - 2 * clim_sd) %>% 
    ungroup() %>%
    
    ggplot(aes(julianday)) +
    geom_ribbon(aes(ymin = clim_lwr, ymax = clim_upr),
                fill = "black", alpha = 0.75) +
    geom_line(aes(y = DO, color = Year, group = Year),
              alpha = 0.5) +
    geom_line(aes(y = clim_mean), size = 1, color = "blue") +
    scale_color_viridis_c() +
    theme_classic() +
    ggtitle(title)
  
}

#southern
make_temp_time_series("historic","South La Jolla SMR", title = "Historic Temp at South La Jolla SMR")
make_temp_time_series("midcen","South La Jolla SMR", title = "Midcen Temp at South La Jolla SMR") 
make_temp_time_series("endcen","South La Jolla SMR", title = "Endcen Temp at South La Jolla SMR")

#channel
make_temp_time_series("historic","Anacapa Island FMR", title = "Historic Temp at Anacapa Island FMR")
make_temp_time_series("midcen","Anacapa Island FMR", title = "Midcen Temp at Anacapa Island FMR") 
make_temp_time_series("endcen","Anacapa Island FMR", title = "Endcen Temp at Anacapa Island FMR")

#central
make_pH_time_series("historic","Ano Nuevo SMR", title = "Historic pH at Ano Nuevo") 
make_pH_time_series("midcen","Ano Nuevo SMR", title = "Midcen pH at Ano Nuevo") 
make_pH_time_series("endcen","Ano Nuevo SMR", title = "Endcen pH at Ano Nuevo")

#northern
make_pH_time_series("historic","Point Arena SMR", title = "Historic pH at Point Arena SMR") 
make_pH_time_series("midcen","Point Arena SMR", title = "Midcen pH at Point Arena SMR") 
make_pH_time_series("endcen","Point Arena SMR", title = "Endcen pH at Point Arena SMR")

#southern
make_pH_time_series("historic","South La Jolla SMR", title = "Historic pH at South La Jolla SMR") 
make_pH_time_series("midcen","South La Jolla SMR", title = "Midcen pH at South La Jolla SMR") 
make_pH_time_series("endcen","South La Jolla SMR", title = "Endcen pH at South La Jolla SMR")

#channel
make_pH_time_series("historic","Anacapa Island FMR", title = "Historic pH at Anacapa Island FMR") 
make_pH_time_series("midcen","Anacapa Island FMR", title = "Midcen pH at Anacapa Island FMR") 
make_pH_time_series("endcen","Anacapa Island FMR", title = "Endcen pH at Anacapa Island FMR")



