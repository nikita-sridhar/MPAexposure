# mod setup ---------------------------------------------------------------
mod_raw <- read_csv("data/processeddata/model/mod_all30m.csv")
mod <- read_csv("data/processeddata/model/mod_all30m.csv") %>%
  rename("T" = T_surf,
         "pH" = pH_surf,
         "DO" = DO_surf) %>%
  mutate(time_utc = lubridate::ymd(paste(Year, Month, Day, sep="-"))) %>%
  select(File, time_utc, T, pH, DO) %>%
  #adding regions
  mutate("region" = case_when(
    File %in% c("tphdo_sta_7.csv","tphdo_sta_14.csv","tphdo_sta_kristy1.csv") ~ "Channel Islands",
    File %in% c("tphdo_sta_19.csv","tphdo_sta_kristy4.csv") ~ "Central CA",
    File %in% c("tphdo_sta_30.csv", "tphdo_sta_31.csv","tphdo_sta_44.csv","tphdo_sta_kristy2.csv","tphdo_sta_kristy3.csv") ~ "Southern CA",
    File %in% c("tphdo_sta_45.csv","tphdo_sta_56.csv","tphdo_sta_kristy5.csv","tphdo_sta_kristy6.csv") ~ "Northern CA"),
    
    #renaming files
    File = str_sub(File, start = 11, end = -5),
    
    #add month/yr/day
    Year = year(time_utc),
    Month = month(time_utc),
    Day = day(time_utc),
    julianday = yday(time_utc))

# obs setup ---------------------------------------------------------------

obs <- load("data/rawdata/obs/for model-obs comparison/clean_all_light.RData")
obs <- light %>%
  rename(site = dataset_id) %>%
  #filtering all datasets based on prev exploration of longest best quality across regions
  filter(site %in% c(7,14,19,30,31,44,45,40,56)) %>%
  #dataset 40 is kristy, but contains multiple coords, so separating those here
  within(site[site == 40 & latitude == 33.4169 & longitude == -118.3977] <- 'kristy1') %>%
  within(site[site == 40 & latitude == 33.5421 & longitude == -117.7947] <- 'kristy2') %>%
  within(site[site == 40 & latitude == 35.2412 & longitude == -120.8955] <- 'kristy3') %>%
  within(site[site == 40 & latitude == 36.0685 & longitude == -121.6016] <- 'kristy4') %>%
  within(site[site == 40 & latitude == 38.946 & longitude == -123.7389] <- 'kristy5') %>%
  within(site[site == 40 & latitude == 39.2711 & longitude == -123.7947] <- 'kristy6') 
rm(light)

obs_means <- obs %>% #calculating daily avgs for temp, ph, do
  group_by(site, time_utc, longitude, latitude) %>%
  summarize(depth_m = mean(depth_m, na.rm=TRUE),
            T = mean(t_C, na.rm = TRUE), 
            pH = mean(pH_total, na.rm = TRUE), 
            DO = mean(do_umolkg, na.rm = TRUE),
            .groups = "drop") %>%
  #adding regions
  mutate("region" = case_when(
    site %in% c("7","14","kristy1") ~ "Channel Islands",
    site %in% c("19","kristy4") ~ "Central CA",
    site %in% c("30", "31","44","kristy2","kristy3") ~ "Southern CA",
    site %in% c("45","56","kristy5","kristy6") ~ "Northern CA")) %>%
  
  #add date/month/year
  mutate(Year = year(time_utc),
         Month = month(time_utc),
         Day = day(time_utc),
         julianday = yday(time_utc))

#turning NaN into NA
obs_means$T[is.nan(obs_means$T)]<-NA
obs_means$pH[is.nan(obs_means$pH)]<-NA
obs_means$DO[is.nan(obs_means$DO)]<-NA

obs_sites <- obs_means %>%
  distinct(site, longitude, latitude, region) %>%
  filter(
    site != 44 |
      (latitude == 34.324 & longitude == -120.815)
  ) 

obs_sites <- obs_means %>%
  distinct(site, longitude, latitude, region) %>%
  group_by(site) %>%
  mutate(
    site = if_else(
      site == 44,
      paste0(site, "_", row_number()),
      as.character(site)
    )
  ) %>%
  ungroup()

file_44 <- obs_means %>%
  filter(site == 44) %>%
  mutate(version = paste0("Lat: ", latitude, ", Lon: ", longitude)) %>%
  pivot_longer(
    cols = c(T, pH, DO),
    names_to = "variable",
    values_to = "value"
  )

ggplot(file_44, aes(x = time_utc, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Time Series for File 44",
    x = "Date",
    y = "Value",
    color = "Latitude / Longitude"
  ) +
  theme_bw()

write.csv(obs_sites, file = "./data/processeddata/obs_sites.csv")

obs_summary <- obs_means %>%
  group_by(File,region) %>%
  summarise(temp_count = sum(!is.na(T)), #num of observations
            pH_count = sum(!is.na(pH)),
            DO_count = sum(!is.na(DO)))


# plotting map of obs sites -----------------------------------------------
obs_sf <- st_as_sf(obs_means, coords = c("longitude", "latitude"), crs = 4326)


#plotting obs sites (might need to run map part from mpaexposure.qmd first)
make_obs_mpa_map <- function(bbox, title, legend) {
  #CA baselayer
  tm_shape(CA_shp, bbox = bbox) +
    tm_polygons(fill = "region",
                fill.scale = tm_scale_categorical(values = region_cols),
                fill_alpha = 0.5,
                fill.legend = tm_legend(show = FALSE),
                lwd = 0) +
    #channel islands layer to color correctly
    tm_shape(channel_clip, bbox = bbox) +
    tm_polygons(fill = "#66c2a5",
                fill_alpha = 0.5,
                lwd = 0) +
    #mpas
    tm_shape(mpa_shp_filtered) +
    tm_polygons(fill = "region",
                fill.scale = tm_scale_categorical(values = region_cols),
                lwd = 0,
                fill.legend = tm_legend(show = legend)) +
    tm_graticules(alpha = 0.1) +
    tm_layout(frame = FALSE,
              inner.margins = c(0.03, 0.03, 0.03, 0.03),
              component.autoscale = FALSE) +
    #add obs time series:
    tm_shape(obs_sf) +
    tm_dots()+
    tm_title(title)
}

norcal <- make_obs_mpa_map(c(-125, 37, -122, 42), "Northern CA", FALSE)
centralca <- make_obs_mpa_map(c(-123, 34.5, -120, 37.5), "Central CA",FALSE)
socal <- make_obs_mpa_map(c(-121, 32.5, -117, 34.5), "Southern CA & Ch. Isl",TRUE)

tmap_arrange(socal,centralca,norcal,nrow = 1)


# pdf/cdf analysis --------------------------------------------------------

#pdf - NorCA
norcal_temp <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(T)) +
  geom_density(data = obs_means%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(T)) +
  ggtitle("Northern CA T fit") +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
norcal_ph <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(pH)) +
  geom_density(data = obs_means%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(pH)) +
  ggtitle("Northern CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
norcal_do <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(DO)) +
  geom_density(data = obs_means%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(DO)) +
  ggtitle("Northern CA DO fit")+
  theme_classic()+
  labs(x = "DO", y = "Probability density")

#pdf - CenCA
cenca_temp <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(T)) +
  geom_density(data = obs_means%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(T)) +
  ggtitle("Central CA T fit")  +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
cenca_ph <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(pH)) +
  geom_density(data = obs_means%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(pH)) +
  ggtitle("Central CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
cenca_do <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(DO)) +
  geom_density(data = obs_means%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(DO)) +
  ggtitle("Central CA DO fit")+
  theme_classic()+
  labs(x = "DO", y = "Probability density")



#pdf - SoCA
socal_temp <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Southern CA"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs%>%filter(region == "Southern CA"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Southern CA T fit") +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
socal_ph <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Southern CA"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs%>%filter(region == "Southern CA"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Southern CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
socal_do <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Southern CA"), fill = "green", alpha = 0.5, aes(mod_DO)) +
  geom_density(data = obs%>%filter(region == "Southern CA"), fill = "red", alpha = 0.4, aes(obs_DO)) +
  ggtitle("Southern CA DO fit") +
  theme_classic()+
  labs(x = "DO", y = "Probability density")


#pdf - channel isl
channel_temp <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Channel Islands"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs%>%filter(region == "Channel Islands"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Channel Islands T fit") +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
channel_ph <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Channel Islands"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs%>%filter(region == "Channel Islands"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Channel Islands pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")

#only adding legend to this one bc in ggarrange it will apply to all
channel_do <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Channel Islands"),
               aes(x = mod_DO, fill="mod"),
               alpha = 0.5) +
  geom_density(data = obs%>%filter(region == "Channel Islands"), 
               aes(x = obs_DO, fill = "obs"), 
               alpha = 0.4) +
  
  scale_fill_manual(name = " ",
                    values = c("mod"="green","obs"="red"),
                    labels = c("Model","Observation")) +
  ggtitle("Channel Islands DO fit") +
  theme_classic()+
  labs(x = "DO", y = "Probability density") 


ggarrange(norcal_temp, norcal_ph, norcal_do, cenca_temp, cenca_ph, cenca_do, 
          socal_temp, socal_ph, socal_do, channel_temp, channel_ph, channel_do,
          labels = c("A","B","C","D", "E", "F", "G", "H", "I", "K", "L", "M"),
          ncol = 3, nrow = 4, 
          common.legend = TRUE,
          legend = "bottom")


#cdf
ggplot() + 
  stat_ecdf(data = mod%>%filter(region == "Northern CA"), color = "green", alpha = 0.5, aes(mod_T)) +
  stat_ecdf(data = obs%>%filter(region == "Northern CA"), color = "red", alpha = 0.4, aes(obs_T))


# Summary stat comparison -------------------------------------------------
#note: interpolation function has been edited from ensemble_mean to account for large
#data gaps in obs data (i.e. sometimes only one dataset for temp and no ph/do)
#should try this new code w actual model projections - might be better and slightly diff

#OBS

#"climatology"
obs_clim <- obs_means %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T, na.rm = TRUE), 
            pH_clim = mean(pH, na.rm = TRUE), 
            DO_clim =   mean(DO, na.rm = TRUE),
            .groups = "drop")



mod_clim_test <- mod %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T, na.rm = TRUE), 
            pH_clim = mean(pH, na.rm = TRUE), 
            DO_clim =   mean(DO, na.rm = TRUE),
            .groups = "drop")


#interpolation
obslist = unique(obs_means$File) #just to get mpa names doesn't matter that it's ipsl
obs_site = rep(NA, 365*14) #14 obs sites. previously mpas
julianday = rep(1:365, 14)
interp = rep(NA, 365*14)

#Set up a vector of julian day assignment for the 15th of each month and the first and last day of the year
x_in <- yday(as.Date(c("2004-01-01", 
                       "2004-01-15","2004-02-15","2004-03-15","2004-04-15",
                       "2004-05-15","2004-06-15","2004-07-15","2004-08-15",
                       "2004-09-15","2004-10-15","2004-11-15","2004-12-15", 
                       "2004-12-31")))

# creating a list of all the days of the year to interpolate to.
x_out <- (1:365) 

interpolate <- function(clim, variable){
  
  # Create name for the coverage column
  n_months_name <- paste0("n_", sub("_clim$", "", variable), "_months")
  
  #create output vectors for sites
  obs_site <- rep(NA, 365 * length(obslist))
  julianday <- rep(1:365, length(obslist))
  interp <- rep(NA, 365 * length(obslist))
  n_months <- rep(NA, 365 * length(obslist))
  
  for (i in seq_along(obslist)){ #for each obs site
    
    #get monthly clim per site
    d <- clim %>% 
      filter(File == obslist[i]) %>% 
      select(File, Month, !!sym(variable)) %>%
      arrange(Month)
    
    #extract monthly values
    y_month <- d[[variable]]
    
    #identify months w usable data
    valid <- !is.na(y_month)
    n_valid <- sum(valid)
    
    #store num of available months for every day
    n_months[((i-1) * 365 + 1):(i * 365)] <- n_valid
    
    if (n_valid >= 2){
      
      #use only months w data
      x_valid <- x_in[1:12][valid]
      y_valid <- y_month[valid]
      
      #Add Dec --> Jan wraparound values
      Dec31 <- (16/30) * y_valid[length(y_valid)] +
        (14/30) * y_valid[1]
      
      Jan1 <- (14/30) * y_valid[length(y_valid)] +
        (16/30) * y_valid[1]
      
      y_in <- c(Jan1, y_valid, Dec31)
      
      x_interp <- c(
        x_in[1],
        x_valid,
        x_in[14]
      )
      
      #Interpolate
      interp_list <- approx(
        x = x_interp,
        y = y_in,
        xout = x_out,
        rule = 2
      )
      
      interp[((i-1) * 365 + 1):(i * 365)] <- interp_list$y
      
    } else if (n_valid == 1) {
      
      #only one observation: use it for the entire year
      interp[((i-1) * 365 + 1):(i * 365)] <- y_month[valid]
      
    } else {
      
      #no available data: leave as NA
      interp[((i-1) * 365 + 1):(i * 365)] <- NA
    }
    
    # Assign site name
    obs_site[((i - 1) * 365 + 1):(i * 365)] <- obslist[i]
  }
  
  
  #Create output data frame
  df <- data.frame(
    obs_site,
    julianday,
    interp,
    n_months
  ) %>%
    rename(File = obs_site) %>%
    rename(!!sym(variable) := interp) %>%
    rename(!!n_months_name := n_months)
  
  return(df)
}

obs_temp_interp <- interpolate(obs_clim, "T_clim")
obs_pH_interp <- interpolate(obs_clim, "pH_clim")
obs_DO_interp <- interpolate(obs_clim, "DO_clim")


mod_temp_interp <- interpolate(mod_clim_test, "T_clim")
mod_pH_interp <- interpolate(mod_clim_test, "pH_clim")
mod_DO_interp <- interpolate(mod_clim_test, "DO_clim")



obs_interp_all <- obs_temp_interp %>%
  left_join(obs_pH_interp, by = c("File", "julianday")) %>%
  left_join(obs_DO_interp, by = c("File", "julianday"))

mod_interp_all <- mod_temp_interp %>%
  left_join(mod_pH_interp, by = c("File", "julianday")) %>%
  left_join(mod_DO_interp, by = c("File", "julianday"))


#make summary stats:

#calculate seasonal SD
obs_seasonal_SD <- obs_clim %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

mod_seasonal_SD <- mod_clim_test %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))



calculate_sumstat <- function(obs, interp_all, seasonalSD){
  
  sumstat <- obs %>%
    
    left_join(interp_all, by = c("File", "julianday"))  %>%
    
    #subtract actual values - interpolated climatology values 
    #filtered for only upwelling months!!
    mutate(temp_deviation = T - T_clim, #this should be for the given julian day, right?
           pH_deviation = pH - pH_clim,
           DO_deviation = DO - DO_clim) %>%
    filter(Month %in% c(5,6,7,8,9)) %>%
    
    #find event/seasonal SD and other sum stats 
    group_by(File) %>%
    mutate(T_eventSD = sd(temp_deviation, na.rm = TRUE),
           pH_eventSD = sd(pH_deviation, na.rm = TRUE),
           DO_eventSD = sd(DO_deviation, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-T_clim, -pH_clim, -DO_clim, -temp_deviation,
           -pH_deviation, -DO_deviation) %>%
    
    #merge seasonal SD 
    left_join(seasonalSD, by = "File") %>%
    
    #adding other summary stats - mean, low 10th and upper 10th percentiles
    group_by(File) %>%
    
    mutate(across(c(T, DO, pH), 
                  list(mean = ~ mean(.x, na.rm = TRUE),
                       low10 = ~ quantile(.x, 0.1, na.rm = TRUE),
                       high10 = ~ quantile(.x, 0.9, na.rm = TRUE)))) %>%
    
    # to get to scale of one row per mpa
    distinct(File, .keep_all = TRUE) %>% ungroup() %>%
    select(-julianday, -time_utc, -Year,-Month, -Day) 
  
  return(sumstat)
}



obs_sumstat <- calculate_sumstat(obs_means, obs_interp_all, obs_seasonal_SD)
mod_sumstat <- calculate_sumstat(mod, mod_interp_all, mod_seasonal_SD)


comparison <- obs_sumstat %>%
  select(File, everything()) %>%
  rename_with(~ paste0(.x, "_obs"), -File) %>%
  inner_join(
    mod_sumstat %>%
      rename_with(~ paste0(.x, "_mod"), -File),
    by = "File"
  )

# Summary statistics to compare
stats <- c(
  "T_mean",
  "T_low10",
  "T_high10",
  "pH_mean",
  "pH_low10",
  "pH_high10",
  "DO_mean",
  "DO_low10",
  "DO_high10",
  "T_eventSD",
  "pH_eventSD",
  "DO_eventSD",
  "T_seasonalSD",
  "pH_seasonalSD",
  "DO_seasonalSD"
)

# Run regression for each summary statistic
regression_results <- map_dfr(stats, function(stat) {
  
  obs_col <- paste0(stat, "_obs")
  mod_col <- paste0(stat, "_mod")
  
  dat <- comparison %>%
    select(File, all_of(obs_col), all_of(mod_col)) %>%
    drop_na()
  
  model <- lm(
    reformulate(obs_col, response = mod_col),
    data = dat
  )
  
  tidy(model) %>%
    filter(term == obs_col) %>%
    transmute(
      statistic = stat,
      n = nrow(dat),
      slope = estimate,
      slope_SE = std.error,
      p_value = p.value,
      R2 = summary(model)$r.squared
    )
})

regression_results

plot_data <- obs_sumstat %>%
  select(File, all_of(stats)) %>%
  rename_with(~ paste0(.x, "_obs"), -File) %>%
  inner_join(
    mod_sumstat %>%
      select(File, all_of(stats)) %>%
      rename_with(~ paste0(.x, "_mod"), -File),
    by = "File"
  ) %>%
  pivot_longer(
    cols = -File,
    names_to = c("statistic", "type"),
    names_pattern = "(.*)_(obs|mod)$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
  drop_na(obs, mod)


ggplot(plot_data, aes(x = obs, y = mod)) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  
  geom_point() +
  
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  
  facet_wrap(
    ~ statistic,
    scales = "free"
  ) +
  
  labs(
    x = "Observed",
    y = "Modeled"
  ) +
  
  theme_classic()









