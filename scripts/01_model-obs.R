#File created 11/6/2023
#Authored by Nikita Sridhar
#Comparison of model and observational T/pH/DO data using PDFs and CDFs to determine
#model fit

library(tidyverse)
library(ggpubr)
library(lubridate)



# mod setup ---------------------------------------------------------------

mod <- read_csv("data/processeddata/model/mod_all30m.csv") %>%
  rename(mod_T = T_surf,
         mod_pH = pH_surf,
         mod_DO = DO_surf) %>%
  mutate(time_utc = lubridate::ymd(paste(Year, Month, Day, sep="-"))) %>%
  select(File, time_utc, mod_T, mod_pH, mod_DO) %>%
  #adding regions
  mutate("region" = case_when(
    File %in% c("tphdo_sta_7.csv","tphdo_sta_14.csv","tphdo_sta_kristy1.csv") ~ "Channel Islands",
    File %in% c("tphdo_sta_19.csv","tphdo_sta_kristy4.csv") ~ "Central CA",
    File %in% c("tphdo_sta_30.csv", "tphdo_sta_31.csv","tphdo_sta_44.csv","tphdo_sta_kristy2.csv","tphdo_sta_kristy3.csv") ~ "Southern CA",
    File %in% c("tphdo_sta_45.csv","tphdo_sta_56.csv","tphdo_sta_kristy5.csv","tphdo_sta_kristy6.csv") ~ "Northern CA"
  ))
  

# obs setup ---------------------------------------------------------------

obs <- load("data/rawdata/obs/for model-obs comparison/clean_all_light.RData")
obs <- light %>%
  rename(File = dataset_id) %>%
  #filtering all datasets based on prev exploration of longest best quality across regions
  filter(File %in% c(7,14,19,30,31,44,45,40,56)) %>%
  #dataset 40 is kristy, but contains multiple coords, so separating those here
  within(File[File == 40 & latitude == 33.4169 & longitude == -118.3977] <- 'kristy1') %>%
  within(File[File == 40 & latitude == 33.5421 & longitude == -117.7947] <- 'kristy2') %>%
  within(File[File == 40 & latitude == 35.2412 & longitude == -120.8955] <- 'kristy3') %>%
  within(File[File == 40 & latitude == 36.0685 & longitude == -121.6016] <- 'kristy4') %>%
  within(File[File == 40 & latitude == 38.946 & longitude == -123.7389] <- 'kristy5') %>%
  within(File[File == 40 & latitude == 39.2711 & longitude == -123.7947] <- 'kristy6') 
rm(light)

obs_means <- obs %>% #calculating daily avgs for temp, ph, do
  group_by(File, time_utc, longitude, latitude) %>%
  summarize(depth_m = mean(depth_m, na.rm=TRUE),
            obs_T = mean(t_C, na.rm = TRUE), 
            obs_pH = mean(pH_total, na.rm = TRUE), 
            obs_DO = mean(do_umolkg, na.rm = TRUE),
            .groups = "drop") %>%
  #adding regions
  mutate("region" = case_when(
    File %in% c("7","14","kristy1") ~ "Channel Islands",
    File %in% c("19","kristy4") ~ "Central CA",
    File %in% c("30", "31","44","kristy2","kristy3") ~ "Southern CA",
    File %in% c("45","56","kristy5","kristy6") ~ "Northern CA")) %>%
  
  #add date/month/year
  mutate(Year = year(time_utc),
         Month = month(time_utc),
         Day = day(time_utc),
         julianday = yday(time_utc))

#turning NaN into NA
obs_means$obs_T[is.nan(obs_means$obs_T)]<-NA
obs_means$obs_pH[is.nan(obs_means$obs_pH)]<-NA
obs_means$obs_DO[is.nan(obs_means$obs_DO)]<-NA

#write.csv(obs_means, file = "./data/processeddata/obs_for_comparison.csv")

obs_summary <- obs_means %>%
  group_by(File,region) %>%
  summarise(m = mean(region),
            temp_count = sum(!is.na(obs_T)), #num of observations
            pH_count = sum(!is.na(obs_pH)),
            DO_count = sum(!is.na(obs_DO)))


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
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs_meanss%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Northern CA T fit") +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
norcal_ph <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs_means%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Northern CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
norcal_do <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(mod_DO)) +
  geom_density(data = obs_means%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(obs_DO)) +
  ggtitle("Northern CA DO fit")+
  theme_classic()+
  labs(x = "DO", y = "Probability density")

#pdf - CenCA
cenca_temp <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Central CA T fit")  +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
cenca_ph <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Central CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
cenca_do <- ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(mod_DO)) +
  geom_density(data = obs%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(obs_DO)) +
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

#"climatology"
obs_clim <- obs_means %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(obs_T, na.rm = TRUE), 
            pH_clim = mean(obs_pH, na.rm = TRUE), 
            DO_clim =   mean(obs_DO, na.rm = TRUE),
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


obs_interp_all <- obs_temp_interp %>%
  left_join(obs_pH_interp, by = c("File", "julianday")) %>%
  left_join(obs_DO_interp, by = c("File", "julianday"))



#make summary stats:

#calculate seasonal SD
obs_seasonal_SD <- obs_clim %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))



obs_calculate_sumstat <- function(obs, interp_all, seasonalSD){
  
  sumstat <- obs %>%
    
    merge(interp_all, by = c("File", "julianday")) %>%
    
    #subtract actual values - interpolated climatology values 
    #filtered for only upwelling months!!
    mutate(temp_deviation = obs_T - T_clim, #this should be for the given julian day, right?
           pH_deviation = obs_pH - pH_clim,
           DO_deviation = obs_DO - DO_clim) %>%
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
    merge(seasonalSD, by = c("File")) %>%
    
    #adding other summary stats - mean, low 10th and upper 10th percentiles
    group_by(File) %>%
    mutate(across(c(obs_T, obs_DO, obs_pH), 
                  list(~ mean(.x, na.rm = TRUE), 
                       low10 = ~ quantile(.x, 0.1, na.rm = TRUE),
                       high10 = ~quantile(.x,0.9, na.rm = TRUE)))) %>%
    
    # to get to scale of one row per mpa
    distinct(File, .keep_all = TRUE) %>% ungroup() %>%
    select(-julianday, -time_utc, -Year,-Month, -Day) 
  
  return(sumstat)
  }



obs_sumstat <- obs_calculate_sumstat(obs_means, obs_interp_all, obs_seasonal_SD)

















