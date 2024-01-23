#File created 11/6/2023
#Authored by Nikita Sridhar
#Comparison of model and observational T/pH/DO data using PDFs and CDFs to determine
#model fit

library(tidyverse)


#Load model and obs data 
mod <- read_csv("data/processeddata/model/mod_all30m.csv")
obs <- load("data/rawdata/obs/for model-obs comparison/clean_all_light.RData")
obs <- light
rm(light)

#model setup
mod <- mod %>%
  rename(dataset_id = dataset,
         mod_T = T_surf,
         mod_pH = pH_surf,
         mod_DO = DO_surf) %>%
  mutate(time_utc = lubridate::ymd(paste(Year, Month, Day, sep="-"))) %>%
  select(dataset_id, time_utc, mod_T, mod_pH, mod_DO) %>%
  #adding regions
  mutate("region" = case_when(
    dataset_id %in% c("7","14","kristy1") ~ "Channel Islands",
    dataset_id %in% c("19","kristy4") ~ "Central CA",
    dataset_id %in% c("30", "31","44","kristy2","kristy3") ~ "Southern CA",
    dataset_id %in% c("45","56","kristy5","kristy6") ~ "Northern CA"
  ))

#obs setup
obs <- obs %>%
  #filtering all datasets based on prev exploration of longest best quality across regions
  filter(dataset_id %in% c(7,14,19,30,31,44,45,40,56)) %>%
  #dataset 40 is kristy, but contains multiple coords, so separating those here
  within(dataset_id[dataset_id == 40 & latitude == 33.4169 & longitude == -118.3977] <- 'kristy1') %>%
  within(dataset_id[dataset_id == 40 & latitude == 33.5421 & longitude == -117.7947] <- 'kristy2') %>%
  within(dataset_id[dataset_id == 40 & latitude == 35.2412 & longitude == -120.8955] <- 'kristy3') %>%
  within(dataset_id[dataset_id == 40 & latitude == 36.0685 & longitude == -121.6016] <- 'kristy4') %>%
  within(dataset_id[dataset_id == 40 & latitude == 38.946 & longitude == -123.7389] <- 'kristy5') %>%
  within(dataset_id[dataset_id == 40 & latitude == 39.2711 & longitude == -123.7947] <- 'kristy6') %>%
  #calculating daily avgs for temp, ph, do
  group_by(dataset_id, time_utc) %>%
  summarize(obs_T = mean(t_C, na.rm = TRUE), 
            obs_pH = mean(pH_total, na.rm = TRUE), 
            obs_DO = mean(do_umolkg, na.rm = TRUE),
            .groups = "drop") %>%
  #adding regions
  mutate("region" = case_when(
    dataset_id %in% c("7","14","kristy1") ~ "Channel Islands",
    dataset_id %in% c("19","kristy4") ~ "Central CA",
    dataset_id %in% c("30", "31","44","kristy2","kristy3") ~ "Southern CA",
    dataset_id %in% c("45","56","kristy5","kristy6") ~ "Northern CA"
  ))
#turning NaN into NA
obs$obs_T[is.nan(obs$obs_T)]<-NA
obs$obs_pH[is.nan(obs$obs_pH)]<-NA
obs$obs_DO[is.nan(obs$obs_DO)]<-NA


#pdf - NorCA
ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Northern CA T fit") +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Northern CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Northern CA"), fill = "green", alpha = 0.5, aes(mod_DO)) +
  geom_density(data = obs%>%filter(region == "Northern CA"), fill = "red", alpha = 0.4, aes(obs_DO)) +
  ggtitle("Northern CA DO fit")+
  theme_classic()+
  labs(x = "DO", y = "Probability density")

#pdf - CenCA
ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Central CA T fit")  +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Central CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Central CA"), fill = "green", alpha = 0.5, aes(mod_DO)) +
  geom_density(data = obs%>%filter(region == "Central CA"), fill = "red", alpha = 0.4, aes(obs_DO)) +
  ggtitle("Central CA DO fit")+
  theme_classic()+
  labs(x = "DO", y = "Probability density")



#pdf - SoCA
ggplot() + 
  geom_density(data = mod%>%filter(region == "Southern CA"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs%>%filter(region == "Southern CA"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Southern CA T fit") +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Southern CA"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs%>%filter(region == "Southern CA"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Southern CA pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Southern CA"), fill = "green", alpha = 0.5, aes(mod_DO)) +
  geom_density(data = obs%>%filter(region == "Southern CA"), fill = "red", alpha = 0.4, aes(obs_DO)) +
  ggtitle("Southern CA DO fit") +
  theme_classic()+
  labs(x = "DO", y = "Probability density")


#pdf - channel isl
ggplot() + 
  geom_density(data = mod%>%filter(region == "Channel Islands"), fill = "green", alpha = 0.5, aes(mod_T)) +
  geom_density(data = obs%>%filter(region == "Channel Islands"), fill = "red", alpha = 0.4, aes(obs_T)) +
  ggtitle("Channel Islands T fit") +
  theme_classic()+
  labs(x = "Temperature (C)", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Channel Islands"), fill = "green", alpha = 0.5, aes(mod_pH)) +
  geom_density(data = obs%>%filter(region == "Channel Islands"), fill = "red", alpha = 0.4, aes(obs_pH)) +
  ggtitle("Channel Islands pH fit")+
  theme_classic()+
  labs(x = "pH", y = "Probability density")
ggplot() + 
  geom_density(data = mod%>%filter(region == "Channel Islands"), fill = "green", alpha = 0.5, aes(mod_DO)) +
  geom_density(data = obs%>%filter(region == "Channel Islands"), fill = "red", alpha = 0.4, aes(obs_DO)) +
  ggtitle("Channel Islands DO fit") +
  theme_classic()+
  labs(x = "DO", y = "Probability density")


#cdf
ggplot() + 
  stat_ecdf(data = mod%>%filter(region == "Northern CA"), color = "green", alpha = 0.5, aes(mod_T)) +
  stat_ecdf(data = obs%>%filter(region == "Northern CA"), color = "red", alpha = 0.4, aes(obs_T))

  
rm(mod)
rm(obs)
