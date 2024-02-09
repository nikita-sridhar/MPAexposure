library(tidyverse)
library(lubridate)

setwd("~/Documents/Packard_MPA_Project/Data")

env_mpa <- read.csv("clean_MPA_int.csv") #reads csv of clean env data intersected with MPAs
env <- read.csv("clean_all_distance_offshore_seacarb.csv")



##############################
# Exploring general env data #
##############################



head(env)

#filtering for datasets w temp, do, and pH flag of 1
best <- env %>%
  filter(t_flag == 1) %>%
  filter(pH_flag == 1) %>%
  filter(do_flag == 1)

#filtering datasets w EITHER temp,do,and pH of 1
bestpH <- env %>%
  filter(pH_flag == 1)
besttemp <- env %>%
  filter(t_flag == 1)
bestdo <- env %>%
  filter(do_flag == 1)

unique(bestpH$dataset_id) #3  5 17 21 23 25 26 28 30 31 32 33 35 36 37 38 39 43 44
bestph17 <- bestpH %>%
  filter(dataset_id == 17)

#filter by depth
bestph25_30 <- bestph25 %>% 
  filter(depth_m ==30)

ggplot(bestph17) + geom_point(aes(time_utc, pH_total), color = "red", size = 0.05 )

#filtering all env data for specific coords
channelisl <- env %>%
  filter(latitude >= 33.36320) %>%
  filter(latitude <= 34.20492) %>%
  filter(longitude <= -118.90879) %>%
  filter(longitude >= -120.60483)

centralca <- env %>%
  filter(latitude >= 35.12842) %>%
  filter(latitude <= 37.795813) %>%
  filter(longitude <= -120.641765) %>%
  filter(longitude >= -124.063303)

norcal <- env %>%
  filter(latitude >= 37.77) %>%
  filter(latitude <= 41.99)

norcal45 <- norcal %>%
  filter(dataset_id == 45) 

unique(norcal9$depth_m) #36.62 (2012-2014) 36.45(2013-2014) 36.95 (2011-2013)

channelph <- channelisl %>%
  filter(pH_flag == 1)

ggplot(centralca39) + geom_point(aes(time_utc, pH_total), color = "red", size = 0.05 )


env25 <- env %>%
  filter(dataset_id == 25) %>%
  filter(depth_m == 0)

unique(env28$latitude) #38.333 38.160 38.080 33.620 32.420 38.210 36.820
  
ggplot(norcal45) + geom_point(aes(time_utc, t_C), size = 0.05) 
ggplot(norcal45) + geom_point(aes(time_utc, do_umolkg), color = "blue", size = 0.05) 
ggplot(norcal45) + geom_point(aes(time_utc, pH_total), color = "red" , size = 0.05)

#create a list of coordinates (x,y) in one cell to separate unique values
bestph5$x <- paste(bestph5$latitude, ",", bestph5$longitude)
unique_bestph5 <- unique(bestph5$x)

#determining which datasets are in best aka good temp do AND ph (5 17 21 23 25 26 28 30 31 32 37 38 39 43 44)
unique(best$dataset_id)

#filtering for each dataset within overall best (best ph, do and temp)
best5 <- best %>%
  filter(dataset_id == 5)

#plots for temperature, DO, and pH
ggplot(best17) + geom_point(aes(time_utc, t_C)) 
ggplot(best17) + geom_point(aes(time_utc, do_umolkg), color = "blue") 
ggplot(best17) + geom_point(aes(time_utc, pH_total), color = "red" )


##############################
###### MPA time series #######
##############################

#filtering for specific MPA
beggrock <- env_mpa %>%
  filter(NAME == "Begg Rock SMR") %>%
  filter(time_utc > "1987/09/12 06:16:00")

#filtering for range of depths
br0 <- beggrock %>% 
  filter(depth_m == 0)
br0_5 <- beggrock %>% 
  filter(depth_m <= 5)
br6_10 <- beggrock %>%
  filter(depth_m <= 10) %>%
  filter(depth_m >5)
br11_15 <- beggrock %>%
  filter(depth_m <= 15) %>%
  filter(depth_m >10)

#plots for temperature and DO
ggplot(beggrock) + geom_line(aes(time_utc, t_C)) 
ggplot(br0) + geom_point(aes(time_utc, do_umolkg), color = "blue")

#to select good long datasets, starting off with kristy's from cleanall. then go onto other good datasets u didn't include before
kristy1 <- clean_all %>%
  filter(dataset_id == 40) %>%
  filter(latitude == 33.4169) %>%
  filter(longitude == -118.3977)

kristy2 <- clean_all %>%
  filter(dataset_id == 40) %>%
  filter(latitude == 33.5421) %>%
  filter(longitude == -117.7947)

kristy3 <- clean_all %>%
  filter(dataset_id == 40) %>%
  filter(latitude == 35.2412) %>%
  filter(longitude == -120.8955)

kristy4 <- clean_all %>%
  filter(dataset_id == 40) %>%
  filter(latitude == 36.0685) %>%
  filter(longitude == -121.6016)

kristy5 <- clean_all %>%
  filter(dataset_id == 40) %>%
  filter(latitude == 38.946) %>%
  filter(longitude == -123.7389)

kristy6 <- clean_all %>%
  filter(dataset_id == 40) %>%
  filter(latitude == 39.2711) %>%
  filter(longitude == -123.7947)

kristy17 <- clean_all %>%
  filter(latitude == 38.5460) %>%
  filter(longitude == -123.2998)

data45 <- clean_all %>%
  filter(dataset_id == 45)

mod_all <- mod_all %>%
  mutate(T_mid = (T_surf + T_bot)/2) %>%
  mutate(pH_mid = (pH_surf + pH_bot)/2) %>%
  mutate(DO_mid = (DO_surf + DO_bot)/2) 
  
modkristy1 <- mod_all30m %>%
  filter(dataset == "kristy1")

#kristy3 model filtered for upwelling years
modkristy3_up <- modkristy3 %>%
  filter(Year == 2010)

kristy3_up <- kristy3 %>%
  filter(year == 2018 | year == 2021) 



#frequency histograms
#dataset 44

data45 <- data45%>%
  mutate(date = lubridate::date(time_utc), month = lubridate::month(time_utc), year = lubridate::year(time_utc)) %>%
  group_by(dataset_id, date, .add = TRUE) %>%
  summarize(temp_daily_avg = mean(t_C, na.rm = TRUE), 
            pH_daily_avg = mean(pH_total, na.rm = TRUE), 
            do_daily_avg = mean(do_umolkg, na.rm = TRUE)) 

length(unique(kristy2$time_utc))
length(unique(kristy3$time_utc)) == nrow(kristy3)

combinedkristy3_up <- rbind(kristy3_up, modkristy3_up)

ggplot(data45) + geom_point(aes(time_utc, t_C), size = 0.3) + labs(subtitle = "Temp")
ggplot(data45) + geom_point(aes(time_utc, do_umolkg), color = "blue", size = 0.3) + labs(subtitle = "DO")
ggplot(data30) + geom_point(aes(time_utc, pH_total), color = "red" , size = 0.3)+ labs(subtitle = "pH") 

#####################
#making sure data is continuous

kristy6 <- kristy6 %>%
  mutate(month = lubridate::month(time_utc), year = lubridate::year(time_utc)) 

grr <- data56 %>%
  filter(month == 1) 
 
length(gr$pH_daily_avg)

#edited out certain months w low data
data31ed <- data31 %>%
  filter(month != 4) %>%
  filter(month != 5) 

mod31ed <- mod31 %>%
  filter(Month != 4) %>%
  filter(Month != 5) 

mod56mhw <- mod56 %>%
  filter(Year == 1997)
data56mhw <- data56 %>%
  filter(year >= 2014) %>%
  filter(year <= 2016)

combined_30m_45 <- rbind(data45, mod45_30m)


#histogram
ggplot(combined56) + 
  geom_histogram(data = mod56, binwidth = 0.002, fill = "green", alpha = 0.5, aes(pH_mid)) +
  geom_histogram(data = data56, binwidth = 0.002, fill = "red", alpha = 0.4, aes(pH_daily_avg)) 

#pdf
ggplot(combined_30m_45) + 
  geom_density(data = mod45_30m, fill = "green", alpha = 0.5, aes(T_surf)) +
  geom_density(data = data45, fill = "red", alpha = 0.4, aes(temp_daily_avg)) 

#cdf
ggplot(combined_30m_45) + 
  stat_ecdf(data = mod45_30m, color = "green", alpha = 0.5, aes(DO_surf)) +
  stat_ecdf(data = data45, color = "red", alpha = 0.4, aes(do_daily_avg))


hm <- clean_all %>% filter(dataset_id == 56)
unique(hm$depth_m)
