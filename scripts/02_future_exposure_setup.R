#File created 1/14/24
#Authored by Nikita Sridhar
#Setup for future projection analysis including cleanup and generation of summary stats
#NOTE: This script is for one projection (IPSL) - manually change every occurrence of IPSl
#to Hadley or GFDL to do same for diff projections.

# Read files -------------------------------------------------------------------
#Read mpa data, convert DO units, add a column for time period, and one for 
#difference between mid/end cen and historic.
mpa <- read_csv("data/processeddata/model/IPSLmpa.csv") %>% 
  mutate(DO_mmolL = DO_surf/1000,
         DO_mgL = convert_DO(DO_mmolL, from = "mmol/L", to = "mg/L")) %>% 
  filter(Year <= 2020|
           Year >= 2040 & Year <= 2060|
           Year >= 2080 & Year <= 2100) %>%
  mutate(period = case_when((Year <= 2020) ~ "historic", 
                            (Year %in% c(2040:2060)) ~ "midcen",
                            (Year %in% c(2080:2100)) ~ "endcen")) %>%
  select(-T_bot, -DO_bot, -pH_bot,-DO_surf,-DO_mmolL) 
mpa$File <- substr(mpa$File, 1, nchar(mpa$File)-4)



# label regions -----------------------------------------------------------

mpa_centroids<- read.csv("~/Documents/Packard_MPA_Project/Data/MPA_data/MPA_polygons.csv")
mpa_centroids$File <- sub("^", "tphdo_mpa_", mpa_centroids$OBJECTID )

mpa_centroids <- mpa_centroids %>%
  mutate(region = ifelse(degy >= 37.29, "norca", 
                         ifelse(degy > 34.8, "centralca", 
                                ifelse("degy" < 34.274 & "degx" < -119.220, 
                                       "channelisl" , 
                                       "socal"))))

channel <- c("Anacapa Island FMCA", "Anacapa Island FMR", "Anacapa Island SMCA", 
             "Anacapa Island SMR", "Anacapa Island Special Closure", 
             "Arrow Point to Lion Head Point SMCA", "Begg Rock SMR", 
             "Blue Cavern Offshore SMCA", "Blue Cavern Onshore SMCA (No-Take)",
             "Carrington Point SMR", "Casino Point SMCA (No-Take)", "Cat Harbor SMCA", 
             "Farnsworth Offshore SMCA", "Farnsworth Onshore SMCA", 
             "Footprint FMR", "Footprint SMR", "Gull Island FMR", "Gull Island SMR", 
             "Harris Point FMR", "Harris Point SMR", "Judith Rock SMR", 
             "Long Point SMR", "Lover’s Cove SMCA", "Painted Cave SMCA", 
             "Richardson Rock FMR", "Richardson Rock SMR", "San Miguel Island Special Closure", 
             "Santa Barbara Island FMR", "Santa Barbara Island SMR", "Scorpion FMR", 
             "Scorpion SMR", "Skunk Point SMR", "South Point FMR", "South Point SMR")

mpa_centroids$region[mpa_centroids$NAME %in% channel]  <- "channel" 
mpa <- merge(mpa, mpa_centroids,  by = "File")

mpa_historic <- mpa %>%
  filter(period == "historic")
mpa_midcen <- mpa %>%
  filter(period == "midcen")
mpa_endcen <- mpa %>%
  filter(period == "endcen")


# generate summary stats --------------------------------------------------

sum_historic <- mpa_historic %>% 
  group_by(File) %>% 
  summarize(across(c(T_surf, DO_mgL, pH_surf), 
                   list(mean = mean, quantile = ~ quantile(.x, 0.1)))) %>% 
  rename(T_mean = T_surf_mean, 
         DO_mean = DO_mgL_mean, 
         pH_mean = pH_surf_mean, 
         T_low10 = T_surf_quantile, 
         DO_low10 = DO_mgL_quantile, 
         pH_low10 = pH_surf_quantile) 
sum_historic <- merge(sum_historic, mpa_centroids,  by = "File")


sum_midcen <- mpa_midcen %>% 
  group_by(File) %>% 
  summarize(across(c(T_surf, DO_mgL, pH_surf), 
                   list(mean = mean, quantile = ~ quantile(.x, 0.1)))) %>% 
  rename(T_mean = T_surf_mean, 
         DO_mean = DO_mgL_mean, 
         pH_mean = pH_surf_mean, 
         T_low10 = T_surf_quantile, 
         DO_low10 = DO_mgL_quantile, 
         pH_low10 = pH_surf_quantile)
sum_midcen <- merge(sum_midcen, mpa_centroids,  by = "File")


sum_endcen <- mpa_endcen %>% 
  group_by(File) %>% 
  summarize(across(c(T_surf, DO_mgL, pH_surf), 
                   list(mean = mean, quantile = ~ quantile(.x, 0.1)))) %>% 
  rename(T_mean = T_surf_mean, 
         DO_mean = DO_mgL_mean, 
         pH_mean = pH_surf_mean, 
         T_low10 = T_surf_quantile, 
         DO_low10 = DO_mgL_quantile, 
         pH_low10 = pH_surf_quantile)
sum_endcen <- merge(sum_endcen, mpa_centroids,  by = "File")


# calculate seasonal variation --------------------------------------------

mpa_historic_climatology <- mpa_historic %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T_surf), T_clim_sd = sd(T_surf), pH_clim = mean(pH_surf), 
            DO_clim =   mean(DO_mgL))
mpa_midcen_climatology <- mpa_midcen %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T_surf), pH_clim = mean(pH_surf), DO_clim = mean(DO_mgL))
mpa_endcen_climatology <- mpa_endcen %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T_surf), pH_clim = mean(pH_surf), DO_clim = mean(DO_mgL))

historic_seasonalSD <- mpa_historic_climatology %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))
midcen_seasonalSD <- mpa_midcen_climatology %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))
endcen_seasonalSD <- mpa_endcen_climatology %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))


# calculate event-based variation -----------------------------------------

#setup for interpolation

#First: Need to create a day1 and day365 proxies. Approx function can only
#interpolate not extrapolate so without this, you can interpolate days 1-14 and 
#350-365. #below creates empty vectors as big as we need (365 days per mpa) for 
#each variable and each mpa. julianday is dates 1-365 as many mpa times

mpaslist = unique(mpa$File) 
mpas = rep(NA, 365*121)
julianday = rep(1:365, 121)
T_clim = rep(NA, 365*121)
pH_clim = rep(NA, 365*121)
DO_clim = rep(NA, 365*121)

#Set up a vector of julian day assignment for the 15th of each month and the 
#first and last day of the year
x_in <- yday(as.Date(c("2000-01-01", "2000-01-15","2000-02-15","2000-03-15",
                       "2000-04-15","2000-05-15","2000-06-15",
                       "2000-07-15","2000-08-15","2000-09-15","2000-10-15",
                       "2000-11-15","2000-12-15", "2000-12-31")))

#create list of all the days of the year not included in x_in to interpolate to.
x_out <- (1:365)
x_out <- x_out[!(x_out %in% x_in)] #removing days of the year we already have values for

#interpolation for temp
#HISTORIC
for (i in 1:length(mpaslist)){
  print(i)
  d = mpa_historic_climatology %>% filter(File == mpaslist[i]) %>% select(T_clim)
  #use a weighted average to get these
  Dec31 = as.numeric(( ((16/30) * (d[12,2])) + ((14/30) * d[1,2]) ))
  Jan1 = as.numeric(( ((14/30) * (d[12,2])) + ((16/30) * d[1,2]) ))
  # a list of y-values of the climatological temp on each of the days in x_in
  y_in <- c(Jan1, d$T_clim, Dec31)
  
  mod = approx(x = x_in, y = y_in, xout = x_out) 
  mpas[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365)
  T_clim[((i-1)*365+1):(i*365)] <- mod$y
}

historic_tempdata = data.frame(mpas, julianday,T_clim)
historic_tempdata <- historic_tempdata %>%
  rename(File = mpas)

#Later we create a julian day variable in our OG mpa dataset, to merge this to all the rows on the dataset by filename and day and then have the mean daily climatological value for each row. 
#MIDCEN
for (i in 1:length(mpaslist)){
  print(i)
  d = mpa_midcen_climatology %>% filter(File == mpaslist[i]) %>% select(T_clim)
  #use a weighted average to get these
  Dec31 = as.numeric(( ((16/30) * (d[12,2])) + ((14/30) * d[1,2]) ))
  Jan1 = as.numeric(( ((14/30) * (d[12,2])) + ((16/30) * d[1,2]) ))
  # a list of y-values of the climatological temp on each of the days in x_in
  y_in <- c(Jan1, d$T_clim, Dec31)
  
  mod = approx(x = x_in, y = y_in, xout = x_out) 
  mpas[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365)
  T_clim[((i-1)*365+1):(i*365)] <- mod$y
}

midcen_tempdata = data.frame(mpas, julianday,T_clim)
midcen_tempdata <- midcen_tempdata %>%
  rename(File = mpas)

#ENDCEN
for (i in 1:length(mpaslist)){
  print(i)
  d = mpa_endcen_climatology %>% filter(File == mpaslist[i]) %>% select(T_clim)
  #use a weighted average to get these
  Dec31 = as.numeric(( ((16/30) * (d[12,2])) + ((14/30) * d[1,2]) ))
  Jan1 = as.numeric(( ((14/30) * (d[12,2])) + ((16/30) * d[1,2]) ))
  # a list of y-values of the climatological temp on each of the days in x_in
  y_in <- c(Jan1, d$T_clim, Dec31)
  
  mod = approx(x = x_in, y = y_in, xout = x_out) 
  mpas[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365)
  T_clim[((i-1)*365+1):(i*365)] <- mod$y
}

endcen_tempdata = data.frame(mpas, julianday,T_clim)
endcen_tempdata <- endcen_tempdata %>%
  rename(File = mpas)



