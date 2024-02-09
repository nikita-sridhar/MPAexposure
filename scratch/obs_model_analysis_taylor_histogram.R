library(tidyverse)
library(lubridate)
library(raster)
library(tidyr)

setwd("~/Documents/Packard_MPA_Project/Data")
env <- load("clean_all_distance_offshore_seacarb.csv")
mod_all <- load("mod_all.RData")
head(mod_all)

env40 <- filter(env, dataset_id == 40)
write.csv(env40, file = "env40.csv")
unique(env40$latitude)

#filtering all datasets for specific chosen ones and time
final_datasets <- c(3,30, 31, 43, 44, 17, 37, 39, 28, 6, 7, 14, 33, 18, 19, 38, 45)
env <- env %>%
  filter(dataset_id %in% final_datasets)

#group by and summarize by date (daily avgs)
daily_data <- env %>%
  mutate(date = lubridate::date(time_utc), month = lubridate::month(time_utc), year = lubridate::year(time_utc)) %>%
  group_by(dataset_id, date, .add = TRUE) %>%
  summarize(temp_daily_avg = mean(t_C, na.rm = TRUE), 
            pH_daily_avg = mean(pH_total, na.rm = TRUE), 
            do_daily_avg = mean(do_umolkg, na.rm = TRUE))

#monthly stats using daily avgs
monthly_data <- daily_data %>%
  mutate(month = lubridate::month(date), year = lubridate::year(date)) %>%
  group_by(dataset_id, year, month, .add = TRUE) %>%
  summarize(temp_monthly_avg = mean(temp_daily_avg, na.rm = TRUE),
            temp_monthly_sd = sd(temp_daily_avg, na.rm = TRUE),
            temp_monthly_var = var(temp_daily_avg, na.rm = TRUE),
            temp_monthly_max = max(temp_daily_avg, na.rm = TRUE),
            temp_monthly_min = min(temp_daily_avg, na.rm = TRUE),
            temp_monthly_10 = quantile(temp_daily_avg, 0.1, na.rm = TRUE),
            temp_monthly_90 = quantile(temp_daily_avg, 0.9, na.rm = TRUE),
            pH_monthly_avg = mean(pH_daily_avg, na.rm = TRUE),
            pH_monthly_sd = sd(pH_daily_avg, na.rm = TRUE),
            pH_monthly_var = var(pH_daily_avg, na.rm = TRUE),
            pH_monthly_max = max(pH_daily_avg, na.rm = TRUE),
            pH_monthly_min = min(pH_daily_avg, na.rm = TRUE),
            pH_monthly_10 = quantile(pH_daily_avg, 0.1, na.rm = TRUE),
            pH_monthly_90 = quantile(pH_daily_avg, 0.9, na.rm = TRUE),
            do_monthly_avg = mean(do_daily_avg, na.rm = TRUE),
            do_monthly_sd = sd(do_daily_avg, na.rm = TRUE),
            do_monthly_var = var(do_daily_avg, na.rm = TRUE),
            do_monthly_max = max(do_daily_avg, na.rm = TRUE),
            do_monthly_min = min(do_daily_avg, na.rm = TRUE),
            do_monthly_10 = quantile(do_daily_avg, 0.1, na.rm = TRUE),
            do_monthly_90 = quantile(do_daily_avg, 0.9, na.rm = TRUE))

#ok, now to average the diff stats u made (monthly stats + unsat monthly stats) over ALL years for each month.
#in hindsight, maybe this is uneccesary bc boxplot alr finds this data from the monthly dataset.
across_yrs <- monthly_data %>% 
  group_by(dataset_id, month) %>%
  summarize(temp_avg = mean(temp_monthly_avg, na.rm = TRUE),
            temp_max = max(temp_monthly_avg, na.rm = TRUE),
            temp_min = min(temp_monthly_avg, na.rm = TRUE),
            temp_10 = quantile(temp_monthly_avg, 0.1, na.rm = TRUE),
            temp_90 = quantile(temp_monthly_avg, 0.9, na.rm = TRUE),
            pH_avg = mean(pH_monthly_avg, na.rm = TRUE),
            pH_max = max(pH_monthly_avg, na.rm = TRUE),
            pH_min = min(pH_monthly_avg, na.rm = TRUE),
            pH_10 = quantile(pH_monthly_avg, 0.1, na.rm = TRUE),
            pH_90 = quantile(pH_monthly_avg, 0.9, na.rm = TRUE),
            do_avg = mean(do_monthly_avg, na.rm = TRUE),
            do_max = max(do_monthly_avg, na.rm = TRUE),
            do_min = min(do_monthly_avg, na.rm = TRUE),
            do_10 = quantile(do_monthly_avg, 0.1, na.rm = TRUE),
            do_90 = quantile(do_monthly_avg, 0.9, na.rm = TRUE)
            ) 
#turn strings to NA
across_yrs<- na_if(across_yrs, '-Inf') 
across_yrs<- na_if(across_yrs, 'Inf')
across_yrs$month = factor(across_yrs$month, ordered = TRUE)

unique(monthly_data$dataset_id) #3  6  7 14 17 18 19 28 30 31 33 37 38 39 43 44 45
unique(unsat$dataset_id) #17 28 30 31 37 38 

#trying to work w initial monthly dataset for boxplots. need to compare to see if means across years calculated right
#looks like it worked!

#changing non numeric to NA and making months factors for boxplot to work
monthly_data<- na_if(monthly_data, '-Inf') 
monthly_data<- na_if(monthly_data, 'Inf')
monthly_data$month = factor(monthly_data$month, ordered = TRUE)
unsat$month = factor(unsat$month, ordered = TRUE)

#unsaturated days analysis
#group by months,then within summarize function, count no. days per month with non NA data and no. unsaturated days per month to get a proportion of unsat days per month 
test <- daily_data %>%
  mutate(date = lubridate::date(date), month = lubridate::month(date), year = lubridate::year(date)) %>%
  group_by(dataset_id, year, month, .add = TRUE) %>%
  summarize(unsat_days = sum(pH_daily_avg < 7.75, na.rm = TRUE), 
            numdays = sum(pH_daily_avg > 0, na.rm = TRUE)) %>%
  mutate(prcnt_unsat = (unsat_days/numdays)*100)
#filter test dataset under 100% unsat, then find stats within unsat days
unsat <- test %>%
  filter(prcnt_unsat > 0) 
######################
#sara's code:
#This gets the date data into a preferred format
daily_data = daily_data %>% 
  mutate(month = lubridate::month(date), year = lubridate::year(date))
#creates a dataframe of unique combos for dataset/year/month for those that has saturated data (aka <100 perc Unsat)
unique_combos = unique(unsat[c("dataset_id","year","month")])
#This is just a quick test to make sure the for loop is working the away you want
i = 1
thing = daily_data %>% #why is thing only one row???
  filter(dataset_id ==unique_combos[i,1]) %>%
  filter(year == as.numeric(unique_combos[i,2])) %>%
  filter(month == as.numeric(unique_combos[i,3])) %>%
  filter(pH_daily_avg<7.75)
#set up whatever vectors you want here to contain computed statistics for each dataeset/year/month combo
avg_ph = vector()
max_ph = vector()
min_ph = vector()
for (i in 1:nrow(unique_combos)){ #so looping through those dataset/year/month combos
  thing = daily_data %>% #this gets a dataframe with only those data in your dataset/year/month combo and that has unsaturated data
    filter(dataset_id ==unique_combos[i,1]) %>%
    filter(year == as.numeric(unique_combos[i,2])) %>%
    filter(month == as.numeric(unique_combos[i,3])) %>%
    filter(pH_daily_avg < 7.75)
  #here you can computer whatever stats on that unsaturated data you are storing in 'thing'
  avg_ph[i] = mean(thing$pH_daily_avg)
  max_ph[i]= max(thing$pH_daily_avg)
  min_ph[i]=min(thing$pH_daily_avg)
}
#use a tidyverse function to append new statistics to your dataset (i used cbind which doesn't play well with tidyverse)
#make sure the lengths are the same for each new row and the below_100prcnt_unsat
#double check it's querying/statisticing in the right order to just append on to this dataframe
unsat <- cbind(unsat, avg_ph) 
unsat <- rename(unsat, avg_unsat_ph = ...7)

unsat <- cbind(unsat,max_ph)
unsat <- rename(unsat,max_unsat_ph = ...8)

unsat <- cbind(unsat, min_ph)
unsat <- rename(unsat,min_unsat_ph = ...9)


###########
#PLOTS
##########

#filter for dataset - do this for each dataset and rerun plots and change dataset to the filtered one
monthly_6 <- monthly_data %>%
  filter(dataset_id == 6)

#plots for averaged temp/ph/do across years
ggplot(monthly_6, aes(x=month, y=temp_monthly_avg, na.rm = TRUE)) + 
  geom_boxplot(fill = " orange") + theme_classic() + labs(x = "Month", y = "Mean temp (C)" , subtitle = "2013 - 2021")
ggplot(monthly_6, aes(x=month, y=pH_monthly_avg, na.rm = TRUE)) + 
  geom_boxplot(fill = "sky blue") + theme_classic() + labs(x = "Month", y = "Mean pH", subtitle = "2013 - 2021")
ggplot(monthly_6, aes(x=month, y=do_monthly_avg, na.rm = TRUE)) + 
  geom_boxplot(fill =  "light green") + theme_classic() + labs(x = "Month", y = "Mean DO (umolkg)", subtitle = "2013 - 2021")

#if the monthly dataset has unsat days, then filter for it from unsat dataset
unsat_17 <- unsat %>%
  filter(dataset_id==17)
#find avg prcnt unsat to plot as simple bar chart
unsat_17_across <- unsat_17 %>% 
  group_by(dataset_id, month) %>%
  summarize(avg_prcnt_unsat = mean(prcnt_unsat, na.rm = TRUE))
#plotting simple bar chart of prcnt unsat by month
ggplot(unsat_17_across, aes(x=month, y=avg_prcnt_unsat, na.rm = TRUE)) + 
   geom_col() + theme_classic()
ggplot(unsat_17, aes(x=month, y=avg_unsat_ph, na.rm = TRUE)) + 
  geom_boxplot(fill = "sky blue") + theme_classic() + labs(x = "Month", y = "Mean pH of Unsat Days")

#####################
#MODEL obs comparison
#####################




#combining indiv model datasets into one
kristy6 <- read_csv("tphdo_kristy6.csv") %>% mutate(dataset = "kristy6")



mod_all <- rbind(mod14, mod17, mod18, mod19, mod28, mod3, mod30, mod31, mod33, mod37, mod38, mod39, mod43, mod44, mod45, mod6, mod7)
mod_all <- rbind(mod_all, modkristy5_100m, modkristy6_100m, mod56)
save(mod_all, file = "mod_all2.RData")

#new: adding kristy's stuff to mod_all
mod_all <- rbind(mod_all, kristy1, kristy2, kristy3, kristy4, kristy5, kristy6)

#monthly avg from model - surface values used from model
mod56 <- read.csv('./tphdo_56.csv')

#30m avg depth from model output
modkristy6_30m <- read.csv('./Tmp/tphdo_sta_kristy6.csv')
mod_all30m <- rbind(mod7_30m, mod14_30m, mod19_30m, mod30_30m, mod31_30m,mod44_30m,mod45_30m,mod56_30m,modkristy1_30m, modkristy2_30m, modkristy3_30m, modkristy4_30m, modkristy5_30m, modkristy6_30m)
modkristy6_30m <- modkristy6_30m %>%
  mutate(dataset = "kristy6")
save(mod_all30m, file = "mod_all30m.RData")


mod45_monthly <- mod45 %>%
  group_by(Year,Month, .add = TRUE) %>%
  summarize(mod_temp = mean(T_surf, na.rm = TRUE),
            mod_ph = mean(pH_surf, na.rm=TRUE),
            mod_do = mean(DO_surf, na.rm=TRUE)) %>%
  mutate(data = "model")
mod45_monthly$Month = factor(mod45_monthly$Month, ordered = TRUE)

mod <- mod56 %>%
  mutate(dataset = 56)

#obs filter
monthly_45 <- monthly_data %>%
  filter(dataset_id == 45)

#plot of model and obs
ggplot() +
  geom_boxplot(data = mod45_monthly, aes(x=Month, y=mod_temp, na.rm = TRUE),  color = "pink", fill = "pink", alpha = 0.4 ) +
  geom_boxplot(data = monthly_45, aes(x=month, y=temp_monthly_avg, na.rm = TRUE), color = "maroon", fill = "maroon", alpha = 0.4) + 
  theme_classic() + labs(x = "Month", y = "Mean temp (C)") + 
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"))
  
ggplot() +
  geom_boxplot(data = mod45_monthly, aes(x=Month, y=mod_ph, na.rm = TRUE),  color = "sky blue", fill = "sky blue", alpha = 0.4 ) +
  geom_boxplot(data = monthly_45, aes(x=month, y=pH_monthly_avg, na.rm = TRUE), color = "blue", fill = "blue", alpha = 0.4) + 
  theme_classic() + labs(x = "Month", y = "Mean pH") + 
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"))  

ggplot() +
  geom_boxplot(data = mod45_monthly, aes(x=Month, y=mod_do, na.rm = TRUE),  color = "light green", fill = "light green", alpha = 0.4 ) +
  geom_boxplot(data = monthly_45, aes(x=month, y=do_monthly_avg, na.rm = TRUE), color = "dark green", fill = "dark green", alpha = 0.4) + 
  theme_classic() + labs(x = "Month", y = "Mean DO (umolkg)") + 
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"))

##########################################
#trying to do unsat analysis w model output

mod6test <- mod6 %>%
  group_by(Year, Month, .add = TRUE) %>%
  summarize(unsat_moddays = sum(pH_surf < 7.75, na.rm = TRUE), 
            nummoddays = sum(pH_surf > 0, na.rm = TRUE)) %>%
  mutate(prcnt_modunsat = (unsat_moddays/nummoddays)*100)

#filter test dataset under 100% unsat, then find stats within unsat days
unsatmod6 <- mod6test %>%
  filter(prcnt_modunsat > 0) 

unique_modcombos = unique(unsatmod6[c("Year","Month")])

i = 1
modthing = mod6 %>% #why is thing only one row???
  filter(Year == as.numeric(unique_modcombos[i,1])) %>%
  filter(Month == as.numeric(unique_modcombos[i,2])) %>%
  filter(pH_surf<7.75)

#set up whatever vectors you want here to contain computed statistics for each dataeset/year/month combo
avg_modph = vector()
max_modph = vector()
min_modph = vector()

#
for (i in 1:nrow(unique_modcombos)){ #so looping through those dataset/year/month combos
  modthing = mod6 %>% #this gets a dataframe with only those data in your dataset/year/month combo and that has unsaturated data
    filter(Year == as.numeric(unique_modcombos[i,1])) %>%
    filter(Month == as.numeric(unique_modcombos[i,2])) %>%
    filter(pH_surf < 7.75)
  #here you can computer whatever stats on that unsaturated data you are storing in 'thing'
  avg_ph[i] = mean(thing$pH_daily_avg)
  max_ph[i]= max(thing$pH_daily_avg)
  min_ph[i]=min(thing$pH_daily_avg)
}
#use a tidyverse function to append new statistics to your dataset (i used cbind which doesn't play well with tidyverse)
#make sure the lengths are the same for each new row and the below_100prcnt_unsat
#double check it's querying/statisticing in the right order to just append on to this dataframe
unsatmod6 <- cbind(unsatmod6, avg_modph) 
unsatmod6 <- rename(unsatmod6, avg_unsat_ph = ...7)

unsat <- cbind(unsat,max_ph)
unsat <- rename(unsat,max_unsat_ph = ...8)

unsat <- cbind(unsat, min_ph)
unsat <- rename(unsat,min_unsat_ph = ...9)


#################
#redoing model obs comparison w averaged daily data per month over yrs instead of averaged monthly data
#################

#filter for dataset - do this for each dataset and rerun plots and change dataset to the filtered one
daily_data$month = factor(daily_data$month, ordered = TRUE)
daily_6 <- daily_data %>%
  filter(dataset_id == 6)

#plots for averaged temp/ph/do across years
ggplot(daily_6, aes(x=month, y=temp_daily_avg, na.rm = TRUE)) + 
  geom_boxplot(fill = " orange") + theme_classic() + labs(x = "Month", y = "Mean temp (C)" , subtitle = "2013 - 2021")



ggplot(monthly_6, aes(x=month, y=pH_monthly_avg, na.rm = TRUE)) + 
  geom_boxplot(fill = "sky blue") + theme_classic() + labs(x = "Month", y = "Mean pH", subtitle = "2013 - 2021")
ggplot(monthly_6, aes(x=month, y=do_monthly_avg, na.rm = TRUE)) + 
  geom_boxplot(fill =  "light green") + theme_classic() + labs(x = "Month", y = "Mean DO (umolkg)", subtitle = "2013 - 2021")


#below is copied from clean version where i was working through taylor diag - now i am using monthly for taylor, so cutting out some of this code
############
#taylor diag
############

#test out w dataset 45 first

#cleanup datasets, choose only columns u want, rename columns so everything but dataset and date is diff, then merge to form one dataset
#daily values
obs_clean <- daily_data %>%
  dplyr::rename(obs_temp = temp_daily_avg) %>%
  dplyr::rename(obs_pH = pH_daily_avg) %>%
  dplyr::rename(obs_do = do_daily_avg)

mod_clean <- mod_all %>%
  dplyr::rename(dataset_id = dataset) %>%
  dplyr::rename(mod_temp_surf = T_surf) %>%
  dplyr::rename(mod_temp_bot = T_bot) %>%
  dplyr::rename(mod_temp_mid = T_mid) %>%
  dplyr::rename(mod_pH_surf = pH_surf) %>%
  dplyr::rename(mod_pH_bot = pH_bot) %>%
  dplyr::rename(mod_pH_mid = pH_mid) %>%
  dplyr::rename(mod_do_surf = DO_surf) %>%
  dplyr::rename(mod_do_bot = DO_bot) %>%
  dplyr::rename(mod_do_mid = DO_mid) %>%
  mutate(date = lubridate::make_date(year = Year, month = Month, day = Day))

#join datasets to perform taylor
join <- full_join(mod_clean,obs_clean, by = c("dataset_id", "date")) 
#ok so acc we need monthly avgs for taylor, not daily, which is what's in this join.

#for loop to find model bias
unique_datasets <- unique(month_join$dataset_id)

surf_temp_bias <- vector()
mid_temp_bias <- vector()
bot_temp_bias <- vector()
surf_ph_bias <- vector()
mid_ph_bias <- vector()
bot_ph_bias <- vector()
surf_do_bias <- vector()
mid_do_bias <- vector()
bot_do_bias <- vector()

for (i in unique_datasets){ 
  surf_temp_bias[i] <- (mean(month_join$temp_surf, na.rm = TRUE) - mean(month_join$temp, na.rm = TRUE))
  mid_temp_bias[i] <- (mean(month_join$temp_mid, na.rm = TRUE) - mean(month_join$temp, na.rm = TRUE))
  bot_temp_bias[i] <- (mean(month_join$temp_bot, na.rm = TRUE) - mean(month_join$temp, na.rm = TRUE))
  
  surf_ph_bias[i] <- (mean(month_join$ph_surf, na.rm = TRUE) - mean(month_join$ph, na.rm = TRUE))
  mid_ph_bias[i] <- (mean(month_join$ph_mid, na.rm = TRUE) - mean(month_join$ph, na.rm = TRUE))
  bot_ph_bias[i] <- (mean(month_join$ph_bot, na.rm = TRUE) - mean(month_join$ph, na.rm = TRUE))
  
  surf_do_bias[i] <- (mean(month_join$do_surf, na.rm = TRUE) - mean(month_join$do, na.rm = TRUE))
  mid_do_bias[i] <- (mean(month_join$do_mid, na.rm = TRUE) - mean(month_join$do, na.rm = TRUE))
  bot_do_bias[i] <- (mean(month_join$do_bot, na.rm = TRUE) - mean(month_join$do, na.rm = TRUE))
  
}


(mean(join$mod_temp_surf, na.rm = TRUE) - mean(join$obs_daily_temp, na.rm = TRUE))  


mod_bias <- cbind(surf_temp_bias,mid_temp_bias,bot_temp_bias, surf_ph_bias, mid_ph_bias, bot_ph_bias, surf_do_bias, mid_do_bias, bot_do_bias)


#filter each dataset and make taylor
join3 <- filter(join, dataset_id == 3)

TaylorDiagram(join3, obs = "obs_daily_temp", mod = "mod_temp_surf", group = "dataset_id", normalise = TRUE, cex = 1)
TaylorDiagram(join28, obs = "obs_daily_temp", mod = "mod_temp_mid", group = "dataset_id", normalise = TRUE, cex = 1)
TaylorDiagram(join14, obs = "obs_daily_temp", mod = "mod_temp_bot", group = "dataset_id", normalise = TRUE, cex = 1)

TaylorDiagram(join3, obs = "obs_daily_pH", mod = "mod_pH_surf", group = "dataset_id", normalise = TRUE, cex = 1)
TaylorDiagram(join44, obs = "obs_daily_pH", mod = "mod_pH_mid", group = "dataset_id", normalise = TRUE, cex = 1)
TaylorDiagram(join14, obs = "obs_daily_pH", mod = "mod_pH_bot", group = "dataset_id", normalise = TRUE, cex = 1)

TaylorDiagram(join44, obs = "obs_daily_do", mod = "mod_do_surf", group = "dataset_id", normalise = TRUE, cex = 1)
TaylorDiagram(join28, obs = "obs_daily_do", mod = "mod_do_mid", group = "dataset_id", normalise = TRUE, cex = 1)
TaylorDiagram(join14, obs = "obs_daily_do", mod = "mod_do_bot", group = "dataset_id", normalise = TRUE, cex = 1)

mod14<- mod_clean %>%
  filter(dataset_id==14)


?TaylorDiagram


###########
#plots#####
###########

#plots for averaged temp/ph/do across years
#6 - temp only
ggplot() +
  geom_boxplot(data = mod6, aes(x=Month, y=T_surf, na.rm = TRUE), color = "pink", fill = "pink", alpha = 0.4 ) +
  scale_color_brewer()+
  geom_boxplot(data = mod6, aes(x=Month, y=T_bot, na.rm = TRUE, fill = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer()+
  geom_boxplot(data = daily_6, aes(x=month, y=temp_daily_avg, na.rm = TRUE, fill = "Obs"), alpha = 0.4) + 
  scale_color_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "402 Temp Observations") 
?scale_color_manual

#7 - temp only
ggplot() +
  geom_boxplot(data = mod7, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod7, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_7, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "5273 Temp Observations") 

#14 - temp only
ggplot() +
  geom_boxplot(data = mod14, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod14, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_14, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "2232 Temp Observations") 

#31
#temp
ggplot() +
  geom_boxplot(data = mod31, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod31, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_31, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1381 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod31, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod31, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_31, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "1381 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod31, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod31, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_31, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "739 DO Observations") 

#30
#temp
ggplot() +
  geom_boxplot(data = mod30, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod30, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_30, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1040 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod30, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod30, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_30, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "1040 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod30, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod30, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_30, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "292 DO Observations") 

#3
#temp
ggplot() +
  geom_boxplot(data = mod3, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod3, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_3, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "578 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod3, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod3, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_3, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "565 pH Observations")   

#44
#temp
ggplot() +
  geom_boxplot(data = mod44, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod44, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_44, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "2299 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod44, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod44, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_44, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "920 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod44, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod44, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_44, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "2299 DO Observations") 

#18
#temp
ggplot() +
  geom_boxplot(data = mod18, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod18, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_18, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "530 Temp Observations")
#do
ggplot() +
  geom_boxplot(data = mod18, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod18, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_18, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "425 DO Observations")

#33
#temp
ggplot() +
  geom_boxplot(data = mod33, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod33, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_33, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "821 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod33, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod33, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_33, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "821 pH Observations")   

#19
#temp
ggplot() +
  geom_boxplot(data = mod19, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod19, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_19, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1951 Temp Observations")
#do
ggplot() +
  geom_boxplot(data = mod19, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod19, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_19, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "1461 DO Observations") 

#38
#temp
ggplot() +
  geom_boxplot(data = mod38, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod38, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_38, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "91 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod38, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod38, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_38, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "71 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod38, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod38, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_38, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "86 DO Observations") 

#28
#temp
ggplot() +
  geom_boxplot(data = mod28, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod28, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_28, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "325 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod28, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod28, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_28, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "204 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod28, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod28, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_28, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "300 DO Observations") 

#37
#temp
ggplot() +
  geom_boxplot(data = mod37, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod37, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_37, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "82 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod37, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod37, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_37, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "56 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod37, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod37, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_37, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "72 DO Observations") 

#39
#temp
ggplot() +
  geom_boxplot(data = mod39, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod39, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_39, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "530 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod39, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod39, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_39, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "530 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod39, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod39, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_39, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "530 DO Observations") 

#17
#temp
ggplot() +
  geom_boxplot(data = mod17, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod17, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_17, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1517 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod17, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod17, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_17, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "1525 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod17, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod17, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_17, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "457 DO Observations") 
sum(!is.na(daily_45$temp_daily_avg))
sum(!is.na(daily_45$pH_daily_avg))
sum(!is.na(daily_45$do_daily_avg))

#45
#temp
ggplot() +
  geom_boxplot(data = mod45, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod45, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_45, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "2751 Temp Observations")
#do
ggplot() +
  geom_boxplot(data = mod45, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod45, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_45, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "2265 DO Observations") 


################
#unsat analysis#
################









