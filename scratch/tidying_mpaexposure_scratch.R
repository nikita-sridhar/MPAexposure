#scratch for making a function
create_summmary_stats <- function(x){
  summary <- mpa %>%
    filter(period == x) %>%
    group_by(File) %>% 
    summarize(across(c(T_surf, DO_mgL, pH_surf), 
                     list(mean = mean, quantile = ~ quantile(.x, 0.1)))) %>% 
    rename(T_mean = T_surf_mean, 
           DO_mean = DO_mgL_mean, 
           pH_mean = pH_surf_mean, 
           T_low10 = T_surf_quantile, 
           DO_low10 = DO_mgL_quantile, 
           pH_low10 = pH_surf_quantile) %>%
    merge(mpa_centroids,  by = "File")
}

sum_historic2 <- create_summmary_stats("historic")


#scratch for simplifying for loop

mpaslist = unique(mpa$File) 
mpas = rep(NA, 365*121)
julianday = rep(1:365, 121)
T_clim = rep(NA, 365*121)
pH_clim = rep(NA, 365*121)
DO_clim = rep(NA, 365*121)

#Set up a vector of julian day assignment for the 15th of each month and the first and last day of the year
x_in <- yday(as.Date(c("2000-01-01", "2000-01-15","2000-02-15","2000-03-15","2000-04-15","2000-05-15","2000-06-15",
                       "2000-07-15","2000-08-15","2000-09-15","2000-10-15","2000-11-15","2000-12-15", "2000-12-31")))

# creating a list of all the days of the year not included in x_in to interpolate to.
x_out <- (1:365)
x_out <- x_out[!(x_out %in% x_in)] #removing the days of the year we already have values for

#HISTORIC

mpa_historic_climatology <- mpa_climatology %>%
  filter(period == "historic")

for (i in 1:length(mpaslist)){
  print(i)
  d = mpa_climatology %>% filter(File == mpaslist[i]) %>% select(T_clim)
  #use a weighted average to get these
  Dec31 = as.numeric(( ((16/30) * (d[12,2])) + ((14/30) * d[1,2]) ))
  Jan1 = as.numeric(( ((14/30) * (d[12,2])) + ((16/30) * d[1,2]) ))
  # a list of y-values of the climatological temp on each of the days in x_in
  y_in <- c(Jan1, d$T_clim, Dec31)
  
  mod = approx(x = x_in, y = y_in, xout = x_out) 
  mpas[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365)
  T_clim[((i-1)*365+1):(i*365)] <- mod$y
}

historic_tempdata <- data.frame(mpas, julianday,T_clim) %>%
  rename(File = mpas)



mpa_climatology_temp <- mpa_climatology %>%
  mutate(Day = 15)

julian_15 <- mpa %>%
  filter(Day == 15)

x_out <- rep(NA, 365*363)


test <- approx(x = mpa_climatology_temp$T_clim, y = mpa_climatology_temp$Day, xout = x_out )

  
