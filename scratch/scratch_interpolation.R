#trying to replace for loops for interpolating julian day values from climatology


#old - for loop method:

mpaslist = unique(mpa$File) 
mpas_test = rep(NA, 365*121)
julianday = rep(1:365, 121)
T_clim_test = rep(NA, 365*121)


x_in <- yday(as.Date(c("2000-01-01", "2000-01-15","2000-02-15","2000-03-15","2000-04-15","2000-05-15","2000-06-15",
                       "2000-07-15","2000-08-15","2000-09-15","2000-10-15","2000-11-15","2000-12-15", "2000-12-31")))

x_out <- (1:365)
x_out <- x_out[!(x_out %in% x_in)]

mpa_historic_climatology <- mpa_climatology %>%
  filter(period == "historic")

#HISTORIC - this is repeated per time period and per variable (temp/pH/DO)
for (i in 1:length(mpaslist)){
  d = mpa_historic_climatology %>% filter(File == mpaslist[i]) %>% select(File,Month,T_clim) #Temp values from historic climatology

    Dec31 = #weighted avg to get end of year and start of year.   
    as.numeric(( ((16/30) * (d[12,3]))  + #changing 12,2 to 12,3 --> 3rd column refers to T_clim in december - after 15th of december
                   ((14/30) * d[1,3]) )) #before 15th of the month - jan t clim value
  
  
    Jan1 = 
    as.numeric(( ((14/30) * (d[12,3])) + 
                     ((16/30) * d[1,3]) ))
 
     # a list of y-values of the climatological temp on each of the days in x_in
  y_in <- c(Jan1, d$T_clim, Dec31)
  
  mod = approx(x = x_in, y = y_in, xout = x_out) 
  mpas_test[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365)
  T_clim_test[((i-1)*365+1):(i*365)] <- mod$y
}

historic_tempdata_test = data.frame(mpas_test, julianday,T_clim_test)
historic_tempdata_test <- historic_tempdata_test %>%
  rename(File = mpas_test) %>%
  mutate(period = "historic") %>%
  rename(interp_temp = T_clim_test)



interpolate <- function(periodt){
  
  mpa_period_climatology <- mpa_climatology %>%
    filter(period == periodt)
  
  for (i in 1:length(mpaslist)){
    d = mpa_period_climatology %>% filter(File == mpaslist[i]) %>% select(File,Month,T_clim)
    #use a weighted average. for dec 31, slightly more than half weight to dec, little less than   half weight to jan. for jan 1, slightly more weight to jan, little less weight to dec
    Dec31 = as.numeric(( ((16/30) * (d[12,3])) + 
                           ((14/30) * d[1,3]) )) 
    Jan1 = as.numeric(( ((14/30) * (d[12,3])) + 
                          ((16/30) * d[1,3]) ))
    # a list of y-values of the climatological temp on each of the days in x_in
    y_in <- c(Jan1, d$T_clim, Dec31)
    
    mod = approx(x = x_in,  #julian day for the 15th of each month &first/ last day of the year
                 y = y_in, #temp values per day
                 xout = x_out) #list of all the days of the year not  in x_in to interpolate to
    
    mpas[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365)
    T_clim[((i-1)*365+1):(i*365)] <- mod$y
  }}


#final function - getting a warning: Warning: number of items to replace is not a multiple of replacement length. trying to resolve this. 
interpolate <- function(periodt)
  
  mpa_period_climatology <- mpa_climatology %>%
    filter(period == "historic")
  
  for (i in 1:length(mpaslist)){
    d = mpa_period_climatology %>% filter(File == mpaslist[i]) %>% select(File,Month,T_clim)
    #use a weighted average. for dec 31, slightly more than half weight to dec, little less than   half weight to jan. for jan 1, slightly more weight to jan, little less weight to dec
    Dec31 = as.numeric(( ((16/30) * (d[12,3])) + 
                           ((14/30) * d[1,3]) )) 
    Jan1 = as.numeric(( ((14/30) * (d[12,3])) + 
                          ((16/30) * d[1,3]) ))
    # a list of y-values of the climatological temp on each of the days in x_in
    y_in <- c(Jan1, d$T_clim, Dec31)
    
    mod = approx(x = x_in,  #julian day for the 15th of each month &first/ last day of the year
                 y = y_in, #temp values per day in x_in
                 xout = x_out) #list of all the days of the year not  in x_in to interpolate to
  
    #this is where warning starts:
    mpas[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365) 
    T_clim[((i-1)*365+1):(i*365)] <- mod$y
  }
  
  
  df <- data.frame(mpas, julianday, T_clim) %>%
    rename(File = mpas) %>%
    mutate(period = "historic") %>%
    rename(interp_temp = T_clim)
  
  assign(x = paste("tempdata","historic", sep="_"), value = df, envir = globalenv())
  

    



interpolate("historic")  

