#File created 11/6/2023
#Authored by Nikita Sridhar

#to figure out how sites were chosen, look at old model-obs file! 
#Comparison of model and observational T/pH/DO data using PDFs and CDFs to determine
#model fit

library(tidyverse)
library(ggpubr)
library(lubridate)
library(purrr)
library(broom)

#new
# Get paths of all .txt files in a specific directory

#obs sites metadata
sites <- read_csv("data/processeddata/obs_sites.csv") %>%
  rename(File = 1) 

#load model data at observational data sites
load_mod <- function(mod_run){

  file_paths <- list.files(path = paste0("data/rawdata/model/for_obs_comparison/30m_avg_values/Sites_",mod_run), 
                           pattern = "\\.txt$", full.names = TRUE)
  
  mod_df <- file_paths %>% 
    set_names(basename(.)) %>% # Optional: cleans the path to just show "filename.txt"
    map_dfr(~read_csv(.x, col_names = FALSE), #for all filepaths, read csv and bind rows into 1 df
            .id = "file_source") %>% #per row, include the id as the file name
    
    rename(Year = X1,
           Month = X2,
           Day = X3,
           T = X4,
           pH = X5,
           DO = X6) %>%
    
    mutate(File = as.double(stringr::str_extract(file_source, "\\d+")),
           julianday = yday(make_date(Year,Month,Day))) %>%
    
    relocate(File, .before = 1) %>%
    
    filter(Year >= 2004 & Year < 2022) %>%
    select(-file_source) %>%
    
    left_join(sites, by = "File")
  
    
  return(mod_df)
  }

GFDL_mod <- load_mod("GFDL")
IPSL_mod <- load_mod("IPSL")
HADLEY_mod <- load_mod("HADL")

    
# obs setup ---------------------------------------------------------------

obs_all <- load("data/rawdata/obs/for model-obs comparison/clean_all_light.RData")
obs_all <- light %>%
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

obs <- obs_all %>% #calculating daily avgs for temp, ph, do
  group_by(site, time_utc, longitude, latitude) %>%
  summarize(depth_m = mean(depth_m, na.rm=TRUE),
            T = mean(t_C, na.rm = TRUE), 
            pH = mean(pH_total, na.rm = TRUE), 
            DO = mean(do_umolkg, na.rm = TRUE),
            .groups = "drop") %>%
  
  left_join(sites, by = c("site", "longitude", "latitude")) %>%
  
  #add date/month/year
  mutate(Year = year(time_utc),
         Month = month(time_utc),
         Day = day(time_utc),
         julianday = yday(time_utc))

#turning NaN into NA
obs$T[is.nan(obs$T)]<-NA
obs$pH[is.nan(obs$pH)]<-NA
obs$DO[is.nan(obs$DO)]<-NA



# Summary stat comparison -------------------------------------------------
#note: interpolation function has been edited from ensemble_mean to account for large
#data gaps in obs data (i.e. sometimes only one dataset for temp and no ph/do)
#should try this new code w actual model projections - might be better and slightly diff

#OBS

#"climatology"
obs_clim <- obs %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T, na.rm = TRUE), 
            pH_clim = mean(pH, na.rm = TRUE), 
            DO_clim =   mean(DO, na.rm = TRUE),
            .groups = "drop")



GFDLmod_clim <- GFDL_mod %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T, na.rm = TRUE), 
            pH_clim = mean(pH, na.rm = TRUE), 
            DO_clim =   mean(DO, na.rm = TRUE),
            .groups = "drop")

IPSLmod_clim <- IPSL_mod %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T, na.rm = TRUE), 
            pH_clim = mean(pH, na.rm = TRUE), 
            DO_clim =   mean(DO, na.rm = TRUE),
            .groups = "drop")

HADLEYmod_clim <- HADLEY_mod %>%
  group_by(File, Month) %>%
  summarise(T_clim = mean(T, na.rm = TRUE), 
            pH_clim = mean(pH, na.rm = TRUE), 
            DO_clim =   mean(DO, na.rm = TRUE),
            .groups = "drop")


#interpolation
obslist = unique(obs$File) #just to get mpa names doesn't matter that it's ipsl
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


GFDLmod_temp_interp <- interpolate(GFDLmod_clim, "T_clim")
GFDLmod_pH_interp <- interpolate(GFDLmod_clim, "pH_clim")
GFDLmod_DO_interp <- interpolate(GFDLmod_clim, "DO_clim")

IPSLmod_temp_interp <- interpolate(IPSLmod_clim, "T_clim")
IPSLmod_pH_interp <- interpolate(IPSLmod_clim, "pH_clim")
IPSLmod_DO_interp <- interpolate(IPSLmod_clim, "DO_clim")

HADLEYmod_temp_interp <- interpolate(HADLEYmod_clim, "T_clim")
HADLEYmod_pH_interp <- interpolate(HADLEYmod_clim, "pH_clim")
HADLEYmod_DO_interp <- interpolate(HADLEYmod_clim, "DO_clim")



obs_interp_all <- obs_temp_interp %>%
  left_join(obs_pH_interp, by = c("File", "julianday")) %>%
  left_join(obs_DO_interp, by = c("File", "julianday"))

GFDLmod_interp_all <- GFDLmod_temp_interp %>%
  left_join(GFDLmod_pH_interp, by = c("File", "julianday")) %>%
  left_join(GFDLmod_DO_interp, by = c("File", "julianday"))

IPSLmod_interp_all <- IPSLmod_temp_interp %>%
  left_join(IPSLmod_pH_interp, by = c("File", "julianday")) %>%
  left_join(IPSLmod_DO_interp, by = c("File", "julianday"))

HADLEYmod_interp_all <- HADLEYmod_temp_interp %>%
  left_join(HADLEYmod_pH_interp, by = c("File", "julianday")) %>%
  left_join(HADLEYmod_DO_interp, by = c("File", "julianday"))

#calculate seasonal SD
obs_seasonal_SD <- obs_clim %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

GFDLmod_seasonal_SD <- GFDLmod_clim %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

IPSLmod_seasonal_SD <- IPSLmod_clim %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

HADLEYmod_seasonal_SD <- HADLEYmod_clim %>%
  group_by(File) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

#make summary stats:
calculate_sumstat <- function(arg, interp_all, seasonalSD){
  
  sumstat <- arg %>%
    
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
    select(-any_of(c("julianday", "Year","Month", "Day", "time_utc"))) 
  
  return(sumstat)
  }

obs_sumstat <- calculate_sumstat(obs, obs_interp_all, obs_seasonal_SD)
GFDLsumstat <- calculate_sumstat(GFDL_mod, GFDLmod_interp_all, GFDLmod_seasonal_SD)
IPSLsumstat <- calculate_sumstat(IPSL_mod, IPSLmod_interp_all, IPSLmod_seasonal_SD)
HADLEYsumstat <- calculate_sumstat(HADLEY_mod, HADLEYmod_interp_all, HADLEYmod_seasonal_SD)

#add model id 
names(IPSLsumstat)[9:ncol(IPSLsumstat)] <- paste0("IPSL_", names(IPSLsumstat)[9:ncol(IPSLsumstat)])
names(GFDLsumstat)[9:ncol(GFDLsumstat)] <- paste0("GFDL_", names(GFDLsumstat)[9:ncol(GFDLsumstat)])
names(HADLEYsumstat)[9:ncol(HADLEYsumstat)] <- paste0("HADLEY_", names(HADLEYsumstat)[9:ncol(HADLEYsumstat)])

ensemble_sumstat <- IPSLsumstat %>%
  
  left_join(GFDLsumstat, by = names(IPSLsumstat)[1:8]) %>%
  left_join(HADLEYsumstat, by = names(IPSLsumstat)[1:8]) %>%
  
  rowwise() %>%
  mutate(
    T_mean = mean(c(IPSL_T_mean, GFDL_T_mean, HADLEY_T_mean), na.rm = TRUE),
    T_low10 = mean(c(IPSL_T_low10, GFDL_T_low10, HADLEY_T_low10), na.rm = TRUE),
    T_high10 = mean(c(IPSL_T_high10, GFDL_T_high10, HADLEY_T_high10), na.rm = TRUE),
    T_eventSD = mean(c(IPSL_T_eventSD, GFDL_T_eventSD, HADLEY_T_eventSD), na.rm = TRUE),
    T_seasonalSD = mean(c(IPSL_T_seasonalSD, GFDL_T_seasonalSD, HADLEY_T_seasonalSD), na.rm = TRUE),
    
    pH_mean = mean(c(IPSL_pH_mean, GFDL_pH_mean, HADLEY_pH_mean), na.rm = TRUE),
    pH_low10 = mean(c(IPSL_pH_low10, GFDL_pH_low10, HADLEY_pH_low10), na.rm = TRUE),
    pH_high10 = mean(c(IPSL_pH_high10, GFDL_pH_high10, HADLEY_pH_high10), na.rm = TRUE),
    pH_eventSD = mean(c(IPSL_pH_eventSD, GFDL_pH_eventSD, HADLEY_pH_eventSD), na.rm = TRUE),
    pH_seasonalSD = mean(c(IPSL_pH_seasonalSD, GFDL_pH_seasonalSD, HADLEY_pH_seasonalSD), na.rm = TRUE),
    
    DO_mean = mean(c(IPSL_DO_mean, GFDL_DO_mean, HADLEY_DO_mean), na.rm = TRUE),
    DO_low10 = mean(c(IPSL_DO_low10, GFDL_DO_low10, HADLEY_DO_low10), na.rm = TRUE),
    DO_high10 = mean(c(IPSL_DO_high10, GFDL_DO_high10, HADLEY_DO_high10), na.rm = TRUE),
    DO_eventSD = mean(c(IPSL_DO_eventSD, GFDL_DO_eventSD, HADLEY_DO_eventSD), na.rm = TRUE),
    DO_seasonalSD = mean(c(IPSL_DO_seasonalSD, GFDL_DO_seasonalSD, HADLEY_DO_seasonalSD), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(1:8, T_mean, T_low10, T_high10, T_eventSD, T_seasonalSD,
         pH_mean, pH_low10, pH_high10, pH_eventSD, pH_seasonalSD,
         DO_mean, DO_low10, DO_high10, DO_eventSD, DO_seasonalSD)

names(ensemble_sumstat)[9:ncol(ensemble_sumstat)] <- paste0("ensemble_", names(ensemble_sumstat)[9:ncol(ensemble_sumstat)])

# compare mod and obs sumstat ---------------------------------------------

make_comparison <- function(mod_sumstat){
  mod_comparison <- obs_sumstat %>%
    select(File, everything()) %>%
    rename_with(~ paste0(.x, "_obs"), -File) %>%
    inner_join(mod_sumstat %>%
                 rename_with(~ paste0(.x, "_mod"), -File),
               by = "File")
  return(mod_comparison)
}

ensemble_comparison <- make_comparison(ensemble_sumstat)
GFDL_comparison <- make_comparison(GFDLsumstat)
IPSL_comparison <- make_comparison(IPSLsumstat)
HADLEY_comparison <- make_comparison(HADLEYsumstat)

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
run_regression <- function(comparison_df) {
  
  mod_run <- sub("_.*", "", deparse(substitute(comparison_df)))
  
  
  #table of regression results
  regression_df <- map_dfr(stats, function(stat) {
    
    obs_col <- paste0(stat, "_obs")
    mod_col <- paste0(mod_run,"_",stat, "_mod")
    
    dat <- comparison_df %>%
      select(File, all_of(obs_col), all_of(mod_col)) %>%
      drop_na()
    
    model <- lm(reformulate(obs_col, response = mod_col),
                data = dat)
    
    tidy(model) %>%
      filter(term == obs_col) %>%
      transmute(statistic = stat,
                n = nrow(dat),
                slope = estimate,
                slope_SE = std.error,
                p_value = p.value,
                R2 = summary(model)$r.squared)
  })
  
  #plot data
  plot_data <- comparison_df %>%
    select(File, region_obs, any_of(paste0(stats, "_obs")),
           any_of(paste0(mod_run, "_", stats, "_mod"))) %>%
   
    rename_with(
      ~ sub(paste0("^", mod_run, "_"), "", .x),
      starts_with(mod_run)
    ) %>%
    pivot_longer(
      cols = -c(File,region_obs),
      names_to = c("statistic", "type"),
      names_pattern = "(.*)_(obs|mod)$",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = type,
      values_from = value
    ) 
  #%>% drop_na(obs, mod)
  
  
  # Plot observed vs. modeled values
  plot <- ggplot(plot_data, aes(x = obs, y = mod)) +
    
    # 1:1 reference line
    geom_abline(
      aes(linetype = "1:1 relationship"),
      slope = 1,
      intercept = 0,
      color = "black"
    ) +
    
    geom_point() +
    
    # Fitted linear regression
    geom_smooth(
      aes(color = "Fitted linear regression"),
      method = "lm",
      se = FALSE,
      linetype = "solid"
    ) +
    
    #r2 and p value for each facet
    geom_text(
      data = regression_df,
      aes(
        x = -Inf,
        y = Inf,
        label = paste0(
          "R² = ", round(R2, 2),
          "\np = ", signif(p_value, 2)
        )
      ),
      inherit.aes = FALSE,
      hjust = -0.1,
      vjust = 1.5
    ) +
    
    # Line legend
    scale_linetype_manual(
      name = NULL,
      values = c("1:1 relationship" = "dotted")
    ) +
    
    scale_color_manual(
      name = NULL,
      values = c("Fitted linear regression" = "blue")
    ) +
    
    facet_wrap(
      ~ statistic,
      scales = "free", ncol=5
    ) +
    
    labs(
      x = "Observed",
      y = "Modeled",
      caption = paste(
        "Black dotted line represents the 1:1 relationship (modeled = observed).",
        "Blue solid line represents the fitted linear regression.",
        "R² values indicate the proportion of variation in modeled values",
        "explained by observed values."),
      title = paste(mod_run,"model vs. observation")
    ) +
    
    theme_classic()
  
  
  
  return(list(regression_df = regression_df, plot_data = plot_data, plot = plot))
  
}


ensemble_plot <- run_regression(ensemble_comparison)$plot
GFDL_plot <- run_regression(GFDL_comparison)$plot
IPSL_plot <- run_regression(IPSL_comparison)$plot
HADLEY_plot <- run_regression(HADLEY_comparison)$plot

ensemble_plot
GFDL_plot
IPSL_plot
HADLEY_plot








