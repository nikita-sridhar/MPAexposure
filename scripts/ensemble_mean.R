#9-24-26

#code to explore and compare the analysis in mpaexposure.qmd with all solutions: IPSL, GFDL, and Hadley. 
#summary statistics generated for each model are averaged the find the ensemble mean
#ensemble mean is then used for analyses that use the summary statistics (i.e. pca, pc change, regression)

load_packages <- function() {
library(tidyverse)
library(patchwork)
library(lubridate)
library(data.table)
library(factoextra)
library(broom)
library(cowplot)
library(respR)
library(here)
library(lattice)
library(RcppRoll)
library(RColorBrewer)
library(gplots)
library(ggpmisc)
library(dendextend)
library(colourvalues)
library(zoo)
library(terra) #needed for tmap?
library(tmap) 
library(sf) 
library(stringr)
library(heatmaply)
library(ggpubr)
library(ggrepel)
library(flextable)
library(clipr)
library(emmeans)}

load_packages()

#load file and add periods
IPSL <- read_csv(here("data/processeddata/model/IPSLmpa.csv"))
GFDL <- read_csv(here("data/processeddata/model/GFDLmpa.csv"))
HADLEY <- read_csv(here("data/processeddata/model/HADLEYmpa.csv"))

#add regions -------------------------------------------------------------------
channel <- c("Anacapa Island FMCA", "Anacapa Island FMR", "Anacapa Island SMCA", "Anacapa Island SMR", "Anacapa Island Special Closure", 
             "Arrow Point to Lion Head Point SMCA", "Begg Rock SMR", "Blue Cavern Offshore SMCA", "Blue Cavern Onshore SMCA (No-Take)", 
             "Carrington Point SMR", "Casino Point SMCA (No-Take)", "Cat Harbor SMCA", "Farnsworth Offshore SMCA", "Farnsworth Onshore SMCA", 
             "Footprint FMR", "Footprint SMR", "Gull Island FMR", "Gull Island SMR", "Harris Point FMR", "Harris Point SMR", "Judith Rock SMR", 
             "Long Point SMR", "Lover's Cove SMCA", "Painted Cave SMCA", "Richardson Rock FMR", "Richardson Rock SMR", "San Miguel Island Special Closure", 
             "Santa Barbara Island FMR", "Santa Barbara Island SMR", "Scorpion FMR", "Scorpion SMR", "Skunk Point SMR", "South Point FMR", "South Point SMR")

mpa_centroids<- read_csv(here("data/rawdata/MPA_polygons.csv")) %>%
  select(-Area_sq_mi, -Type) %>%
  mutate(File = sub("^", "tphdo_mpa_", OBJECTID),
         region = ifelse(degy >= 37.29, "norca", 
                         ifelse(degy > 34.8, "centralca",
                                ifelse(NAME %in% channel, "channel",
                                "socal"))))
#clean mpa files ---------------------------------------------------------------
clean_mpa <- function(mod){
  mod <- mod %>%
  mutate(DO_mmolL = DO_surf/1000,
         DO_mgL = convert_DO(DO_mmolL, from = "mmol/L", to = "mg/L")) %>% 
  #add periods
  filter(Year <= 2030|
           Year >= 2035 & Year <= 2065|
           Year >= 2070 & Year <= 2100) %>%
  mutate(period = case_when((Year <= 2030) ~ "presentday", 
                            (Year %in% c(2035:2065)) ~ "midcen",
                            (Year %in% c(2070:2100)) ~ "endcen"),
         Date = make_date(Year, Month, Day),
         julianday = yday(Date),
         File = substr(File, 1, nchar(File)-4)) %>%
  rename(Temp = T_surf,
         DO = DO_mgL,
         pH = pH_surf) %>%
    
  select(-T_bot, -DO_bot, -pH_bot,-DO_surf,-DO_mmolL) %>%
  #add regions  
  left_join(mpa_centroids, by = "File")
    
return(mod)}

IPSLmpa <- clean_mpa(IPSL)
GFDLmpa <- clean_mpa(GFDL)
HADLEYmpa <- clean_mpa(HADLEY)

rm(IPSL)
rm(GFDL)
rm(HADLEY)

#calculate climatology for each MPA for each time period
make_climatology <- function(mod_mpa){
  mod_mpa %>%
    group_by(File, Month, period) %>%
    summarise(T_clim = mean(Temp), 
              pH_clim = mean(pH), 
              DO_clim =   mean(DO))
}

IPSLclim <- make_climatology(IPSLmpa)
GFDLclim <- make_climatology(GFDLmpa)
HADLEYclim <- make_climatology(HADLEYmpa)


#interpolation for event SD ----------------------------------------------------
mpaslist = unique(IPSLmpa$File) #just to get mpa names doesn't matter that it's ipsl
mpas = rep(NA, 365*121)
julianday = rep(1:365, 121)
interp = rep(NA, 365*121)

#Set up a vector of julian day assignment for the 15th of each month and the first and last day of the year
x_in <- yday(as.Date(c("2001-01-01", 
                       "2001-01-15","2001-02-15","2001-03-15","2001-04-15",
                       "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                       "2001-09-15","2001-10-15","2001-11-15","2001-12-15", 
                       "2001-12-31")))

# creating a list of all the days of the year to interpolate to.
x_out <- (1:365) 


interpolate <- function(mod_clim, periodt, variable){
  
  mpa_period_climatology <- mod_clim %>%
    filter(period == periodt)
  
  for (i in 1:length(mpaslist)){ #for each mpa
    
    d <- mpa_period_climatology %>% #for given mpa iterating through, selects var from clim
      filter(File == mpaslist[i]) %>% 
      select(File,Month, !!sym(variable))
    
    Dec31 = as.numeric(( ((16/30) * (d[12,3])) +  
                           ((14/30) * d[1,3]) )) 
    Jan1 = as.numeric(( ((14/30) * (d[12,3])) + 
                          ((16/30) * d[1,3]) ))
    
    y_in <- c(Jan1, d[[variable]], Dec31)
    
    mod <- approx(x = x_in, #days to interpolate from
                  y = y_in, #temp values per day in x_in
                  xout = x_out) #1-365 days to interpolate out to
    
    mpas[((i-1)*365+1):(i*365)] <- mpaslist[i] #rep(mpaslist[1], 365)
    interp[((i-1)*365+1):(i*365)] <- mod$y
  }
  
  df <- data.frame(mpas, julianday, interp) %>%
    rename(File = mpas) %>%
    mutate(period = periodt) %>%
    rename(!!sym(variable) := interp)
  
  return(df)
  
}


#IPSL interpolations
IPSL_interp_all <- map_dfr(
  c("presentday", "midcen", "endcen"),
  function(period_name) {
    
    temp <- interpolate(IPSLclim, period_name, "T_clim")
    pH <- interpolate(IPSLclim, period_name, "pH_clim")
    DO <- interpolate(IPSLclim, period_name, "DO_clim")
    
    temp %>%
      left_join(pH, by = c("File", "julianday", "period")) %>%
      left_join(DO, by = c("File", "julianday", "period"))
  }
)

#GFDL interpolations
GFDL_interp_all <- map_dfr(
  c("presentday", "midcen", "endcen"),
  function(period_name) {
    
    temp <- interpolate(GFDLclim, period_name, "T_clim")
    pH <- interpolate(GFDLclim, period_name, "pH_clim")
    DO <- interpolate(GFDLclim, period_name, "DO_clim")
    
    temp %>%
      left_join(pH, by = c("File", "julianday", "period")) %>%
      left_join(DO, by = c("File", "julianday", "period"))
  }
)

#HADLEY interpolations
HADLEY_interp_all <- map_dfr(
  c("presentday", "midcen", "endcen"),
  function(period_name) {
    
    temp <- interpolate(HADLEYclim, period_name, "T_clim")
    pH <- interpolate(HADLEYclim, period_name, "pH_clim")
    DO <- interpolate(HADLEYclim, period_name, "DO_clim")
    
    temp %>%
      left_join(pH, by = c("File", "julianday", "period")) %>%
      left_join(DO, by = c("File", "julianday", "period"))
  }
)


#make summary stats ------------------------------------------------------------

#calculate seasonal SD 
IPSLseasonal_SD <- IPSLclim %>%
  group_by(File, period) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

GFDLseasonal_SD <- GFDLclim %>%
  group_by(File, period) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

HADLEYseasonal_SD <- HADLEYclim %>%
  group_by(File, period) %>%
  summarise(T_seasonalSD = sd(T_clim),
            pH_seasonalSD = sd(pH_clim),
            DO_seasonalSD = sd(DO_clim))

#calculate rest of summary stats
calculate_sumstat <- function(mod_mpa, mod_interp_all, mod_seasonalSD){

sumstat <- mod_mpa %>%
  
  merge(mod_interp_all, by = c("File", "julianday", "period")) %>%
  
  #subtract actual values - interpolated climatology values 
  #filtered for only upwelling months!!
  mutate(temp_deviation = Temp - T_clim, #this should be for the given julian day, right?
         pH_deviation = pH - pH_clim,
         DO_deviation = DO - DO_clim) %>%
  filter(Month == c(5,6,7,8,9)) %>%
  
  #find event/seasonal SD and other sum stats 
  group_by(File, period) %>%
  mutate(T_eventSD = sd(temp_deviation),
         pH_eventSD = sd(pH_deviation),
         DO_eventSD = sd(DO_deviation)) %>%
  ungroup() %>%
  select(-T_clim, -pH_clim, -DO_clim, -temp_deviation,
         -pH_deviation, -DO_deviation) %>%
  
  #merge seasonal SD 
  merge(mod_seasonalSD, by = c("File", "period")) %>%
  
  #adding other summary stats - mean, low 10th and upper 10th percentiles
  group_by(File, period) %>%
  mutate(across(c(Temp, DO, pH), 
                list(mean = mean, 
                     low10 = ~ quantile(.x, 0.1),
                     high10 = ~quantile(.x,0.9)))) %>%
  
  # to get to scale of one row per mpa
  select(-...1, -Temp, -pH, -DO, -julianday, -Year, -Month, -Day, -Date) %>%
  distinct(File, .keep_all = TRUE) %>% ungroup()

return(sumstat)}


IPSLsumstat <- calculate_sumstat(IPSLmpa, IPSL_interp_all, IPSLseasonal_SD)
GFDLsumstat <- calculate_sumstat(GFDLmpa, GFDL_interp_all, GFDLseasonal_SD)
HADLEYsumstat <- calculate_sumstat(HADLEYmpa, HADLEY_interp_all, HADLEYseasonal_SD)


#compare PCA ------------------------------------------------------------
make_pca <- function(mod_sumstat){
  
  model_name <- sub("sumstat.*", "", deparse(substitute(mod_sumstat)))
  
  sumstat_4pca <- mod_sumstat %>%
    select(-OBJECTID, -NAME, -File, -SHORTNAME,
         -degx, -degy, -region, -period)

  # PCA on all observations
  pca <- prcomp(sumstat_4pca, scale. = TRUE)

  #varimax rotation
  n_comp <- 2
  raw_loadings <- pca$rotation[, 1:n_comp]
  varimax_res  <- varimax(raw_loadings)
  pca$rotation[, 1:n_comp] <- varimax_res$loadings
  scaled_data <- scale(sumstat_4pca)
  pca$x[, 1:n_comp] <- scaled_data %*% varimax_res$loadings
  
  #Keep first 3 PCs and add metada
  scores <- as.data.frame(pca$x[,1:3])
  scores$MPA <- mod_sumstat$NAME
  scores$MPA_num <- mod_sumstat$OBJECTID
  scores$region <- mod_sumstat$region
  scores$period <- factor(mod_sumstat$period, levels = c("presentday","midcen","endcen"))

  #pca biplot
  region_cols <- c("norca" = "#8da0cb","centralca" = "#fc8d62", 
                   "channel" = "#66c2a5","socal" = "#e78ac3")

  pca_plot <- fviz_pca_biplot(
    pca, axes = c(1,2), repel = TRUE, col.var = "black",
    addEllipses = FALSE, invisible = "ind"
    ) +
  
    geom_point(
      data = scores, 
      aes(PC1, PC2, color = region, shape = period),
      size = 2, alpha = 0.7, inherit.aes = FALSE
    ) + 
    scale_color_manual(values = region_cols) +
    scale_shape_manual(values = c(presentday = 16,midcen = 17,endcen = 15)) +
    labs(title = paste0(model_name, " PCA"))
  
  if (grepl("HADLEY|ensemble", model_name, ignore.case = TRUE)) {
    pca_plot <- pca_plot + scale_x_reverse()
  }


  return(list(scores = scores, plot = pca_plot))

}

#pca 
IPSLpca <- make_pca(IPSLsumstat)
GFDLpca <- make_pca(GFDLsumstat)
HADLEYpca <- make_pca(HADLEYsumstat)

IPSLpca$plot + theme(legend.position = "none") +
GFDLpca$plot + theme(legend.position = "none") +
HADLEYpca$plot



# ensemble mean -----------------------------------------------------------

#add model id 
names(IPSLsumstat)[9:ncol(IPSLsumstat)] <- paste0("IPSL_", names(IPSLsumstat)[9:ncol(IPSLsumstat)])
names(GFDLsumstat)[9:ncol(GFDLsumstat)] <- paste0("GFDL_", names(GFDLsumstat)[9:ncol(GFDLsumstat)])
names(HADLEYsumstat)[9:ncol(HADLEYsumstat)] <- paste0("HADLEY_", names(HADLEYsumstat)[9:ncol(HADLEYsumstat)])

#calculate avg of summary stats btwn all models
ensemble_sumstat <- IPSLsumstat %>%
  
  left_join(GFDLsumstat, by = names(IPSLsumstat)[1:8]) %>%
  left_join(HADLEYsumstat, by = names(IPSLsumstat)[1:8]) %>%
  
  rowwise() %>%
  mutate(
    T_mean = mean(c(IPSL_Temp_mean, GFDL_Temp_mean, HADLEY_Temp_mean), na.rm = TRUE),
    T_low10 = mean(c(IPSL_Temp_low10, GFDL_Temp_low10, HADLEY_Temp_low10), na.rm = TRUE),
    T_high10 = mean(c(IPSL_Temp_high10, GFDL_Temp_high10, HADLEY_Temp_high10), na.rm = TRUE),
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
  


# Run analyses w ensemble mean --------------------------------------------
ensemble_pca <- make_pca(ensemble_sumstat)
print(ensemble_pca$plot)








