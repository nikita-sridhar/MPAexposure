#REPEATING PCA AND REGRESSION ANALYSIS WITH ANOM EVENT METRICS OF EXPOSURE

#merging all events
all_events <- temp_mpa_summary %>%
  merge(pH_mpa_summary, by = c("File", "NAME", "degx", "degy"), suffixes = c("_temp","")) %>%
  merge(DO_mpa_summary,by = c("File", "NAME", "degx", "degy"), suffixes = c("_pH","_DO")) %>%
  merge(sum[,c("File","region")], by = c("File")) #to get the region

#PCA:
make_pca_anom <- function(df, periodt){
  sum_period <- df %>% 
    filter(period_temp == periodt,
           period_pH == periodt,
           period_DO == periodt) 
  sumsub <- sum_period %>% select(meanannual_days_abovethresh, meanannual_avg_temp_abovethresh, 
                                  meanannual_days_belowthresh_pH, meanannual_avg_pH_belowthresh,
                                  meanannual_days_belowthresh_DO, meanannual_avg_DO_belowthresh) #mean_annual_days_abovethresh is for temp, need to rename
  
  if (periodt == "endcen"){
    pca <- prcomp(sumsub[,-1], scale = TRUE)
  } else {
    pca <- prcomp(sumsub, scale = TRUE)
  }
  
  
  fviz_pca_biplot(pca, repel = TRUE,
                  col.var = "black",
                  col.ind = sum_period$region,
                  label ="var",
                  labelsize = 3,
                  addEllipses = TRUE,
                  title = paste("PCA of summary statistics in", periodt,"period")) 
  
}

make_pca_anom(all_events, "historic")
make_pca_anom(all_events, "midcen")
make_pca_anom(all_events, "endcen")
#no strong clustering for PCA


#regressions:
make_regression_anom <- function(sumstat, period1, period2,title){
  
  plot_matrix <- all_events %>%
    filter(period_pH == period1 | period_pH == period2) %>%
    select(sumstat, period_pH, NAME, File, region) %>%
    pivot_wider(names_from = period_pH, values_from = sumstat) 
  
  ggplot(data = plot_matrix, aes_string(x = period1, y = period2)) +
    stat_poly_line(colour = "black") +
    stat_poly_eq(use_label(c("eq", "R2"))) +
    geom_point(aes(colour = region), alpha = 0.7) +
    theme_classic() +
    ggtitle(title)
  
} 
 

make_regression_anom("meanannual_days_belowthresh_pH", "historic", "midcen", "Historic vs. mid century")

