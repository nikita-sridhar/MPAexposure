#trying to add diff btwn variables as an ifelse statement - haven't fully figured out yet

make_anom_map <- function(mpa_summary_df, sumstat, periodt, diff = FALSE){
  
  anom_points <- st_as_sf((mpa_summary_df %>% filter(period == periodt)), 
                          coords = c("degx","degy"))
  
  #calculating diff btwn time points for given sumstat
  period_diff_anom_points <- st_as_sf((mpa_summary_df %>%
                                         pivot_wider(names_from = period,
                                                     values_from = sumstat,
                                                     id_cols = c(File, degx, degy)) %>%
                                         mutate(mid_hist_diff = midcen - historic,
                                                end_hist_diff = endcen - historic,
                                                end_mid_diff = endcen - midcen)),
                                      coords = c("degx","degy"))
  
  #finding equal interval breaks based on all time periods and w diff btwn time periods
  all_breaks <- classInt::classIntervals(mpa_summary_df[[sumstat]], 10, "equal")
  
  all_diff_breaks <- classInt::classIntervals(c(period_diff_anom_points$mid_hist_diff,period_diff_anom_points$end_hist_diff,period_diff_anom_points$end_mid_diff), 10, "equal")
  
  if_else(diff = FALSE,
          
          #if parameter for diff is TRUE, plot just variable of interest
          tm_shape(CA_shp) + #basemap
            tm_fill(col = "#ccebc5") +
            
            tm_shape(anom_points) +
            tm_dots(col = sumstat, size = 0.05,
                    breaks = all_breaks$brks) +
            
            tm_layout(bg.color = "#a6cee3", 
                      main.title = "Annual # of anomalous heat events",
                      main.title.position = "center",
                      main.title.size = .95,
                      legend.title.size = .9,
                      legend.text.size = .5),
          
          #if parameter for diff is FALSE, plot difference between time periods
          tm_shape(CA_shp) + #basemap
            tm_fill(col = "#ccebc5") +
            
            tm_shape(anom_points) +
            tm_dots(col = sumstat, size = 0.05,
                    breaks = all_breaks$brks) +
            
            tm_layout(bg.color = "#a6cee3", 
                      main.title = "Annual # of anomalous heat events",
                      main.title.position = "center",
                      main.title.size = .95,
                      legend.title.size = .9,
                      legend.text.size = .5)
  )
}

make_anom_map(temp_mpa_summary, "annual_days_abovethresh", "historic")
make_anom_map(temp_mpa_summary, "annual_days_abovethresh", "historic", TRUE)


make_anom_map(temp_mpa_summary, "annual_days_abovethresh", "midcen")
make_anom_map(temp_mpa_summary, "annual_days_abovethresh", "endcen")



  anom_points <- st_as_sf((temp_mpa_summary %>% filter(period == "historic")), 
                          coords = c("degx","degy"))
  
  
  #finding equal interval breaks based on all time periods
  all_breaks <- classInt::classIntervals(temp_mpa_summary$`Mean num of events in a year`, 10, "equal")
  
  tm_shape(CA_shp) + #basemap
    tm_fill(col = "#ccebc5") +
    
    tm_shape(anom_points) +
    tm_dots(col = "Mean num of events in a year", size = 0.05,
            breaks = all_breaks$brks) +
    
    tm_layout(bg.color = "#a6cee3", 
              main.title = "title",
              main.title.position = "center",
              main.title.size = .8,
              legend.title.size = .9,
              legend.text.size = .5)
  mpa_summary_df_period <- mpa_summary_df %>% filter(period == periodt)
  period_breaks <- classInt::classIntervals(mpa_summary_df_period[[sumstat]], 8, "equal")  


  #######keep getting error bc of way im indexing sumstat in classintervals. trying to fix this.
  make_anom_map <- function(mpa_summary_df, sumstat, periodt, title)
    
    anom_points <- st_as_sf((temp_mpa_summary %>% filter(period == "endcen")), 
                            coords = c("degx","degy"))
    
    #finding equal interval breaks based on all time periods
    all_breaks <- classInt::classIntervals(temp_mpa_summary$"Mean num of events in a year", 10, "equal")
    
    all_breaks_tile <- ntile(temp_mpa_summary$"Mean num of events in a year", n =10)
 
    
    
    #map with equal breaks across periods
    tm_shape(CA_shp) + #basemap
      tm_fill(col = "#ccebc5") +
      
      tm_shape(anom_points) +
      tm_dots(col = "Mean num of events in a year", size = 0.05, 
              breaks = all_breaks[["brks"]]) +
      
      tm_layout(bg.color = "#a6cee3", 
                main.title.position = "center",
                main.title.size = .8,
                legend.title.size = .9,
                legend.text.size = .5)
  
  
  make_anom_map(temp_mpa_summary, "Mean num of events in a year", "historic", 
                "Annual # of anomalous heat events in historic period")   

