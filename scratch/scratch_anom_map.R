#3-3-23: scratch for difference between anom maps integrated in same function


make_anom_map <- function(mpa_summary_df, sumstat, periodt, 
                          palette = "YlOrRd", diff = FALSE){
  
  #converting df to spatial data format
  anom_points <- st_as_sf((mpa_summary_df %>% filter(period == periodt)), 
                          coords = c("degx","degy"))
  #creating period diff df: calculating diff btwn time points for given sumstat
  period_diff <- temp_mpa_summary %>%
                    pivot_wider(names_from = period,
                                values_from = meanannual_days_abovethresh,
                                id_cols = c(File, degx, degy)) %>%
                    mutate(mid_hist_diff = midcen - historic,
                           end_hist_diff = endcen - historic,
                           end_mid_diff = endcen - midcen)
  #converting period diff df to spatial data format
  period_diff_anom_points <- st_as_sf(period_diff,coords = c("degx","degy"))
  
  #finding equal interval breaks based on all time periods
  all_breaks <- classInt::classIntervals(mpa_summary_df[[sumstat]], 8, "equal")
  all_diff_breaks <- pivot_longer(period_diff, c(mid_hist_diff,end_hist_diff,end_mid_diff))
  all_diff_breaks <- classInt::classIntervals(all_diff_breaks$value, 10, "equal")

   #if diff parameter is false: create regular maps of sumstat
    if (diff == FALSE){
      tm_shape(CA_shp) + #basemap
        tm_fill(col = "#ccebc5") +
        
        tm_shape(anom_points) +
        tm_dots(col = sumstat, palette = palette, size = 0.2, 
                breaks = all_breaks[["brks"]]) +
        
        tm_layout(bg.color = "#a6cee3", 
                  main.title.position = "center",
                  main.title.size = 1.3,
                  main.title = paste(sumstat, periodt),
                  legend.title.size = 1,
                  legend.text.size = .8) 

  #if diff parameter is true: create 3 maps of diff between each time period
   } else {
     #mid-hist diff
     tm_shape(CA_shp) + #basemap
       tm_fill(col = "#ccebc5") +
       
       tm_shape(period_diff_anom_points) +
       tm_dots(col = "mid_hist_diff", palette = palette, size = 0.2, 
               breaks = all_diff_breaks[["brks"]]) +
       
       tm_layout(bg.color = "#a6cee3", 
                 main.title.position = "center",
                 main.title.size = 1.3,
                 main.title = paste("Mid_historic_diff", "_", sumstat),
                 legend.title.size = 1,
                 legend.text.size = .8) 
     #end-hist diff
     tm_shape(CA_shp) + #basemap
       tm_fill(col = "#ccebc5") +
       
       tm_shape(period_diff_anom_points) +
       tm_dots(col = "end_hist_diff", palette = palette, size = 0.2, 
               breaks = all_diff_breaks[["brks"]]) +
       
       tm_layout(bg.color = "#a6cee3", 
                 main.title.position = "center",
                 main.title.size = 1.3,
                 main.title = paste("End_historic_diff", "_", sumstat),
                 legend.title.size = 1,
                 legend.text.size = .8)
     
     #end-mid diff
       tm_shape(CA_shp) + #basemap
       tm_fill(col = "#ccebc5") +
       
       tm_shape(period_diff_anom_points) +
       tm_dots(col = "end_mid_diff", palette = palette, size = 0.2, 
               breaks = all_diff_breaks[["brks"]]) +
       
       tm_layout(bg.color = "#a6cee3", 
                 main.title.position = "center",
                 main.title.size = 1.3,
                 main.title = paste("End_mid_diff", " ", sumstat),
                 legend.title.size = 1,
                 legend.text.size = .8) 
   }
  }

make_anom_map(temp_mpa_summary, "meanannual_days_abovethresh", "historic")
make_anom_map(temp_mpa_summary, "meanannual_days_abovethresh", "midcen")
make_anom_map(temp_mpa_summary, "meanannual_days_abovethresh", "endcen")

make_anom_map(temp_mpa_summary, "meanannual_days_abovethresh", "historic", diff = TRUE)


make_anom_map(temp_mpa_summary, "annual_days_abovethresh", "midcen")
make_anom_map(temp_mpa_summary, "annual_days_abovethresh", "endcen")




 
