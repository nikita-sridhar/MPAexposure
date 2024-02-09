
make_regression <- function(df, sumstat, period1, period2){
 plot_matrix <- df %>%
   filter(period == period1 | period == period2) %>%
   select(sumstat, period, NAME, File) %>%
   pivot_wider(names_from = period, values_from = sumstat) 
 
 ggplot(data = plot_matrix, aes_string(x = period1, y = period2)) +
   stat_poly_line() +
   stat_poly_eq(use_label(c("eq", "R2"))) +
   geom_point() +
   theme_classic()
}

make_regression(sum, "Temp_mean", "historic", "midcen")



plot_matrix <- sum %>%
  filter(period == "historic" | period == "midcen") %>%
  select(Temp_mean, period, NAME, File) %>%
  pivot_wider(names_from = period, values_from = Temp_mean) 

ggplot(data = plot_matrix, aes(x = historic, y = midcen)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "midcen", x = "historic") 



          matrix <- sum %>%
            select(Temp_mean, NAME, degy, period) %>%
            pivot_wider(names_from = period, values_from = Temp_mean) %>%
            arrange(-degy) %>%
            select(-degy) 
