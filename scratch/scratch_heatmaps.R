
#create dataset w historic, midcen, and endcen. then make heatmap for just one variable
#with time period on x-axis. this is sumdiff.

#doing above stuff but just to get T mean across time periods to plto!!

#all code (except heatmaps for individual regions) reflected in mpaexposure.qmd.

col <- colorRampPalette(brewer.pal(9,"YlOrRd"))(256)
invert_col <- colorRampPalette(rev(brewer.pal(9,"YlOrRd")))(256)

# T Mean ------------------------------------------------------------------

#T MEAN SOCAL
socal_sum_diff_T_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "socal") %>%
select(NAME,historic_T_mean, midcen_T_mean, endcen_T_mean) 

rownames(socal_sum_diff_T_mean) <- socal_sum_diff_T_mean[,1]

socal_Tmean <- as.matrix(socal_sum_diff_T_mean[,2:4])

heatmap.2(socal_Tmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T mean Southern CA",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#T MEAN NORCAL
norca_sum_diff_T_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "norca") %>%
  select(NAME,historic_T_mean, midcen_T_mean, endcen_T_mean) 

rownames(norca_sum_diff_T_mean) <- norca_sum_diff_T_mean[,1]

norca_Tmean <- as.matrix(norca_sum_diff_T_mean[,2:4])

heatmap.2(norca_Tmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T mean Northern CA",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#T MEAN CENTRAL CA
centralca_sum_diff_T_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "centralca") %>%
  select(NAME,historic_T_mean, midcen_T_mean, endcen_T_mean) 

rownames(centralca_sum_diff_T_mean) <- centralca_sum_diff_T_mean[,1]

centralca_Tmean <- as.matrix(centralca_sum_diff_T_mean[,2:4])

heatmap.2(centralca_Tmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T mean Central CA",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#T MEAN CHANNEL ISL
channel_sum_diff_T_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "channel") %>%
  select(NAME,historic_T_mean, midcen_T_mean, endcen_T_mean) 

rownames(channel_sum_diff_T_mean) <- channel_sum_diff_T_mean[,1]

channel_Tmean <- as.matrix(channel_sum_diff_T_mean[,2:4])

heatmap.2(channel_Tmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T mean Channel Isl",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#T mean all regions
sum_diff_T_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_T_mean, midcen_T_mean, endcen_T_mean) 

rownames(sum_diff_T_mean) <- sum_diff_T_mean[,1]

Tmean <- as.matrix(sum_diff_T_mean[,2:4])

heatmap.2(Tmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T mean",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#T low 10 all regions
sum_diff_T_low10 <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_T_low10, midcen_T_low10, endcen_T_low10) 

rownames(sum_diff_T_low10) <- sum_diff_T_low10[,1]

Tlow10 <- as.matrix(sum_diff_T_low10[,2:4])

heatmap.2(Tlow10,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T low10",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#T seasonalSD all regions
sum_diff_T_seasonalSD <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_T_seasonalSD, midcen_T_seasonalSD, endcen_T_seasonalSD) 

rownames(sum_diff_T_seasonalSD) <- sum_diff_T_seasonalSD[,1]

TseasonalSD <- as.matrix(sum_diff_T_seasonalSD[,2:4])

heatmap.2(TseasonalSD,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T seasonalSD",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#T eventSD all regions
sum_diff_T_eventSD <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_T_eventSD, midcen_T_eventSD, endcen_T_eventSD) 

rownames(sum_diff_T_eventSD) <- sum_diff_T_eventSD[,1]

TeventSD <- as.matrix(sum_diff_T_eventSD[,2:4])

heatmap.2(TeventSD,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "T eventSD",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

# DO mean -----------------------------------------------------------------

#DO MEAN SOCAL
socal_sum_diff_DO_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "socal") %>%
  select(NAME,historic_DO_mean, midcen_DO_mean, endcen_DO_mean) 

rownames(socal_sum_diff_DO_mean) <- socal_sum_diff_DO_mean[,1]
socal_DOmean <- as.matrix(socal_sum_diff_DO_mean[,2:4])

heatmap.2(socal_DOmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO mean SoCal",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#DO MEAN NORCAL
norca_sum_diff_DO_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "norca") %>%
  select(NAME,historic_DO_mean, midcen_DO_mean, endcen_DO_mean) 

rownames(norca_sum_diff_DO_mean) <- norca_sum_diff_DO_mean[,1]
norca_DOmean <- as.matrix(norca_sum_diff_DO_mean[,2:4])

heatmap.2(norca_DOmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO mean norca",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#DO MEAN CENTRAL CA
centralca_sum_diff_DO_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "centralca") %>%
  select(NAME,historic_DO_mean, midcen_DO_mean, endcen_DO_mean) 

rownames(centralca_sum_diff_DO_mean) <- centralca_sum_diff_DO_mean[,1]
centralca_DOmean <- as.matrix(centralca_sum_diff_DO_mean[,2:4])

heatmap.2(centralca_DOmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO mean centralca",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#DO MEAN CHANNEL ISL
channel_sum_diff_DO_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "channel") %>%
  select(NAME,historic_DO_mean, midcen_DO_mean, endcen_DO_mean) 

rownames(channel_sum_diff_DO_mean) <- channel_sum_diff_DO_mean[,1]
channel_DOmean <- as.matrix(channel_sum_diff_DO_mean[,2:4])

heatmap.2(channel_DOmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO mean channel",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#DO mean all regions
sum_diff_DO_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_DO_mean, midcen_DO_mean, endcen_DO_mean) 

rownames(sum_diff_DO_mean) <- sum_diff_DO_mean[,1]

DOmean <- as.matrix(sum_diff_DO_mean[,2:4])

heatmap.2(DOmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO mean",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=invert_col, srtCol = 45)

#DO low 10 all regions
sum_diff_DO_low10 <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_DO_low10, midcen_DO_low10, endcen_DO_low10) 

rownames(sum_diff_DO_low10) <- sum_diff_DO_low10[,1]

DOlow10 <- as.matrix(sum_diff_DO_low10[,2:4])

heatmap.2(DOlow10,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO low10",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=invert_col, srtCol = 45)

#DO seasonalSD all regions
sum_diff_DO_seasonalSD <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_DO_seasonalSD, midcen_DO_seasonalSD, endcen_DO_seasonalSD) 

rownames(sum_diff_DO_seasonalSD) <- sum_diff_DO_seasonalSD[,1]

DOseasonalSD <- as.matrix(sum_diff_DO_seasonalSD[,2:4])

heatmap.2(DOseasonalSD,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO seasonalSD",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#DO eventSD all regions
sum_diff_DO_eventSD <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_DO_eventSD, midcen_DO_eventSD, endcen_DO_eventSD) 

rownames(sum_diff_DO_eventSD) <- sum_diff_DO_eventSD[,1]

DOeventSD <- as.matrix(sum_diff_DO_eventSD[,2:4])

heatmap.2(DOeventSD,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "DO eventSD",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

# pH mean -----------------------------------------------------------------

#pH MEAN SOCAL
socal_sum_diff_pH_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "socal") %>%
  select(NAME,historic_pH_mean, midcen_pH_mean, endcen_pH_mean) 

rownames(socal_sum_diff_pH_mean) <- socal_sum_diff_pH_mean[,1]
socal_pHmean <- as.matrix(socal_sum_diff_pH_mean[,2:4])

heatmap.2(socal_pHmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH mean SoCal",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#pH MEAN NORCAL
norca_sum_diff_pH_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "norca") %>%
  select(NAME,historic_pH_mean, midcen_pH_mean, endcen_pH_mean) 

rownames(norca_sum_diff_pH_mean) <- norca_sum_diff_pH_mean[,1]
norca_pHmean <- as.matrix(norca_sum_diff_pH_mean[,2:4])

heatmap.2(norca_pHmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH mean norca",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#pH MEAN CENTRAL CA
centralca_sum_diff_pH_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "centralca") %>%
  select(NAME,historic_pH_mean, midcen_pH_mean, endcen_pH_mean) 

rownames(centralca_sum_diff_pH_mean) <- centralca_sum_diff_pH_mean[,1]
centralca_pHmean <- as.matrix(centralca_sum_diff_pH_mean[,2:4])

heatmap.2(centralca_pHmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH mean centralca",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#pH MEAN CHANNEL ISL
channel_sum_diff_pH_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  filter( region.y  == "channel") %>%
  select(NAME,historic_pH_mean, midcen_pH_mean, endcen_pH_mean) 

rownames(channel_sum_diff_pH_mean) <- channel_sum_diff_pH_mean[,1]
channel_pHmean <- as.matrix(channel_sum_diff_pH_mean[,2:4])

heatmap.2(channel_pHmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH mean channel",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#pH mean all regions
sum_diff_pH_mean <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_pH_mean, midcen_pH_mean, endcen_pH_mean) 

rownames(sum_diff_pH_mean) <- sum_diff_pH_mean[,1]

pHmean <- as.matrix(sum_diff_pH_mean[,2:4])

heatmap.2(pHmean,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH mean",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=invert_col, srtCol = 45)

#pH low 10 all regions
sum_diff_pH_low10 <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_pH_low10, midcen_pH_low10, endcen_pH_low10) 

rownames(sum_diff_pH_low10) <- sum_diff_pH_low10[,1]

pHlow10 <- as.matrix(sum_diff_pH_low10[,2:4])

heatmap.2(pHlow10,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH low10",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=invert_col, srtCol = 45)

#pH seasonalSD all regions
sum_diff_pH_seasonalSD <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_pH_seasonalSD, midcen_pH_seasonalSD, endcen_pH_seasonalSD) 

rownames(sum_diff_pH_seasonalSD) <- sum_diff_pH_seasonalSD[,1]

pHseasonalSD <- as.matrix(sum_diff_pH_seasonalSD[,2:4])

heatmap.2(pHseasonalSD,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH seasonalSD",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#pH eventSD all regions
sum_diff_pH_eventSD <- sum_diff[order(sum_diff$degy.x),] %>%
  select(NAME,historic_pH_eventSD, midcen_pH_eventSD, endcen_pH_eventSD) 

rownames(sum_diff_pH_eventSD) <- sum_diff_pH_eventSD[,1]

pHeventSD <- as.matrix(sum_diff_pH_eventSD[,2:4])

heatmap.2(pHeventSD,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = "pH eventSD",tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45)

#regressions


# T -----------------------------------------------------------------------

#historic T_mean vs. midcen T_mean
ggplot(data = sum_diff, aes(x = historic_T_mean, y = midcen_T_mean)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen T_mean", x = "Historic T_mean") +
  ggtitle("Historic vs. midcen T_mean")

#historic T_mean vs. endcen T_mean
ggplot(data = sum_diff, aes(x = historic_T_mean, y = endcen_T_mean)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen T_mean", x = "Historic T_mean") +
  ggtitle("Historic vs. endcen T_mean")

#historic T_seasonalSD vs. midcen T_seasonalSD
ggplot(data = sum_diff, aes(x = historic_T_seasonalSD, y = midcen_T_seasonalSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen T_seasonalSD", x = "Historic T_seasonalSD") +
  ggtitle("Historic vs. midcen T_seasonalSD")

#historic T_seasonalSD vs. endcen T_seasonalSD
ggplot(data = sum_diff, aes(x = historic_T_seasonalSD, y = endcen_T_seasonalSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen T_seasonalSD", x = "Historic T_seasonalSD") +
  ggtitle("Historic vs. endcen T_seasonalSD")

#historic T_eventSD vs. midcen T_eventSD
ggplot(data = sum_diff, aes(x = historic_T_eventSD, y = midcen_T_eventSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen T_eventSD", x = "Historic T_eventSD") +
  ggtitle("Historic vs. midcen T_eventSD")

#historic T_eventSD vs. endcen T_eventSD
ggplot(data = sum_diff, aes(x = historic_T_eventSD, y = endcen_T_eventSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen T_eventSD", x = "Historic T_eventSD") +
  ggtitle("Historic vs. endcen T_eventSD")


# pH ----------------------------------------------------------------------

#historic pH_mean vs. midcen pH_mean
ggplot(data = sum_diff, aes(x = historic_pH_mean, y = midcen_pH_mean)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen pH_mean", x = "Historic pH_mean") +
  ggtitle("Historic vs. midcen pH_mean")

#historic pH_mean vs. endcen pH_mean
ggplot(data = sum_diff, aes(x = historic_pH_mean, y = endcen_pH_mean)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen pH_mean", x = "Historic pH_mean") +
  ggtitle("Historic vs. endcen pH_mean")

#historic pH_seasonalSD vs. midcen pH_seasonalSD
ggplot(data = sum_diff, aes(x = historic_pH_seasonalSD, y = midcen_pH_seasonalSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen pH_seasonalSD", x = "Historic pH_seasonalSD") +
  ggtitle("Historic vs. midcen pH_seasonalSD")

#historic pH_seasonalSD vs. endcen pH_seasonalSD
ggplot(data = sum_diff, aes(x = historic_pH_seasonalSD, y = endcen_pH_seasonalSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen pH_seasonalSD", x = "Historic pH_seasonalSD") +
  ggtitle("Historic vs. endcen pH_seasonalSD")

#historic pH_eventSD vs. midcen pH_eventSD
ggplot(data = sum_diff, aes(x = historic_pH_eventSD, y = midcen_pH_eventSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen pH_eventSD", x = "Historic pH_eventSD") +
  ggtitle("Historic vs. midcen pH_eventSD")

#historic pH_eventSD vs. endcen pH_eventSD
ggplot(data = sum_diff, aes(x = historic_pH_eventSD, y = endcen_pH_eventSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen pH_eventSD", x = "Historic pH_eventSD") +
  ggtitle("Historic vs. endcen pH_eventSD")

# DO ----------------------------------------------------------------------

#historic DO_mean vs. midcen DO_mean
ggplot(data = sum_diff, aes(x = historic_DO_mean, y = midcen_DO_mean)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen DO_mean", x = "Historic DO_mean") +
  ggtitle("Historic vs. midcen DO_mean")

#historic DO_mean vs. endcen DO_mean
ggplot(data = sum_diff, aes(x = historic_DO_mean, y = endcen_DO_mean)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen DO_mean", x = "Historic DO_mean") +
  ggtitle("Historic vs. endcen DO_mean")

#historic DO_seasonalSD vs. midcen DO_seasonalSD
ggplot(data = sum_diff, aes(x = historic_DO_seasonalSD, y = midcen_DO_seasonalSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen DO_seasonalSD", x = "Historic DO_seasonalSD") +
  ggtitle("Historic vs. midcen DO_seasonalSD")

#historic DO_seasonalSD vs. endcen DO_seasonalSD
ggplot(data = sum_diff, aes(x = historic_DO_seasonalSD, y = endcen_DO_seasonalSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen DO_seasonalSD", x = "Historic DO_seasonalSD") +
  ggtitle("Historic vs. endcen DO_seasonalSD")

#historic DO_eventSD vs. midcen DO_eventSD
ggplot(data = sum_diff, aes(x = historic_DO_eventSD, y = midcen_DO_eventSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Midcen DO_eventSD", x = "Historic DO_eventSD") +
  ggtitle("Historic vs. midcen DO_eventSD")

#historic DO_eventSD vs. endcen DO_eventSD
ggplot(data = sum_diff, aes(x = historic_DO_eventSD, y = endcen_DO_eventSD)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
  labs(y= "Endcen DO_eventSD", x = "Historic DO_eventSD") +
  ggtitle("Historic vs. endcen DO_eventSD")

###############################################

#scratch for make_heatmap function

make_heatmap <- function(df, variable){
  
  #make matrix for variable of interest
  matrix <- sum %>%
    select(variable, NAME, degy, period) %>%
    pivot_wider(names_from = period, values_from = variable) %>%
    arrange(-degy) %>%
    select(-degy)
  
  rownames(matrix) <- matrix[,1]
  
  matrix_numeric <- matrix %>%
    as.matrix(matrix[,2:4])
  
  
  heatmap.2(matrix_numeric,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
            main = variable,tracecol=NA, revC= TRUE,
            margins = c(5,10), col=col, srtCol = 45, labRow = matrix$NAME)
  
  
  print(matrix)
  print(matrix_numeric)
  
}

make_heatmap(sum, "Temp_mean")



matrix <- sum %>%
  select(Temp_mean, NAME, degy, period) %>%
  pivot_wider(names_from = period, values_from = Temp_mean) %>%
  arrange(-degy) %>%
  select(-degy) %>%
  select(historic, midcen, endcen)

matrix_numeric <- matrix %>%
  select(-NAME) %>%
  as.matrix()

row.names(matrix_numeric) <- matrix$NAME

heatmap.2(matrix_numeric,Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          main = variable,tracecol=NA, revC= TRUE,
          margins = c(5,10), col=col, srtCol = 45, labRow = matrix$NAME)


region_cols <- colour_vales(matrix$region, c("darkred", "forestgreen", "orange", "blue"))



  
  

    #make matrix for variable of interest
    matrix <- sum %>%
      select(Temp_mean, NAME, degy, period, region) %>%
      pivot_wider(names_from = period, values_from = Temp_mean) %>%
      mutate(col = case_when((region == "centralca") ~ "#ffff99", 
                             (region == "norca") ~ "#beaed4",
                             (region == "socal") ~ "#fdc086",
                             (region == "channel") ~ "#7fc97f")) %>%
      arrange(-degy) 
    
    #making matrix numeric for heatmap to work
    matrix_numeric <- matrix %>%
      select(-NAME, -region, -degy) %>%
      select(historic, midcen, endcen) %>%
      as.matrix()
    
    row.names(matrix_numeric) <- matrix$NAME
    
    heatmap.2(matrix_numeric, Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
              main = sumstat, tracecol=NA, revC= TRUE,
              margins = c(5,15), col= col, srtCol = 45, labRow = matrix$NAME, 
              RowSideColors = matrix$col)
  



