sum_historic <- sum %>% filter(period == "historic")
sumsub_historic <- sum_historic %>% select(-OBJECTID,-NAME, -File, -SHORTNAME, -degx, -degy, -region, -period) 

sum_midcen <- sum %>% filter(period == "midcen")
sumsub_midcen <- sum_midcen %>% select(-OBJECTID,-NAME, -File, -SHORTNAME, -degx, -degy, -region, -period)

sum_endcen <- sum %>% filter(period == "endcen")
sumsub_endcen <- sum_endcen %>% select(-OBJECTID,-NAME, -File, -SHORTNAME, -degx, -degy, -region, -period) 


pca_historic <- prcomp(sumsub_historic, scale = TRUE)
summary(pca_historic)
fviz_pca_biplot(pca_historic, repel = TRUE,
                col.var = "black",
                col.ind = sum_historic$region,
                label ="var",
                labelsize = 3,
                addEllipses = TRUE,
                title = "IPSL Historic") 


make_pca <- function(df, periodt){
  
  sum_period <- sum %>% filter(period == periodt) 
  sumsub <- sum_period %>% select(-OBJECTID,-NAME, -File, -SHORTNAME, -degx, 
                                  -degy, -region, -period, -Date) 
  
  pca <- prcomp(sumsub, scale = TRUE)
  fviz_pca_biplot(pca, repel = TRUE,
                  col.var = "black",
                  col.ind = sum_period$region,
                  label ="var",
                  labelsize = 3,
                  addEllipses = TRUE,
                  title = periodt) 
  
  fviz_pca_ind(pca, label="none", habillage=sum_period$region,
               addEllipses=TRUE,  col.ind = sum_period$region)
  }


make_pca(sum, "historic" )

rm(sum_period)
rm(sumsub)
rm(pca)

################################################
#7/12/26
#Investigating PCA clustering

library(hopkins)
library(mclust)
  
#de-functioning, for individual plots per time period

#historic-----------------------------------------------------------------------
sum_historic <- sum %>% filter(period == "historic")
sumsub_historic <- sum_historic %>% select(-OBJECTID,-NAME, -File, -SHORTNAME, -degx, 
                                -degy, -region, -period)

historic_pca <- prcomp(sumsub_historic, scale = TRUE)
historic_results <- historic_pca$x[,1:3]

#clustering tendency - to cluster or not to cluster?
hopkins(historic_results, m = nrow(results)-1) #close to 1 so highly clustered
#sillhouette statistic - how many clusters to use?
fviz_nbclust(historic_results, FUNcluster=kmeans, k.max = 8) #suggests should use 2 clusters
#gap statistic - how many clusters to use?
fviz_nbclust(historic_results, FUNcluster=kmeans, method="gap_stat", k.max = 8)+ 
  theme_classic() #suggests should use 4 clusters
#distance metric - which metric to use, euclidean or manhattan?
historic_cluster<-eclust(historic_results, "kmeans", hc_metric="eucliden",k=4)
test<-eclust(historic_results, "kmeans", hc_metric="manhattan",k=4)

fviz_silhouette(historic_cluster) 
fviz_silhouette(test) 
#they both look and quantitatively are identical so doesn't matter which one (this is true for all
#time periods so just using euclidean going forward)

#visualizing clusters:
fviz_cluster(historic_cluster, 
             repel = TRUE,            
             show.clust.cent = TRUE, 
             col.var = "black",
             col.ind = sum_historic$region,
             ellipse.type = "convex", 
             ggtheme = theme_minimal()) 

#overlaying pc scores and clusters on same plot - used chat gpt here onwards
scores <- data.frame(
  PC1 = historic_pca$x[,1],
  PC2 = historic_pca$x[,2],
  cluster = factor(historic_cluster$cluster),
  region = sum_historic$region
)

#extractling hull shapes
hulls <- scores %>%
  group_by(cluster) %>%
  slice(chull(PC1, PC2))

#pca biplot
p <- fviz_pca_biplot(
  historic_pca,
  repel = TRUE,
  col.var = "black",
  col.ind = sum_historic$region,
  pointshape = 19,
  label = "var",
  labelsize = 3,
  addEllipses = FALSE
) 
p

#labeling cluster colors
cluster_cols <- c(
  "1" = "goldenrod2",
  "2" = "tomato",
  "3" = "purple",
  "4" = "dodgerblue3"
)

#overlaying pca biplot with the hulls from clusters
p +
  geom_polygon(
    data = hulls,
    aes(x = PC1, y = PC2, group = cluster, fill = cluster),
    alpha = 0.1,
    color = NA,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = cluster_cols) +
  labs(title = "Historic Period MPAs")



#midcen -----------------------------------------------------------

sum_midcen <- sum %>% filter(period == "midcen")
sumsub_midcen <- sum_midcen %>% select(-OBJECTID,-NAME, -File, -SHORTNAME, -degx, 
                                           -degy, -region, -period)

midcen_pca <- prcomp(sumsub_midcen, scale = TRUE)
midcen_results <- midcen_pca$x[,1:3]

#clustering tendency - to cluster or not to cluster?
hopkins(midcen_results, m = nrow(results)-1) #close to 1 so highly clustered
#sillhouette statistic - how many clusters to use?
fviz_nbclust(midcen_results, FUNcluster=kmeans, k.max = 8) #suggests should use 2 clusters
#gap statistic - how many clusters to use?
fviz_nbclust(midcen_results, FUNcluster=kmeans, method="gap_stat", k.max = 8)+ 
  theme_classic() #suggests should use 5 clusters

#making cluster
midcen_cluster <-eclust(midcen_results, "kmeans", hc_metric="eucliden",k=4)

#visualizing clusters:
fviz_silhouette(midcen_cluster) 

fviz_cluster(midcen_cluster, 
             repel = TRUE,            
             show.clust.cent = TRUE, 
             col.var = "black",
             col.ind = sum_endcen$region,
             ellipse.type = "convex", 
             ggtheme = theme_minimal()) 

#overlaying pc scores and clusters on same plot - used chat gpt here onwards
scores <- data.frame(
  PC1 = midcen_pca$x[,1],
  PC2 = midcen_pca$x[,2],
  cluster = factor(midcen_cluster$cluster),
  region = sum_midcen$region
)

#extractling hull shapes
hulls <- scores %>%
  group_by(cluster) %>%
  slice(chull(PC1, PC2))

#pca biplot
p <- fviz_pca_biplot(
  midcen_pca,
  repel = TRUE,
  col.var = "black",
  col.ind = sum_midcen$region,
  pointshape = 19,
  label = "var",
  labelsize = 3,
  addEllipses = FALSE
) 
p

#labeling cluster colors
cluster_cols <- c(
  "1" = "goldenrod2",
  "2" = "tomato",
  "3" = "purple",
  "4" = "dodgerblue3",
  "5" = "green"
)

#overlaying pca biplot with the hulls from clusters
p +
  geom_polygon(
    data = hulls,
    aes(x = PC1, y = PC2, group = cluster, fill = cluster),
    alpha = 0.1,
    color = NA,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = cluster_cols) +
  labs(title = "Midcen Period MPAs")


#endcen ------------------------------------------------------------------------

sum_endcen <- sum %>% filter(period == "endcen")
sumsub_endcen <- sum_endcen %>% select(-OBJECTID,-NAME, -File, -SHORTNAME, -degx, 
                                       -degy, -region, -period)

endcen_pca <- prcomp(sumsub_endcen[,-1], scale = TRUE)
endcen_results <- endcen_pca$x[,1:3]

#clustering tendency - to cluster or not to cluster?
hopkins(endcen_results, m = nrow(results)-1) #close to 1 so highly clustered
#sillhouette statistic - how many clusters to use?
fviz_nbclust(endcen_results, FUNcluster=kmeans, k.max = 8) #suggests should use 2 clusters
#gap statistic - how many clusters to use?
fviz_nbclust(endcen_results, FUNcluster=kmeans, method="gap_stat", k.max = 8)+ 
  theme_classic() #suggests should use 5 clusters

#making cluster 
endcen_cluster<-eclust(endcen_results, "kmeans", hc_metric="eucliden",k=4)

#visualizing clusters:
fviz_silhouette(endcen_cluster) 

fviz_cluster(endcen_cluster, 
             repel = TRUE,            
             show.clust.cent = TRUE, 
             col.var = "black",
             col.ind = sum_endcen$region,
             ellipse.type = "convex", 
             ggtheme = theme_minimal()) 

#overlaying pc scores and clusters on same plot - used chat gpt here onwards
scores <- data.frame(
  PC1 = endcen_pca$x[,1],
  PC2 = endcen_pca$x[,2],
  cluster = factor(endcen_cluster$cluster),
  region = sum_endcen$region
)

#extractling hull shapes
hulls <- scores %>%
  group_by(cluster) %>%
  slice(chull(PC1, PC2))

#pca biplot
p <- fviz_pca_biplot(
  endcen_pca,
  repel = TRUE,
  col.var = "black",
  col.ind = sum_endcen$region,
  pointshape = 19,
  label = "var",
  labelsize = 3,
  addEllipses = FALSE
) 
p

#labeling cluster colors
cluster_cols <- c(
  "1" = "goldenrod2",
  "2" = "tomato",
  "3" = "purple",
  "4" = "dodgerblue3",
)

#overlaying pca biplot with the hulls from clusters
p +
  geom_polygon(
    data = hulls,
    aes(x = PC1, y = PC2, group = cluster, fill = cluster),
    alpha = 0.1,
    color = NA,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = cluster_cols) +
  labs(title = "Endcen Period MPAs")



#comparing clusters across periods:---------------------------------------------
historic_cluster_asnmt <- historic_cluster$cluster
midcen_cluster_asnmt <- midcen_cluster$cluster
endcen_cluster_asnmt <- endcen_cluster$cluster


cluster_df <- data.frame(
  MPA = sum_historic$SHORTNAME,
  MPA_num = sum_historic$OBJECTID,
  region = sum_historic$region,
  historic = historic_cluster_asnmt,
  midcen = midcen_cluster_asnmt,
  endcen = endcen_cluster_asnmt
)

table(cluster_df$region, cluster_df$historic)
table(cluster_df$region, cluster_df$midcen)
table(cluster_df$region, cluster_df$endcen)

#using adjusted rand index to compare clusters (1 = identical, 0 = random)
#if two MPAs are in the same region, are they also likely to be assigned to same cluster?
adjustedRandIndex(cluster_df$region, cluster_df$historic)
adjustedRandIndex(cluster_df$region, cluster_df$midcen)
adjustedRandIndex(cluster_df$region, cluster_df$endcen)


#track changes in individual MPAs
cluster_df$changed_hist_mid <-
  cluster_df$historic != cluster_df$midcen

cluster_df$changed_mid_end <-
  cluster_df$midcen != cluster_df$endcen

cluster_df$changed_hist_end <-
  cluster_df$historic != cluster_df$endcen

cluster_df$n_changes <-
  (cluster_df$historic != cluster_df$midcen) +
  (cluster_df$midcen != cluster_df$endcen)

aggregate(n_changes ~ region, cluster_df, mean)


################################################################################
#according to chat (and Pete), run one PCA with all time periods. otherwise
#cluster 1 in historic doesn't neccessarily correspond to cluster 1 in midcen 
#as they could be different scales.
#below is from chat:


# Remove metadata
sumsub <- sum %>%
  select(-OBJECTID, -NAME, -File, -SHORTNAME,
         -degx, -degy, -region, -period)

# PCA on all observations
pca_all <- prcomp(sumsub, scale. = TRUE)

#varimax rotation
n_comp <- 2
raw_loadings <- pca_all$rotation[, 1:n_comp]
varimax_res  <- varimax(raw_loadings)
pca_all$rotation[, 1:n_comp] <- varimax_res$loadings
scaled_data <- scale(sumsub)
pca_all$x[, 1:n_comp] <- scaled_data %*% varimax_res$loadings

# Keep first 3 PCs
scores <- as.data.frame(pca_all$x[,1:3])

#deciding how many clusters to use (not too sure about this)
fviz_nbclust(scores[,1:3], FUNcluster=kmeans, k.max = 8) 

# Add metadata back
scores$MPA <- sum$NAME
scores$MPA_num <- sum$OBJECTID
scores$region <- sum$region
scores$period <- sum$period

#cluster: first set seed and run k means
set.seed(123)
km <- kmeans(scores[,1:2], centers = 4, nstart = 50)
scores$cluster <- factor(km$cluster)

cluster_table <- scores %>%
  select(MPA, MPA_num, region, period, cluster) %>%
  pivot_wider(names_from = period, values_from = cluster) %>%
  mutate(region = recode(region, "channel" = "Channel Islands","norca" = "Northern CA",
                         "socal" = "Southern CA", "centralca" = "Central CA"))

#count cluster changes
cluster_table$n_changes <-
  (cluster_table$historic != cluster_table$midcen) +
  (cluster_table$midcen != cluster_table$endcen)

#visualizing summary for cluster changes
general_cluster_table_summary <- cluster_table %>%
  group_by(region) %>%
  summarise(
    total_MPAs = n(),
    num_MPAs_changed = sum(n_changes > 0),
    mean_changes = mean(n_changes),
    .groups = "drop")

#making pretty table
ft_general_cluster_table <- flextable(general_cluster_table_summary) %>%
  set_header_labels(
    region = "Region", total_MPAs = "Total\nMPAs", num_MPAs_changed = "MPAs\nChanged",mean_changes = "Mean Cluster\nChanges") %>%
  colformat_double(j = "mean_changes", digits = 2) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(align = "center", j = c("total_MPAs", "num_MPAs_changed", "mean_changes"),
        part = "all") %>%
  align(align = "left",j = "region",
        part = "all") %>%
  autofit() %>%
  fontsize(size = 10, part = "all")
ft_general_cluster_table


mpa_cluster_table_summary <- scores %>%
  select(MPA, MPA_num, region, period, cluster) %>%
  pivot_wider(
    names_from = period,
    values_from = cluster,
    names_glue = "{period}_cluster"
  ) %>%
  # Reorder columns
  select(region, historic_cluster, midcen_cluster, endcen_cluster, MPA, MPA_num) %>%
  # Keep only MPAs that changed cluster
  filter(historic_cluster != midcen_cluster | historic_cluster != endcen_cluster 
         |midcen_cluster != endcen_cluster) %>%
  # Collapse MPAs with identical trajectories
  group_by(region,historic_cluster,midcen_cluster,endcen_cluster) %>%
  summarise(MPAs = paste(MPA, collapse = ", "),MPA_nums = paste(MPA_num, collapse = ", "),
    .groups = "drop")  %>%
  arrange(region,historic_cluster,midcen_cluster,endcen_cluster)

#making a pretty table
ft_mpa_cluster_table <- flextable(mpa_cluster_table_summary) %>%
  set_header_labels(
    region = "Region",
    historic_cluster = "Historic\nCluster",
    midcen_cluster = "Mid-century\nCluster",
    endcen_cluster = "End-century\nCluster",
    MPAs = "MPAs",
    MPA_nums = "MPA IDs"
  ) %>%
  theme_booktabs() %>%
  autofit() %>%
  align(
    j = c("historic_cluster", "midcen_cluster", "endcen_cluster"),
    align = "center",
    part = "all"
  ) %>%
  align(
    j = c("region", "MPAs", "MPA_nums"),
    align = "left",
    part = "all"
  ) %>%
  merge_v(j = "region") %>%
  valign(j = "region", valign = "top") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  width(j = "region", width = 1.2) %>%
  width(j = c("historic_cluster", "midcen_cluster", "endcen_cluster"), width = 0.8) %>%
  width(j = "MPAs", width = 3.8) %>%
  width(j = "MPA_nums", width = 1.5)

ft_mpa_cluster_table

#ARI someway to show if things are similar or different?
scores %>%
  group_by(period) %>%
  summarise(
    ARI = adjustedRandIndex(region, cluster)
  )


#plotting
hulls <- scores %>%
  group_by(cluster) %>%
  slice(chull(PC1, PC2))

scores$period <- factor(scores$period, 
                        levels = c("historic","midcen","endcen"))

#adding this to draw lines for only mpas that changed clusters
changed_mpas <- cluster_table %>%
  filter(n_changes > 0) %>%
  pull(MPA_num)

scores_segments <- scores %>%
  filter(MPA_num %in% changed_mpas) %>%
  select(MPA_num, region, period, PC1, PC2) %>%
  pivot_wider(
    names_from = period,
    values_from = c(PC1,PC2)
  )

#segments for plot
seg_hist_mid <- cluster_table %>%
  filter(historic != midcen) %>%
  select(MPA_num) %>%
  left_join(scores %>%
      select(MPA_num, period, PC1, PC2, region) %>%
      tidyr::pivot_wider(
        names_from = period,
        values_from = c(PC1, PC2)
      ),
    by = "MPA_num"
  )

seg_mid_end <- cluster_table %>%
  filter(midcen != endcen) %>%
  select(MPA_num) %>%
  left_join(scores %>%
              select(MPA_num, period, PC1, PC2, region) %>%
              tidyr::pivot_wider(
                names_from = period,
                values_from = c(PC1, PC2)
              ),
            by = "MPA_num"
  )

#pca biplot
p <- fviz_pca_biplot(
  pca_all,
  axes = c(1,2), #plots varimax rotated
  repel = TRUE,
  col.var = "black",
  col.ind = sum$region,
  pointshape = 19,
  label = "var",
  labelsize = 3,
  addEllipses = FALSE,
  invisible = "quali"
) 
p

#labeling cluster colors
cluster_cols <- c(
  "1" = "#8da0cb",
  "2" = "#66c2a5",
  "3" = "#fc8d62",
  "4" = "#e78ac3"
)
region_cols <- c(
  "centralca" = "#fc8d62",
  "channel" = "#66c2a5",
  "norca" = "#8da0cb",
  "socal" = "#e78ac3"
)

#overlaying pca biplot with the hulls from clusters
p +
  geom_polygon(
    data = hulls,
    aes(x = PC1, y = PC2, 
        group = cluster, fill = cluster),
    alpha = 0.3,
    color = NA,
    inherit.aes = FALSE
  ) +
  
  geom_segment(
    data = seg_hist_mid,
    aes(x = PC1_historic,
        y = PC2_historic,
        xend = PC1_midcen,
        yend = PC2_midcen,
        color = region),
    linewidth = 0.4,
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  
  geom_segment(
    data = seg_mid_end,
    aes(x = PC1_midcen,
        y = PC2_midcen,
        xend = PC1_endcen,
        yend = PC2_endcen,
        color = region),
    linewidth = 0.4,
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    data = scores, 
    aes(PC1, PC2,
        color = region,
        shape = period),
    size = 2,
    inherit.aes = FALSE
  ) + 

  scale_fill_manual(values = cluster_cols) +
  scale_color_manual(values = region_cols) +
    
  
  labs(title = "All Period MPAs")

