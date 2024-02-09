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
