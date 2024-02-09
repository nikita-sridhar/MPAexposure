#File created 1/12/24
#Authored by Nikita Sridhar
#Code to merge model output .csv file. Output lives in processeddata/model


#model output used for model-obs comparison ------------------------------------

#Raw data obtained from Jerome Feichter
#30m depth avg
#sites include selected coordinates based on good quality obs dataset locations

mod_all30m_files <-  list.files(path = here("data/rawdata/model/model_for_obs_comparison_30m"), 
                                pattern = '.csv', 
                                full.names = TRUE)

mod_all30m <- map_df(mod_all30m_files, ~read.csv(.x) %>% mutate(File = basename(.x)))
write.csv(mod_all30m, here("data/processeddata/model/mod_all30m.csv"))
  

#future projections model output ------------- ---------------------------------

#Raw data obtained from Jerome Feichter
#30m depth avg
#sites include every mpa in CA (124)

#create IPSL csv
IPSLfiles <- list.files(path = here("data/rawdata/model/IPSL"), pattern = '.csv', full.names = TRUE)
IPSLmpa <- map_df(IPSLfiles, ~read.csv(.x) %>% mutate(File = basename(.x)))
write.csv(IPSLmpa, here("data/processeddata/model/IPSLmpa.csv"))

#create GFDL csv
GFDLfiles <- list.files(path = here("data/rawdata/model/GFDL"), pattern = '.csv', full.names = TRUE)
GFDLmpa <- map_df(GFDLfiles, ~read.csv(.x) %>% mutate(File = basename(.x)))
write.csv(GFDLmpa, here("data/processeddata/model/GFDLmpa.csv"))

#create HADLEY csv
HADLEYfiles <- list.files(path = here("data/rawdata/model/HADLEY"), pattern = '.csv', full.names = TRUE)
HADLEYmpa <- map_df(HADLEYfiles, ~read.csv(.x) %>% mutate(File = basename(.x)))
write.csv(HADLEYmpa, here("data/processeddata/model/HADLEYmpa.csv"))


