#File created on 1/12/24
#Authored by Nikita Sridhar
#Pipeline for complete analysis of exposure of CA MPAs to future climate conditions

#Load required libraries--------------------------------------------------------
library(tidyverse)
library(here)
library(data.table)
library(factoextra)
library(broom)
library(cowplot)
library(respR)
library(lattice)
library(RcppRoll)
library(RColorBrewer)
library(gplots)
library(ggpmisc)

#1 - Load and merge model output files------------------------------------------
#merge model output (one csv per mpa) and create one csv with all mpas per model projection
#only run once! files are saved to processeddata/model folder
source(here::here("./scripts/00_mergecsv.R"))

#2 - Model-obs comparison-------------------------------------------------------
source(here::here("./scripts/01_model-obs.R"))

#3 - Future exposure analysis --------------------------------------------------
source(here::here("./reports/mpaexposure.qmd"))




