#Code for generating csvs used in qgis to make maps for anomalous event analysis
#Run after mpaexposure.qmd

anom_event_path <- "./reports/anom_event"

write.csv(pH_event, file.path(anom_event_path, "IPSL_pH_event.csv"))
write.csv(DO_event, file.path(anom_event_path, "IPSL_DO_event.csv"))
write.csv(temp_event, file.path(anom_event_path, "IPSL_temp_event.csv"))
