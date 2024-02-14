#removed ggsave so i can visualize in quarto.

ggsave(here(paste("./figs/pca/pca",periodt,".png")), plot)

ggsave(here(paste("./figs/regression/regression",sumstat,period2,".png")), plot)


tmap_save(map, here(paste("./figs/anom_map/",filename)))

ggsave(here(paste("./figs/sevintdur/","temp_events", periodt, site,".png",sep="_")), plot)

