library(tidyverse)
library(lubridate)
library(raster)
library(tidyr)
library(openair)
library(plotrix)
library(magrittr)

#load clean_all_distance_offshore_seacarb RData and mod_all RData prior to starting
#NEW: instead of clean_all line, load light dataset

setwd("~/Documents/Packard_MPA_Project/Data")
clean_all <- load("clean_all_distance_offshore_seacarb.RData")
mod_all <- load("mod_all.RData") #manually do this if it doesn't work
rm(clean_all)

clean_all <- light
rm(light)


#############
#model setup#
#############

#creating avg of surface and bottom values from model data
mod_all <- mutate(mod_all, T_mid = (T_surf + T_bot)/2, pH_mid = (pH_surf + pH_bot)/2, DO_mid = (DO_surf + DO_bot)/2)

###########
#obs setup#
###########

#filtering all datasets for specific chosen ones and time
clean_all <- clean_all %>% mutate(
  HabitatFlag = case_when(
    dataset_id %in% c(1,2,3,5,6,7,8,9,11,12,13,14,15,16,19,20,21,22,23,24,25,26,27,30,31,32,33,34,35,36,40,41,42,43,44,45,47,50,52,55,56,57,58,59,60,61) ~ "Ocean",
    dataset_id %in% c(10) ~ "Estuary - Purpose specific",
    dataset_id %in% c(17,18,38,46,53,54) ~ "Estuary",
    dataset_id %in% c(28,37) ~ "Surfzone",
    dataset_id %% 39 & latitude > 35.7 ~ "Estuary",
    dataset_id %% 39 & latitude < 35.7 ~ "Ocean",
    dataset_id %% 49 & longitude < -122.58 ~ "Ocean",
    dataset_id %% 49 & longitude > -122.58 ~ "Estuary")
)

clean17<- filter(clean_all, dataset_id == 17)

clean_all <- clean_all %>%
  filter(HabitatFlag == "Ocean") 

unique(clean_all$dataset_id)

final_datasets <- c(3,30, 31, 44, 17, 37, 39, 28, 6, 7, 14, 33, 18, 19, 38, 45)

env <- clean_all %>%
  filter(dataset_id %in% final_datasets)

#group by and summarize by date (daily avgs)
daily_data <- env %>%
  mutate(date = lubridate::date(time_utc), month = lubridate::month(time_utc), year = lubridate::year(time_utc)) %>%
  group_by(dataset_id, date, .add = TRUE) %>%
  dplyr::summarize(temp_daily_avg = mean(t_C, na.rm = TRUE), 
            pH_daily_avg = mean(pH_total, na.rm = TRUE), 
            do_daily_avg = mean(do_umolkg, na.rm = TRUE)) %>%
  mutate(date = lubridate::date(date), month = lubridate::month(date), year = lubridate::year(date))

#turn months to factors for boxplot to work
daily_data$month = factor(daily_data$month, ordered = TRUE)
mod_all$Month = factor(mod_all$Month, ordered = TRUE)

#monthly avgs
obs_month <- daily_data %>%
  group_by(dataset_id, month) %>%
  summarize(temp = mean(temp_daily_avg, na.rm=TRUE), ph = mean(pH_daily_avg, na.rm=TRUE), do= mean(do_daily_avg, na.rm=TRUE)) 
  
mod_month <- mod_all %>%
  group_by(dataset, Month) %>%
  summarize(temp_surf = mean(T_surf, na.rm=TRUE), temp_mid = mean(T_mid, na.rm=TRUE), temp_bot = mean(T_bot, na.rm=TRUE),
            ph_surf = mean(pH_surf, na.rm=TRUE), ph_mid = mean(pH_mid, na.rm=TRUE), ph_bot = mean(pH_bot, na.rm=TRUE),
            do_surf = mean(DO_surf, na.rm=TRUE), do_mid = mean(DO_mid, na.rm=TRUE), do_bot = mean(DO_bot, na.rm=TRUE)) %>%
  rename(month = Month, dataset_id = dataset)

month_join <- full_join(mod_month,obs_month, by = c("dataset_id", "month"))

############
#taylor diag
############

#model bias
mod_bias <- month_join %>%
  group_by(dataset_id) %>%
  summarize(surf_temp_bias = 
              (mean(temp_surf, na.rm = TRUE) - mean(temp, na.rm = TRUE)),
            mid_temp_bias =
              (mean(temp_mid, na.rm = TRUE) - mean(temp, na.rm = TRUE)),
            bot_temp_bias = 
              (mean(temp_bot, na.rm = TRUE) - mean(temp, na.rm = TRUE)),
            surf_ph_bias =
              (mean(ph_surf, na.rm = TRUE) - mean(ph, na.rm = TRUE)),
            mid_ph_bias =
              (mean(ph_mid, na.rm = TRUE) - mean(ph, na.rm = TRUE)),
            bot_ph_bias =
              (mean(ph_bot, na.rm = TRUE) - mean(ph, na.rm = TRUE)),
            surf_do_bias = 
              (mean(do_surf, na.rm = TRUE) - mean(do, na.rm = TRUE)),
            mid_do_bias = 
              (mean(do_mid, na.rm = TRUE) - mean(do, na.rm = TRUE)),
            bot_do_bias = 
              (mean(do_bot, na.rm = TRUE) - mean(do, na.rm = TRUE)))

write.csv(mod_bias, "mod.bias.csv")

#trying to make taylor w plottrix and overlaying points to get all datasets on same plot
#temp surf

#taylor diagram function copy pasted in order to add text parameter
taylor.diagram.modified <- function (ref, model, add = FALSE, col = "red", pch = 19, pos.cor = TRUE, 
          xlab = "Standard deviation", ylab = "", main = "Taylor Diagram", 
          show.gamma = TRUE, ngamma = 3, gamma.col = 8, sd.arcs = 0, 
          ref.sd = FALSE, sd.method = "sample", grad.corr.lines = c(0.2, 
                                                                    0.4, 0.6, 0.8, 0.9), pcex = 1, cex.axis = 1, normalize = FALSE, 
          mar = c(4, 3, 4, 3),text, ...) 
{
  grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 
                      1)
  R <- cor(ref, model, use = "pairwise")
  if (is.list(ref)) 
    ref <- unlist(ref)
  if (is.list(model)) 
    ref <- unlist(model)
  SD <- function(x, subn) {
    meanx <- mean(x, na.rm = TRUE)
    devx <- x - meanx
    ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) - 
                                                  subn))
    return(ssd)
  }
  subn <- sd.method != "sample"
  sd.r <- SD(ref, subn)
  sd.f <- SD(model, subn)
  if (normalize) {
    sd.f <- sd.f/sd.r
    sd.r <- 1
  }
  maxsd <- 1.5 * max(sd.f, sd.r)
  oldpar <- par("mar", "xpd", "xaxs", "yaxs")
  if (!add) {
    par(mar = mar)
    if (pos.cor) {
      if (nchar(ylab) == 0) 
        ylab = "Standard deviation"
      plot(0, xlim = c(0, maxsd * 1.1), ylim = c(0, maxsd * 
                                                   1.1), xaxs = "i", yaxs = "i", axes = FALSE, main = main, 
           xlab = "", ylab = ylab, type = "n", cex = cex.axis, 
           ...)
      mtext(xlab, side = 1, line = 2.3)
      if (grad.corr.lines[1]) {
        for (gcl in grad.corr.lines) lines(c(0, maxsd * 
                                               gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3)
      }
      segments(c(0, 0), c(0, 0), c(0, maxsd), c(maxsd, 
                                                0))
      axis.ticks <- pretty(c(0, maxsd))
      axis.ticks <- axis.ticks[axis.ticks <= maxsd]
      axis(1, at = axis.ticks, cex.axis = cex.axis)
      axis(2, at = axis.ticks, cex.axis = cex.axis)
      if (sd.arcs[1]) {
        if (length(sd.arcs) == 1) 
          sd.arcs <- axis.ticks
        for (sdarc in sd.arcs) {
          xcurve <- cos(seq(0, pi/2, by = 0.03)) * sdarc
          ycurve <- sin(seq(0, pi/2, by = 0.03)) * sdarc
          lines(xcurve, ycurve, col = "blue", lty = 3)
        }
      }
      if (show.gamma[1]) {
        if (length(show.gamma) > 1) 
          gamma <- show.gamma
        else gamma <- pretty(c(0, maxsd), n = ngamma)[-1]
        if (gamma[length(gamma)] > maxsd) 
          gamma <- gamma[-length(gamma)]
        labelpos <- seq(45, 70, length.out = length(gamma))
        for (gindex in 1:length(gamma)) {
          xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] + 
            sd.r
          endcurve <- which(xcurve < 0)
          endcurve <- ifelse(length(endcurve), min(endcurve) - 
                               1, 105)
          ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
          maxcurve <- xcurve * xcurve + ycurve * ycurve
          startcurve <- which(maxcurve > maxsd * maxsd)
          startcurve <- ifelse(length(startcurve), max(startcurve) + 
                                 1, 0)
          lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], 
                col = gamma.col)
          if (xcurve[labelpos[gindex]] > 0) 
            boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], 
                         gamma[gindex], border = FALSE)
        }
      }
      xcurve <- cos(seq(0, pi/2, by = 0.01)) * maxsd
      ycurve <- sin(seq(0, pi/2, by = 0.01)) * maxsd
      lines(xcurve, ycurve)
      bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
      medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
      smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
      segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * 
                 maxsd, cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 
                 0.97 * maxsd)
      par(xpd = TRUE)
      if (ref.sd) {
        xcurve <- cos(seq(0, pi/2, by = 0.01)) * sd.r
        ycurve <- sin(seq(0, pi/2, by = 0.01)) * sd.r
        lines(xcurve, ycurve)
      }
      points(sd.r, 0, cex = pcex)
      text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 
             1.05 * maxsd, sin(c(bigtickangles, acos(c(0.95, 
                                                       0.99)))) * 1.05 * maxsd, c(seq(0.1, 0.9, by = 0.1), 
                                                                                  0.95, 0.99), cex = cex.axis)
      text(maxsd * 0.8, maxsd * 0.8, "Correlation", srt = 315, 
           cex = cex.axis)
      segments(cos(medtickangles) * maxsd, sin(medtickangles) * 
                 maxsd, cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 
                 0.98 * maxsd)
      segments(cos(smltickangles) * maxsd, sin(smltickangles) * 
                 maxsd, cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 
                 0.99 * maxsd)
    }
    else {
      x <- ref
      y <- model
      R <- cor(x, y, use = "pairwise.complete.obs")
      E <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
      xprime <- x - mean(x, na.rm = TRUE)
      yprime <- y - mean(y, na.rm = TRUE)
      sumofsquares <- (xprime - yprime)^2
      Eprime <- sqrt(sum(sumofsquares)/length(complete.cases(x)))
      E2 <- E^2 + Eprime^2
      if (add == FALSE) {
        maxray <- 1.5 * max(sd.f, sd.r)
        plot(c(-maxray, maxray), c(0, maxray), type = "n", 
             asp = 1, bty = "n", xaxt = "n", yaxt = "n", 
             xlim = c(-1.1 * maxray, 1.1 * maxray), xlab = xlab, 
             ylab = ylab, main = main, cex = cex.axis)
        discrete <- seq(180, 0, by = -1)
        listepoints <- NULL
        for (i in discrete) {
          listepoints <- cbind(listepoints, maxray * 
                                 cos(i * pi/180), maxray * sin(i * pi/180))
        }
        listepoints <- matrix(listepoints, 2, length(listepoints)/2)
        listepoints <- t(listepoints)
        lines(listepoints[, 1], listepoints[, 2])
        lines(c(-maxray, maxray), c(0, 0))
        lines(c(0, 0), c(0, maxray))
        for (i in grad.corr.lines) {
          lines(c(0, maxray * i), c(0, maxray * sqrt(1 - 
                                                       i^2)), lty = 3)
          lines(c(0, -maxray * i), c(0, maxray * sqrt(1 - 
                                                        i^2)), lty = 3)
        }
        for (i in grad.corr.full) {
          text(1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                         i^2), i, cex = cex.axis, adj = cos(i)/2)
          text(-1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                          i^2), -i, cex = cex.axis, adj = 1 - cos(i)/2)
        }
        seq.sd <- seq.int(0, 2 * maxray, by = (maxray/10))[-1]
        for (i in seq.sd) {
          xcircle <- sd.r + (cos(discrete * pi/180) * 
                               i)
          ycircle <- sin(discrete * pi/180) * i
          for (j in 1:length(xcircle)) {
            if ((xcircle[j]^2 + ycircle[j]^2) < (maxray^2)) {
              points(xcircle[j], ycircle[j], col = "darkgreen", 
                     pch = ".")
              if (j == 10) 
                text(xcircle[j], ycircle[j], signif(i, 
                                                    2), cex = cex.axis, col = "darkgreen", 
                     srt = 90)
            }
          }
        }
        seq.sd <- seq.int(0, maxray, length.out = 5)
        for (i in seq.sd) {
          xcircle <- cos(discrete * pi/180) * i
          ycircle <- sin(discrete * pi/180) * i
          if (i) 
            lines(xcircle, ycircle, lty = 3, col = "blue")
          text(min(xcircle), -0.06 * maxray, signif(i, 
                                                    2), cex = cex.axis, col = "blue")
          text(max(xcircle), -0.06 * maxray, signif(i, 
                                                    2), cex = cex.axis, col = "blue")
        }
        text(0, -0.14 * maxray, "Standard Deviation", 
             cex = cex.axis, col = "blue")
        text(0, -0.22 * maxray, "Centered RMS Difference", 
             cex = cex.axis, col = "darkgreen")
        points(sd.r, 0, pch = 22, bg = "darkgreen", cex = pcex)
        text(0, 1.2 * maxray, "Correlation Coefficient", 
             cex = cex.axis)
      }
      S <- (2 * (1 + R))/(sd.f + (1/sd.f))^2
    }
  }
  points(sd.f * R, sd.f * sin(acos(R)), pch = pch, col = col, 
         cex = pcex)
  text(sd.f * R, sd.f * sin(acos(R)),  
       labels=text, cex = pcex, pos=3)
  invisible(oldpar)
}

#pch values: 16 = socal, 17 = channel, 18 = norcal, 15 = central
#surf temp taylor diagram
month_join %>%
  filter(dataset_id == 3) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#A3134B", main = "Surface Temp Taylor Diagram", text = "3", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 6) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#28189A", add =TRUE, text = "6", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 7) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#751568", add =TRUE, text = "7", pch = 17,normalize = TRUE)
month_join %>%
  filter(dataset_id == 14) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#D1112D", add =TRUE, text = "14", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#651672", add =TRUE, text = "17", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#84145E", add =TRUE, text = "18", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#56167C", add =TRUE, text = "19", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#931455", add =TRUE, text = "28", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#EF1019", add =TRUE, text = "30", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#C11237", add =TRUE, text = "31", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#1919A4", add =TRUE, text = "33", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#B21341", add =TRUE, text = "37", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#FF1010", add =TRUE, text = "38", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#E01123", add =TRUE, text = "39", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#471786", add =TRUE, text = "44", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(temp, temp_surf, col = "#371790", add =TRUE, text = "45", pch = 18, normalize = TRUE)

#mid temp taylor diagram
month_join %>%
  filter(dataset_id == 3) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#751568", main = "Mid Temp Taylor Diagram", text = "3", pch = 16,normalize=TRUE)
month_join %>%
  filter(dataset_id == 6) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#FF1010", add =TRUE, text = "6", pch = 17,normalize=TRUE)
month_join %>%
  filter(dataset_id == 7) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#C11237", add =TRUE, text = "7", pch = 17,normalize=TRUE)
month_join %>%
  filter(dataset_id == 14) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#E01123", add =TRUE, text = "14", pch = 17,normalize=TRUE)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#371790", add =TRUE, text = "17", pch = 18,normalize=TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#56167C", add =TRUE, text = "18", pch = 15,normalize=TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#471786", add =TRUE, text = "19", pch = 15,normalize=TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#651672", add =TRUE, text = "28", pch = 18,normalize=TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#B21341", add =TRUE, text = "30", pch = 16,normalize=TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#A3134B", add =TRUE, text = "31", pch = 16,normalize=TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#1919A4", add =TRUE, text = "33", pch = 15,normalize=TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#84145E", add =TRUE, text = "37", pch = 18,normalize=TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#D1112D", add =TRUE, text = "38", pch = 18,normalize=TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#931455", add =TRUE, text = "39", pch = 18,normalize=TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#EF1019", add =TRUE, text = "44", pch = 16,normalize=TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(temp, temp_mid, col = "#28189A", add =TRUE, text = "45", pch = 18,normalize=TRUE)

#bot temp taylor diagram
month_join %>%
  filter(dataset_id == 3) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#751568", main = "Bottom Temp Taylor Diagram", text = "3", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 6) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#FF1010", add =TRUE, text = "6", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 7) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#D1112D", add =TRUE, text = "7", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 14) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#E01123", add =TRUE, text = "14", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#371790", add =TRUE, text = "17", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#651672", add =TRUE, text = "18", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#471786", add =TRUE, text = "19", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#56167C", add =TRUE, text = "28", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#A3134B", add =TRUE, text = "30", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#B21341", add =TRUE, text = "31", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#28189A", add =TRUE, text = "33", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#84145E", add =TRUE, text = "37", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#C11237", add =TRUE, text = "38", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#931455", add =TRUE, text = "39", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#EF1019", add =TRUE, text = "44", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(temp, temp_bot, col = "#1919A4", add =TRUE, text = "45", pch = 18, normalize = TRUE)

#surf ph taylor diagram
month_join %>%
  filter(dataset_id == 3) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#EF1019", main = "Surface pH Taylor Diagram", text = "3", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 6) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#FF1010", add =TRUE, text = "6", pch = 17,normalize = TRUE)
month_join %>%
  filter(dataset_id == 7) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#D1112D", add =TRUE, text = "7", pch = 17 ,normalize = TRUE)
month_join %>%
  filter(dataset_id == 14) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#E01123", add =TRUE, text = "14", pch = 17,normalize = TRUE)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#751568", add =TRUE, text = "17", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#651672", add =TRUE, text = "18", pch = 15,normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#471786", add =TRUE, text = "19", pch = 15,normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#FF1010", add =TRUE, text = "28", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#B21341", add =TRUE, text = "30", pch = 16,normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#A3134B", add =TRUE, text = "31", pch = 16,normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#931455", add =TRUE, text = "33", pch = 15,normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#E01123", add =TRUE, text = "37", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#D1112D", add =TRUE, text = "38", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#84145E", add =TRUE, text = "39", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#C11237", add =TRUE, text = "44", pch = 16,normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(ph, ph_surf, col = "#1919A4", add =TRUE, text = "45", pch = 18,normalize = TRUE)

#mid pH taylor diag
month_join %>%
  filter(dataset_id == 3) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#D1112D", main = "Mid pH Taylor Diagram", text = "3", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 6) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#FF1010", add =TRUE, text = "6", pch = 17,normalize = TRUE)
month_join %>%
  filter(dataset_id == 7) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#D1112D", add =TRUE, text = "7", pch = 17 ,normalize = TRUE)
month_join %>%
  filter(dataset_id == 14) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#E01123", add =TRUE, text = "14", pch = 17,normalize = TRUE)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#751568", add =TRUE, text = "17", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#651672", add =TRUE, text = "18", pch = 15,normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#471786", add =TRUE, text = "19", pch = 15,normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#EF1019", add =TRUE, text = "28", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#B21341", add =TRUE, text = "30", pch = 16,normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#931455", add =TRUE, text = "31", pch = 16,normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#A3134B", add =TRUE, text = "33", pch = 15,normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#E01123", add =TRUE, text = "37", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#C11237", add =TRUE, text = "38", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#84145E", add =TRUE, text = "39", pch = 18,normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#FF1010", add =TRUE, text = "44", pch = 16,normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(ph, ph_mid, col = "#1919A4", add =TRUE, text = "45", pch = 18,normalize = TRUE)

#bot ph taylor diagram 
month_join %>%
  filter(dataset_id == 3) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#B21341", main = "Bottom pH Taylor Diagram", text = "3", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 6) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#FF1010", add =TRUE, text = "6", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 7) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#D1112D", add =TRUE, text = "7", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 14) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#E01123", add =TRUE, text = "14", pch = 17, normalize = TRUE)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#751568", add =TRUE, text = "17", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#651672", add =TRUE, text = "18", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#471786", add =TRUE, text = "19", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#EF1019", add =TRUE, text = "28", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(ph, ph_bot, col = "##A3134B", add =TRUE, text = "30", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#931455", add =TRUE, text = "31", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#D1112D", add =TRUE, text = "33", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#E01123", add =TRUE, text = "37", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#C11237", add =TRUE, text = "38", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#84145E", add =TRUE, text = "39", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#FF1010", add =TRUE, text = "44", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(ph, ph_bot, col = "#1919A4", add =TRUE, text = "45", pch = 18, normalize = TRUE)

#surf do taylor diagram (dataset 3-14 was na couldnst start w it took it out)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(do, do_surf, col = "#B21341", main = "Surface DO Taylor Diagram", text = "17", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(do, do_surf, col = "#A3134B", add =TRUE, text = "18", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(do, do_surf, col = "#651672", add =TRUE, text = "19", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(do, do_surf, col = "#FF1010", add =TRUE, text = "28", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(do, do_surf, col = "#E01123", add =TRUE, text = "30", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(do, do_surf, col = "#84145E", add =TRUE, text = "31", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(do, do_surf, col = "#D1112D", add =TRUE, text = "33", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(do, do_surf, col = "#EF1019", add =TRUE, text = "37", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(do, do_surf, col = "#D1112D", add =TRUE, text = "38", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(do, do_surf, col = "#751568", add =TRUE, text = "39", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(do, do_surf, col = "#C11237", add =TRUE, text = "44", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(do, do_surf, col = "#B21341", add =TRUE, text = "45", pch = 18, normalize = TRUE)

#mid do taylor diagram (dataset 3-14 was na couldnst start w it took it out)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(do, do_mid, col = "#A3134B", main = "Mid DO Taylor Diagram", text = "17", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(do, do_mid, col = "#931455", add =TRUE, text = "18", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(do, do_mid, col = "#651672", add =TRUE, text = "19", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(do, do_mid, col = "#EF1019", add =TRUE, text = "28", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(do, do_mid, col = "#D1112D", add =TRUE, text = "30", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(do, do_mid, col = "#84145E", add =TRUE, text = "31", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(do, do_mid, col = "#D1112D", add =TRUE, text = "33", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(do, do_mid, col = "#E01123", add =TRUE, text = "37", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(do, do_mid, col = "#C11237", add =TRUE, text = "38", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(do, do_mid, col = "#751568", add =TRUE, text = "39", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(do, do_mid, col = "#FF1010", add =TRUE, text = "44", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(do, do_mid, col = "#B21341", add =TRUE, text = "45", pch = 18, normalize = TRUE)

#bot do taylor diagram (dataset 3-14 was na couldnst start w it took it out)
month_join %>%
  filter(dataset_id == 17) %$%
  taylor.diagram.modified(do, do_bot, col = "#A3134B", main = "Bottom DO Taylor Diagram", text = "17", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 18) %$%
  taylor.diagram.modified(do, do_bot, col = "#84145E", add =TRUE, text = "18", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 19) %$%
  taylor.diagram.modified(do, do_bot, col = "#751568", add =TRUE, text = "19", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 28) %$%
  taylor.diagram.modified(do, do_bot, col = "#EF1019", add =TRUE, text = "28", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 30) %$%
  taylor.diagram.modified(do, do_bot, col = "#B21341", add =TRUE, text = "30", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 31) %$%
  taylor.diagram.modified(do, do_bot, col = "#931455", add =TRUE, text = "31", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 33) %$%
  taylor.diagram.modified(do, do_bot, col = "#D1112D", add =TRUE, text = "33", pch = 15, normalize = TRUE)
month_join %>%
  filter(dataset_id == 37) %$%
  taylor.diagram.modified(do, do_bot, col = "#E01123", add =TRUE, text = "37", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 38) %$%
  taylor.diagram.modified(do, do_bot, col = "#D1112D", add =TRUE, text = "38", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 39) %$%
  taylor.diagram.modified(do, do_bot, col = "#651672", add =TRUE, text = "39", pch = 18, normalize = TRUE)
month_join %>%
  filter(dataset_id == 44) %$%
  taylor.diagram.modified(do, do_bot, col = "#FF1010", add =TRUE, text = "44", pch = 16, normalize = TRUE)
month_join %>%
  filter(dataset_id == 45) %$%
  taylor.diagram.modified(do, do_bot, col = "#C11237", add =TRUE, text = "45", pch = 18, normalize = TRUE)
###########
#time series box plots #####
###########

#plots for averaged temp/ph/do across years
#6 - temp only
ggplot() +
  geom_boxplot(data = mod6, aes(x=Month, y=T_surf, na.rm = TRUE), color = "pink", fill = "pink", alpha = 0.4 ) +
  scale_color_brewer()+
  geom_boxplot(data = mod6, aes(x=Month, y=T_bot, na.rm = TRUE, fill = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer()+
  geom_boxplot(data = daily_6, aes(x=month, y=temp_daily_avg, na.rm = TRUE, fill = "Obs"), alpha = 0.4) + 
  scale_color_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "402 Temp Observations") 
?scale_color_manual

#7 - temp only
ggplot() +
  geom_boxplot(data = mod7, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod7, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_7, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "5273 Temp Observations") 

#14 - temp only
ggplot() +
  geom_boxplot(data = mod14, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod14, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_14, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "2232 Temp Observations") 

#31
#temp
ggplot() +
  geom_boxplot(data = mod31, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod31, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_31, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1381 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod31, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod31, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_31, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "1381 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod31, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod31, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_31, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "739 DO Observations") 

#30
#temp
ggplot() +
  geom_boxplot(data = mod30, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod30, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_30, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1040 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod30, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod30, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_30, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "1040 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod30, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod30, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_30, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "292 DO Observations") 

#3
#temp
ggplot() +
  geom_boxplot(data = mod3, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod3, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_3, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "578 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod3, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod3, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_3, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "565 pH Observations")   

#44
#temp
ggplot() +
  geom_boxplot(data = mod44, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod44, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_44, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "2299 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod44, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod44, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_44, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "920 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod44, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod44, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_44, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "2299 DO Observations") 

#18
#temp
ggplot() +
  geom_boxplot(data = mod18, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod18, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_18, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "530 Temp Observations")
#do
ggplot() +
  geom_boxplot(data = mod18, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod18, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_18, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "425 DO Observations")

#33
#temp
ggplot() +
  geom_boxplot(data = mod33, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod33, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_33, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "821 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod33, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod33, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_33, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "821 pH Observations")   

#19
#temp
ggplot() +
  geom_boxplot(data = mod19, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod19, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_19, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1951 Temp Observations")
#do
ggplot() +
  geom_boxplot(data = mod19, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod19, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_19, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "1461 DO Observations") 

#38
#temp
ggplot() +
  geom_boxplot(data = mod38, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod38, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_38, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "91 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod38, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod38, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_38, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "71 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod38, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod38, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_38, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "86 DO Observations") 

#28
#temp
ggplot() +
  geom_boxplot(data = mod28, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod28, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_28, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "325 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod28, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod28, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_28, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "204 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod28, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod28, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_28, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "300 DO Observations") 

#37
#temp
ggplot() +
  geom_boxplot(data = mod37, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod37, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_37, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "82 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod37, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod37, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_37, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "56 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod37, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod37, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_37, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "72 DO Observations") 

#39
#temp
ggplot() +
  geom_boxplot(data = mod39, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod39, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_39, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "530 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod39, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod39, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_39, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "530 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod39, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod39, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_39, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "530 DO Observations") 

#17
#temp
ggplot() +
  geom_boxplot(data = mod17, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod17, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_17, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "1517 Temp Observations")
#ph
ggplot() +
  geom_boxplot(data = mod17, aes(x=Month, y=pH_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod17, aes(x=Month, y=pH_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_17, aes(x=month, y=pH_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean pH",  subtitle = "1525 pH Observations")   
#do
ggplot() +
  geom_boxplot(data = mod17, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod17, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_17, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "457 DO Observations") 
sum(!is.na(daily_45$temp_daily_avg))
sum(!is.na(daily_45$pH_daily_avg))
sum(!is.na(daily_45$do_daily_avg))

#45
#temp
ggplot() +
  geom_boxplot(data = mod45, aes(x=Month, y=T_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod45, aes(x=Month, y=T_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_45, aes(x=month, y=temp_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean temp (C)", subtitle = "2751 Temp Observations")
#do
ggplot() +
  geom_boxplot(data = mod45, aes(x=Month, y=DO_surf, na.rm = TRUE, color = "Model surface"), alpha = 0.4 ) +
  scale_color_brewer(aesthetics = c("colour,fill")) +
  scale_fill_brewer() +
  geom_boxplot(data = mod45, aes(x=Month, y=DO_bot, na.rm = TRUE, color = "Model bottom"), alpha = 0.4 ) +
  scale_color_brewer() +
  scale_fill_brewer() +
  geom_boxplot(data = daily_45, aes(x=month, y=do_daily_avg, na.rm = TRUE, color = "Obs"), alpha = 0.4) + 
  scale_color_brewer() +
  scale_fill_brewer()+
  theme_classic() + labs(x = "Month", y = "Mean DO",  subtitle = "2265 DO Observations") 


################
#unsat analysis#
################






