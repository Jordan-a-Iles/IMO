# Script name: Profiling pilot
# Purpose: Import NTU logfile and plot up pilot profiles from Mackay trip
# 
# Author: Jordan Iles
# Date created:  2021-01-29
# Email: Jordan.iles@jcu.edu.au


#### Prepare the workspace ----
# rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)
library(plotly)

files <- list.files(path = 'data/Profile')

#### Import NTU logfile ----
NTU.log <- read_delim(file = 'data/Profile/NTU-LPT0049_LOG_0029.TXT',
                      delim = ",",
                      col_names = c('ID', 'SN', 'Date', 'Time', 'Wiper', 'Voltage',
                                    'Depth', 'TempW', 'Tilt', 'TempLED', 'TempDET',
                                    'LEDREF', 'COUNTS', 'NTU'), 
                      col_types = cols(.default = col_double(),
                                       ID = col_character(),
                                       SN = col_character(),
                                       Date = col_date(format = '%d/%m/%Y'),
                                       Time = col_time(format = '%H:%M:%OS')),
                      skip = 37) %>% 
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%OS"),
         Timestamp_1min = floor_date(Timestamp, unit = "minute")) %>% 
  filter(ID == 'NTU-LPT')

p1 <- ggplot(NTU.log, aes(x = Timestamp, y = Depth)) +
  geom_point() +
  scale_y_reverse()
ggplotly(p1)

NTU.cut <- NTU.log %>% 
  filter(Depth > 0) %>% 
  filter(Timestamp >= as_datetime('2021-02-09 12:26:05', tz = NULL) &
           Timestamp <= as_datetime('2021-02-09 12:27:19', tz = NULL))

Depth.plot <- NTU.cut %>% 
  # filter(Timestamp >= as_datetime('2021-01-27 15:45:31', tz = NULL) &
  #          Timestamp <= as_datetime('2021-01-27 15:46:02', tz = NULL)) %>% 
  # filter(Timestamp >= as_datetime('2021-01-27 15:46:02', tz = NULL) &
  #          Timestamp <= as_datetime('2021-01-27 15:46:32', tz = NULL)) %>% 
  ggplot(aes(x = NTU, y = Depth)) +
  geom_point(col = 'blue', size = 0.2) +
  geom_smooth(method = 'loess') +
  xlab('Turbidity (NTU)') +  
  ylab('Depth (m)') +
  scale_x_continuous(limits = c(0, 2.5)) +
  scale_y_reverse() +
  # scale_y_log10() +
  theme_bw(base_size = 16)

NTU.bin <- NTU.cut %>% 
  mutate(Depth_10cm = round(x = Depth, digits = 1)) %>% 
  group_by(Depth_10cm) %>% 
  summarise(NTU.mean = mean(NTU, na.rm = TRUE), 
            NTU.sd = sd(NTU, na.rm = TRUE),
            NTU.n = n(),
            .groups = 'drop')

NTU.plot <- NTU.bin %>% 
  filter (Depth_10cm > 0) %>% 
  ggplot(aes(x = NTU.mean, y = Depth_10cm)) +
  geom_errorbarh(aes(xmin = NTU.mean - NTU.sd, 
                     xmax = NTU.mean + NTU.sd)) +
  geom_text(aes(x = NTU.mean + NTU.sd + 0.05, y = Depth_10cm, label = NTU.n), 
            size = 2, col = 'grey40') +
  # geom_path(col = 'light blue') +
  geom_point(col = 'blue') +
  scale_y_reverse() +
  xlab('Turbidity (NTU)') +
  ylab('Depth (m)') +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank())
  
ggsave(filename = 'output/Profile/plots/Turb_BowenChannel.png',
       plot = NTU.plot,
       width = 10.6, 
       height = 21.2,
       units = 'cm')


Temp.bin <- NTU.cut %>% 
  mutate(Depth_10cm = round(x = Depth, digits = 1)) %>% 
  group_by(Depth_10cm) %>% 
  summarise(Temp.mean = mean(TempW, na.rm = TRUE), 
            Temp.sd = sd(TempW, na.rm = TRUE),
            Temp.n = n(),
            .groups = 'drop')

Temp.plot <- Temp.bin %>% 
  filter (Depth_10cm > 0) %>% 
  ggplot(aes(x = Temp.mean, y = Depth_10cm)) +
  geom_errorbarh(aes(xmin = Temp.mean - Temp.sd, 
                     xmax = Temp.mean + Temp.sd)) +
  geom_text(aes(x = Temp.mean + Temp.sd + 0.05, y = Depth_10cm, label = Temp.n), 
            size = 2, col = 'grey40') +
  # geom_path(col = 'light blue') +
  geom_point(col = 'blue') +
  scale_y_reverse() +
  xlab('Temperature (°C)') +
  ylab('Depth (m)') +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank())

ggsave(filename = 'output/Profile/plots/Temp_BowenChannel.png',
       plot = Temp.plot,
       width = 10.6, 
       height = 21.2,
       units = 'cm')

Tilt.bin <- NTU.cut %>% 
  mutate(Depth_10cm = round(x = Depth, digits = 1)) %>% 
  group_by(Depth_10cm) %>% 
  summarise(Tilt.mean = mean(Tilt, na.rm = TRUE), 
            Tilt.sd = sd(Tilt, na.rm = TRUE),
            Tilt.n = n(),
            .groups = 'drop')

Tilt.plot <- Tilt.bin %>% 
  filter (Depth_10cm > 0) %>% 
  ggplot(aes(x = Tilt.mean, y = Depth_10cm)) +
  geom_vline(xintercept = 180, col = 'tomato') +
  geom_errorbarh(aes(xmin = Tilt.mean - Tilt.sd, 
                     xmax = Tilt.mean + Tilt.sd)) +
  geom_text(aes(x = Tilt.mean + Tilt.sd + 0.05, y = Depth_10cm, label = Tilt.n), 
            size = 2, col = 'grey40') +
  # geom_path(col = 'light blue') +
  geom_point(col = 'blue') +
  scale_y_reverse() +
  xlab('Tilt (°)') +
  ylab('Depth (m)') +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank())

ggsave(filename = 'output/Profile/plots/Tilt.png',
       plot = Tilt.plot,
       width = 10.6, 
       height = 21.2,
       units = 'cm')



#### Import MS9 logfile ----
MS9.log <- read_delim(file = 'data/Profile/MS9-LPT0052_LOG_0013.TXT',
                      delim = ",",
                      col_names = c('ID', 'SN', 'Date', 'Time', 'Wiper', 'Voltage',
                                    'Depth', 'TempW', 'Tilt', 'TempINT', 'PAR',
                                    'Channel_0', 'Channel_1', 'Channel_2', 
                                    'Channel_3', 'Channel_4', 'Channel_5',
                                    'Channel_6', 'Channel_7','Channel_8'), 
                      col_types = cols(.default = col_double(),
                                       ID = col_character(),
                                       SN = col_character(),
                                       Date = col_date(format = '%d/%m/%Y'),
                                       Time = col_time(format = '%H:%M:%OS')),
                      skip = 39) %>% 
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%OS"),
         Timestamp_10min = floor_date(Timestamp, unit = "minute"))


p2 <- ggplot(MS9.log, aes(x = Timestamp, y = Depth)) +
  geom_point() +
  scale_y_reverse()
ggplotly(p2)

MS9.cut <- MS9.log %>% 
  filter(Depth > 0) %>% 
  filter(Timestamp >= as_datetime('2021-02-09 12:26:25', tz = NULL) &
           Timestamp <= as_datetime('2021-02-09 12:27:21', tz = NULL))




MS9.Depth.plot <- MS9.cut %>% 
  # filter(Timestamp >= as_datetime('2021-01-27 15:45:31', tz = NULL) &
  #          Timestamp <= as_datetime('2021-01-27 15:46:02', tz = NULL)) %>% 
  ggplot(aes(x = PAR, y = Depth)) +
  geom_point(col = 'blue', size = 0.2) +
  xlab(expression(PAR~(µW~cm^-2~nm^-1))) +  
  ylab('Depth (m)') +
  # scale_x_continuous(limits = c(0, 2.5)) +
  scale_y_reverse() +
  # scale_y_log10() +
  theme_bw(base_size = 16)


MS9.bin <- MS9.cut %>% 
  mutate(Depth_10cm = round(x = Depth, digits = 1)) %>% 
  group_by(Depth_10cm) %>% 
  summarise(PAR.mean = mean(PAR, na.rm = TRUE), 
            PAR.sd = sd(PAR, na.rm = TRUE),
            PAR.n = n(),
            .groups = 'drop')

PAR.plot <- MS9.bin %>% 
  filter(Depth_10cm > 0) %>% 
  ggplot(aes(x = PAR.mean, y = Depth_10cm)) +
  geom_errorbarh(aes(xmin = PAR.mean - PAR.sd, 
                     xmax = PAR.mean + PAR.sd)) +
  geom_text(aes(x = PAR.mean + PAR.sd + 0.05, y = Depth_10cm, label = PAR.n), 
            size = 2, col = 'grey40') +
  # geom_path(col = 'light blue') +
  geom_point(col = 'blue') +
  scale_y_reverse() +
  # xlab(expression(PAR~(µW~cm^-2~nm^-1))) +  
  xlab(expression(PAR~(µmol~m^-2~s^-1))) +
  ylab('Depth (m)') +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank())

ggsave(filename = 'output/Profile/plots/PAR_BowenChannel.png',
       plot = PAR.plot,
       width = 10.6, 
       height = 21.2,
       units = 'cm')



breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

PAR.k.plot <- MS9.bin %>% 
  filter(Depth_10cm > 0) %>% 
  ggplot(aes(x = Depth_10cm, y = PAR.mean)) +
  geom_smooth(method = 'lm', formula = y ~ x, 
              col = 'dark red', alpha = 0) +
  geom_errorbar(aes(ymin = PAR.mean - PAR.sd, 
                     ymax = PAR.mean + PAR.sd)) +
  geom_point(col = 'blue') +
  coord_flip() +
  scale_x_reverse() +
  scale_y_log10(limits = c(10, 3000),
                breaks = breaks,
                minor_breaks = minor_breaks) +
  xlab('Depth (m)') +
  # ylab(expression(PAR~(µW~cm^-2~nm^-1))) +  
  ylab(expression(PAR~(µmol~m^-2~s^-1))) +
  theme_bw(base_size = 16) # +
  # theme(panel.grid = element_blank())

ggsave(filename = 'output/Profile/plots/PAR_k_BowenChannel.png',
       plot = PAR.k.plot,
       width = 10.6, 
       height = 21.2,
       units = 'cm')

fit <- MS9.bin %>% 
  filter(Depth_10cm > 0) %>% 
  lm(formula = -log2(PAR.mean) ~ Depth_10cm)

PAR.Kd <- as.numeric(fit$coefficients[2])
# Kd = 0.28 m-1  

  
MS9.pc.bin <- MS9.cut %>% 
  mutate(Depth_10cm = round(x = Depth, digits = 1)) %>% 
  group_by(Depth_10cm) %>% 
  summarise(PAR.mean = mean(PAR/2130, na.rm = TRUE), 
            PAR.sd = sd(PAR/2130, na.rm = TRUE),
            PAR.n = n(),
            .groups = 'drop')


#### light attenuation coefficient (Kd) ----
fit <- MS9.pc.bin %>% 
  filter(Depth_10cm > 0) %>% 
  lm(formula = -log2(PAR.mean) ~ Depth_10cm)

PAR.Kd <- round(as.numeric(fit$coefficients[2]), 2)
# Kd = 0.28 m-1  



PAR.Kd.plot <- MS9.pc.bin %>% 
  filter(Depth_10cm > 0) %>% 
  ggplot(aes(x = Depth_10cm, y = PAR.mean)) +
  geom_smooth(method = 'glm', formula = y ~ log(-x), 
              col = 'dark red', alpha = 0) +
  # geom_smooth(method = "glm", formula = y~x,
  #             method.args = list(family = gaussian(link = 'log'))) +
  geom_errorbar(aes(ymin = PAR.mean - PAR.sd, 
                    ymax = PAR.mean + PAR.sd)) +
  geom_point(col = 'blue') +
  annotate(geom = 'text', 
           x = 8, y = 0.8, 
            # label = paste0('Kd = ', round(PAR.Kd, 2), ' m^-1')) +
           label = expression(K[d]~'='~0.11~m^-1)) +
           # label = expression(K[d]~'='~PAR.Kd~m^-1)) +
           # label = label) +
  coord_flip() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 1)) +
  xlab('Depth (m)') +
  ylab('PAR (% surface)') +  
  theme_bw(base_size = 16)

ggsave(filename = 'output/Profile/plots/PAR_Kd.png',
       plot = PAR.Kd.plot,
       width = 10.6, 
       height = 21.2,
       units = 'cm')


