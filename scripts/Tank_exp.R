# Script name: Nathans tank experiment
# Purpose: Import NTU logfile and plot up Nathans tank experiment
# 
# Author: Jordan Iles
# Date created:  2020-10-15
# Email: Jordan.iles@jcu.edu.au


#### Prepare the workspace ----
# rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)

files <- list.files(path = 'data/Tank')

#### Import NTU logfile ----
NTU.log <- read_delim(file = 'data/Tank/Tank experiment 2020-12-17 LOG_0013.TXT',
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


NTU.raw.plot <- NTU.log %>% 
  filter(Timestamp_1min >= as_datetime('2020-12-17 17:30:00', tz = NULL) &
           Timestamp_1min <= as_datetime('2020-12-17 18:54:00', tz = NULL)) %>% 
  ggplot(aes(x = Timestamp, y = NTU)) +
  geom_point(col = 'blue', size = 0.2) +
  xlab(NULL) +
  ylab('Turbidity (NTU)') +
  scale_y_continuous(limits = c(0, 40)) +
  # scale_y_log10() +
  theme_bw(base_size = 16)


ggsave(filename = 'output/Tank/plots/Tank_exp_raw_2020-12-17.png',
       plot = NTU.raw.plot,
       width = 15.9, 
       height = 7.95,
       units = 'cm')


NTU.log.1min <- NTU.log %>% 
  group_by(Timestamp_1min) %>% 
  summarise(Voltage = mean(Voltage, na.rm = TRUE),
            DepthW = mean(Depth, na.rm = TRUE),
            TempW = mean(TempW, na.rm = TRUE),
            Tilt = mean(Tilt, na.rm = TRUE),
            COUNTS = mean(COUNTS, na.rm = TRUE),
            NTU = mean(NTU, na.rm = TRUE),
            RMS = sqrt(sum(Depth - mean(Depth))^2)/length(Depth),
            .groups = 'drop')


# NTU.log.1m <- NTU.log %>% 
#   group_by(Timestamp_1min) %>% 
#   summarise(Voltage2 = mean(Voltage, na.rm = TRUE),
#             Depth2 = mean(Depth, na.rm = TRUE),
#             TempW2 = mean(TempW, na.rm = TRUE),
#             Tilt2 = mean(Tilt, na.rm = TRUE),
#             COUNTS2 = mean(COUNTS, na.rm = TRUE),
#             NTU2 = mean(NTU, na.rm = TRUE),
#             RMS2 = sqrt(sum(Depth - mean(Depth))^2)/length(Depth),
#             .groups = 'drop')


NTU.plot <- NTU.log.1min %>% 
  filter(Timestamp_1min >= as_datetime('2020-12-17 17:30:00', tz = NULL) &
           Timestamp_1min <= as_datetime('2020-12-17 18:54:00', tz = NULL)) %>% 
  ggplot(aes(x = Timestamp_1min, y = NTU)) +
  # geom_point(col = 'black', size = 0.2) +
  geom_line(col = 'blue') +
  xlab(NULL) +
  ylab('Turbidity (NTU)') +
  theme_bw(base_size = 16)


ggsave(filename = 'output/Tank/plots/Tank_exp_2020-12-17.png',
       plot = NTU.plot,
       width = 15.9, 
       height = 7.95,
       units = 'cm')

