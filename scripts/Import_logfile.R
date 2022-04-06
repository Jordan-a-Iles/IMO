# Script name: NTU import
# Purpose: Import NTU and MS9 logfiles
# 
# Author: Jordan Iles
# Date created:  2020-10-15
# Email: Jordan.iles@jcu.edu.au


#### Prepare the workspace ----
# rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)


files <- list.files(path = 'data/')

temp <- read_lines(file = paste('data/', files[2], sep = ""))
temp_file <- str_replace_all(temp[3], "[:()]", "")
new_file <- paste0(temp_file, ' ', today(), '.txt')
write_lines(x = temp, 
            path = paste0('data/', new_file))

#### Import NTU logfile ----
NTU.log <- read_delim(file = paste('data/', files[2], sep = ""),
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
         Timestamp_10min = floor_date(Timestamp, unit = "minute")) %>% 
  filter(ID == 'NTU-LPT')

NTU.log.10min <- NTU.log %>% 
  group_by(Timestamp_10min) %>% 
  summarise(Voltage = mean(Voltage, na.rm = TRUE),
            DepthW = mean(Depth, na.rm = TRUE),
            TempW = mean(TempW, na.rm = TRUE),
            Tilt = mean(Tilt, na.rm = TRUE),
            COUNTS = mean(COUNTS, na.rm = TRUE),
            NTU = mean(NTU, na.rm = TRUE),
            RMS = sqrt(sum(Depth - mean(Depth))^2)/length(Depth))


NTU.log.10m <- NTU.log %>% 
  group_by(Timestamp_10min) %>% 
  summarise(Voltage2 = mean(Voltage, na.rm = TRUE),
            Depth2 = mean(Depth, na.rm = TRUE),
            TempW2 = mean(TempW, na.rm = TRUE),
            Tilt2 = mean(Tilt, na.rm = TRUE),
            COUNTS2 = mean(COUNTS, na.rm = TRUE),
            NTU2 = mean(NTU, na.rm = TRUE),
            RMS2 = sqrt(sum(Depth - mean(Depth))^2)/length(Depth))


ggplot(NTU.log.10m, aes(x = Timestamp_10min, y = RMS2)) +
  geom_line()

#### calculate RMS values for each 10 min interval
#### below is using just the first 10 readings but should be expanded to all 50 
RMS <- NTU.log %>% 
  select(Timestamp, Timestamp_10min, Depth) %>% 
  group_by(Timestamp_10min) %>% 
  mutate(burst_id = paste0('v', 1:n())) %>% 
  pivot_wider(id_cols = Timestamp_10min,
              names_from = burst_id,
              values_from = Depth) 
  
RMS <- RMS %>% 
  mutate(Depth = mean(v1:v10, na.rm = TRUE), 
         RMS = ((((v1 - Depth)^2) + 
                 ((v2 - Depth)^2) + 
                 ((v3 - Depth)^2) + 
                 ((v4 - Depth)^2) + 
                 ((v5 - Depth)^2) + 
                 ((v6 - Depth)^2) + 
                 ((v7 - Depth)^2) +
                 ((v8 - Depth)^2) + 
                 ((v9 - Depth)^2) + 
                 ((v10 - Depth)^2))/10)^0.5) 




RMS.long <- RMS %>% 
  select(Timestamp_10min, Depth, RMS) %>% 
  pivot_longer(cols = c(Depth, RMS),
               names_to = 'parameter', 
               values_to = 'value')

ggplot(RMS.long, aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  theme_bw(base_size = 16)

NTU.long <- NTU.log.10min %>% 
  pivot_longer(cols = -Timestamp_10min,
               names_to = 'parameter', 
               values_to = 'value')
  

ggplot(NTU.long, aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  theme_bw(base_size = 16)






#### Import MS9 logfile ----
MS9.log <- read_delim(file = paste('data/', files[1], sep = ""),
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



MS9.log.10min <- MS9_log %>% 
  group_by(Timestamp_10min) %>% 
  summarise(PAR = mean(PAR, na.rm = TRUE), 
            Channel_0 = mean(Channel_1, na.rm = TRUE),
            Channel_1 = mean(Channel_1, na.rm = TRUE),
            Channel_2 = mean(Channel_1, na.rm = TRUE),
            Channel_3 = mean(Channel_1, na.rm = TRUE),
            Channel_4 = mean(Channel_1, na.rm = TRUE),
            Channel_5 = mean(Channel_1, na.rm = TRUE),
            Channel_6 = mean(Channel_1, na.rm = TRUE),
            Channel_7 = mean(Channel_1, na.rm = TRUE),
            Channel_8 = mean(Channel_1, na.rm = TRUE))

test <- MS9.log.10min %>% 
  rowwise() %>% 
  mutate(foo = sum(Channel_1:Channel_8), 
         bar = PAR/foo)

ggplot(test, aes(x = PAR, y = foo)) +
  geom_point()

MS9.long <- MS9.log.10min %>% 
  filter(Timestamp_10min < as_datetime('2020-10-14 05:00:00')) %>% 
  pivot_longer(cols = -Timestamp_10min,
               names_to = 'parameter', 
               values_to = 'value')
  
ggplot(MS9.long, aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  theme_bw(base_size = 16)
  
ggplot(MS9.log.10min, aes(x = Timestamp_10min, y = Channel_1)) +
  geom_line(col = 'blue') +
  scale_y_continuous(limits = c(0, 5)) +
  theme_bw(base_size = 16)



#### Channel allocations ----
# Channel 0 = 410 nm
# Channel 1 = 440 nm
# Channel 2 = 490 nm
# Channel 3 = 510 nm
# Channel 4 = 550 nm
# Channel 5 = 636 nm
# Channel 6 = 660 nm
# Channel 7 = 685 nm
# Channel 8 = 710 nm

ggplot()
