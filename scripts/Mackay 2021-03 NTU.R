# Script name: Data import
# Purpose: Import NTU and MS9 logfiles from Mackay 2021-03
# 
# Author: Jordan Iles
# Date created:  2020-10-15
# Email: Jordan.iles@jcu.edu.au


#### Prepare the workspace ----
# rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)


files <- list.files(path = 'data/Mackay 2021-03')

# temp <- read_lines(file = paste0('data/Mackay 2021-03/', files[2]))
# temp_file <- str_replace_all(temp[3], "[:()]", "")
# new_file <- paste0(temp_file, ' ', today(), '.txt')
# write_lines(x = temp, 
#             path = paste0('data/', new_file))

#### Import NTU logfile ----
#### MKY_AMB5 (Slade Island) NTU-LPT0051 ----

MKY_AMB5.NTU <- read_delim(file = 'data/Mackay 2021-03/NTU-LPT0051_LOG_0020.TXT',
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
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%OS", tz = 'GMT'),
         Timestamp_10min = floor_date(Timestamp, unit = "minute")) %>% 
  filter(ID == 'NTU-LPT')

MKY_AMB5.NTU.10min <- MKY_AMB5.NTU %>% 
  group_by(Timestamp_10min) %>% 
  summarise(Voltage = mean(Voltage, na.rm = TRUE),
            DepthW = mean(Depth, na.rm = TRUE),
            TempW = mean(TempW, na.rm = TRUE),
            Tilt = mean(Tilt, na.rm = TRUE),
            COUNTS = mean(COUNTS, na.rm = TRUE),
            NTU = mean(NTU, na.rm = TRUE),
            .groups = 'drop')


#### calculate RMS values for each 10 min interval
#### below is using just the first 10 readings but should be expanded to all 50 
MKY_AMB5.RMS <- MKY_AMB5.NTU %>% 
  select(Timestamp, Timestamp_10min, Depth) %>% 
  group_by(Timestamp_10min) %>% 
  mutate(burst_id = paste0('v', 1:n())) %>% 
  pivot_wider(id_cols = Timestamp_10min,
              names_from = burst_id,
              values_from = Depth) 

MKY_AMB5.RMS.10min <- MKY_AMB5.RMS %>% 
  mutate(Depth = mean(v1:v50, na.rm = TRUE), 
         RMS = ((((v1 - Depth)^2) + 
                   ((v6 - Depth)^2) + 
                   ((v11 - Depth)^2) + 
                   ((v16 - Depth)^2) + 
                   ((v21 - Depth)^2) + 
                   ((v26 - Depth)^2) + 
                   ((v31 - Depth)^2) +
                   ((v36 - Depth)^2) + 
                   ((v41 - Depth)^2) + 
                   ((v46 - Depth)^2))/10)^0.5) %>% 
  select(Timestamp_10min, RMS)


MKY_AMB5.NTU.10min <- left_join(x = MKY_AMB5.NTU.10min, 
                               y = MKY_AMB5.RMS.10min, 
                               by = "Timestamp_10min")


MKY_AMB5.NTU.long <- MKY_AMB5.NTU.10min %>% 
  pivot_longer(cols = -Timestamp_10min,
               names_to = 'parameter', 
               values_to = 'value')


MKY_AMB5_plot <- MKY_AMB5.NTU.long  %>% 
  filter(Timestamp_10min >= as_datetime("2021-01-26 16:00:00") &
           Timestamp_10min <= as_datetime("2021-03-10 09:00:00")) %>% 
  filter(!parameter == 'COUNTS') %>% 
  ggplot(aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter), cex = 0.2) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  ggtitle(label = 'MKY_AMB5 (Slade Island)') +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = 'output/Mackay 2021-03/plots/MKY_AMB5_NTU.png',
       plot = MKY_AMB5_plot,
       width = 21.2, 
       height = 15.9,
       units = 'cm')


#### MKY_AMB10 (Victor Island) NTU-LPT0050 ----

MKY_AMB10.NTU <- read_delim(file = 'data/Mackay 2021-03/NTU-LPT0050_LOG_0006.TXT',
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
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%OS", tz = 'GMT'),
         Timestamp_10min = floor_date(Timestamp, unit = "minute")) %>% 
  filter(ID == 'NTU-LPT')

MKY_AMB10.NTU.10min <- MKY_AMB10.NTU %>% 
  group_by(Timestamp_10min) %>% 
  summarise(Voltage = mean(Voltage, na.rm = TRUE),
            DepthW = mean(Depth, na.rm = TRUE),
            TempW = mean(TempW, na.rm = TRUE),
            Tilt = mean(Tilt, na.rm = TRUE),
            COUNTS = mean(COUNTS, na.rm = TRUE),
            NTU = mean(NTU, na.rm = TRUE),
            .groups = 'drop')


#### calculate RMS values for each 10 min interval
#### below is using just the first 10 readings but should be expanded to all 50 
MKY_AMB10.RMS <- MKY_AMB10.NTU %>% 
  select(Timestamp, Timestamp_10min, Depth) %>% 
  group_by(Timestamp_10min) %>% 
  mutate(burst_id = paste0('v', 1:n())) %>% 
  pivot_wider(id_cols = Timestamp_10min,
              names_from = burst_id,
              values_from = Depth) 

MKY_AMB10.RMS.10min <- MKY_AMB10.RMS %>% 
  mutate(Depth = mean(v1:v50, na.rm = TRUE), 
         RMS = ((((v1 - Depth)^2) + 
                   ((v6 - Depth)^2) + 
                   ((v11 - Depth)^2) + 
                   ((v16 - Depth)^2) + 
                   ((v21 - Depth)^2) + 
                   ((v26 - Depth)^2) + 
                   ((v31 - Depth)^2) +
                   ((v36 - Depth)^2) + 
                   ((v41 - Depth)^2) + 
                   ((v46 - Depth)^2))/10)^0.5) %>% 
  select(Timestamp_10min, RMS)


MKY_AMB10.NTU.10min <- left_join(x = MKY_AMB10.NTU.10min, 
                                y = MKY_AMB10.RMS.10min, 
                                by = "Timestamp_10min")


MKY_AMB10.NTU.long <- MKY_AMB10.NTU.10min %>% 
  pivot_longer(cols = -Timestamp_10min,
               names_to = 'parameter', 
               values_to = 'value')

MKY_AMB10_plot <- MKY_AMB10.NTU.long  %>% 
  filter(Timestamp_10min >= as_datetime("2021-01-26 16:00:00") &
           Timestamp_10min <= as_datetime("2021-03-10 09:00:00")) %>% 
  filter(!parameter == 'COUNTS') %>% 
  ggplot(aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter), cex = 0.2) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  ggtitle(label = 'MKY_AMB10 (Victor Island)') +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = 'output/Mackay 2021-03/plots/MKY_AMB10_NTU.png',
       plot = MKY_AMB10_plot,
       width = 21.2, 
       height = 15.9,
       units = 'cm')

require(plotly)

ggplotly(MKY_AMB5_plot)


# #### Combine the lot into one tibble ----
# dat.imo.NTU <- bind_rows('MKY_AMB5' = MKY_AMB5.NTU.10min, 'MKY_AMB10' = MKY_AMB10.NTU.10min,  
#                          .id = 'Site_code')
# 
# # save(dat.imo.NTU, 
# #      file = 'data/imo_NTU.Rdata')
# 
# 
# dat.imo.NTU.long <- dat.imo.NTU %>% 
#   group_by(Site_code, Timestamp_10min) %>% 
#   pivot_longer(cols = c('Voltage', 'DepthW', 'TempW', 'Tilt', 'NTU', 'RMS'),
#                names_to = 'parameter',
#                values_to = 'value') 
# 
# dat.imo.NTU.long %>% 
#   filter(Site_code == 'MKY_AMB5') %>% 
#   filter(Timestamp_10min >= as_datetime("2021-01-26 16:00:00") &
#            Timestamp_10min <= as_datetime("2021-03-10 09:00:00")) %>% 
#   ggplot(aes(x = Timestamp_10min, y = value)) +
#   geom_line(aes(col = parameter)) +
#   facet_grid(. ~ parameter) +
#   theme_bw()

MKY_AMB5.MS9 <- read_delim(file = 'data/Mackay 2021-03/MS9-LPT0053_LOG_0013.TXT',
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
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%OS", tz = 'GMT'),
         Timestamp_10min = floor_date(Timestamp, unit = "minute"))



MKY_AMB5.MS9.10min <- MKY_AMB5.MS9 %>% 
  group_by(Timestamp_10min) %>% 
  summarise(Depth = mean(Depth, na.rm = TRUE), 
            Temp = mean(TempW, na.rm = TRUE),
            Tilt = mean(Tilt, na.rm = TRUE),
            PAR = mean(PAR, na.rm = TRUE), 
            Channel_0 = mean(Channel_0, na.rm = TRUE),
            Channel_1 = mean(Channel_1, na.rm = TRUE),
            Channel_2 = mean(Channel_2, na.rm = TRUE),
            Channel_3 = mean(Channel_3, na.rm = TRUE),
            Channel_4 = mean(Channel_4, na.rm = TRUE),
            Channel_5 = mean(Channel_5, na.rm = TRUE),
            Channel_6 = mean(Channel_6, na.rm = TRUE),
            Channel_7 = mean(Channel_7, na.rm = TRUE),
            Channel_8 = mean(Channel_8, na.rm = TRUE),
            .groups = 'drop')


MKY_AMB5.MS9.long <- MKY_AMB5.MS9.10min %>% 
  pivot_longer(cols = -Timestamp_10min,
               names_to = 'parameter', 
               values_to = 'value')

MKY_AMB5_MS9_plot <- MKY_AMB5.MS9.long  %>% 
  filter(Timestamp_10min >= as_datetime("2021-01-26 16:00:00") &
           Timestamp_10min <= as_datetime("2021-03-10 09:00:00")) %>% 
  filter(parameter %in% c('Depth', 'Temp', 'Tilt', 'PAR')) %>% 
  ggplot(aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter), cex = 0.2) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  ggtitle(label = 'MKY_AMB5 (Slade Island)') +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = 'output/Mackay 2021-03/plots/MKY_AMB5_MS9.png',
       plot = MKY_AMB5_MS9_plot,
       width = 21.2, 
       height = 15.9,
       units = 'cm')



MKY_AMB10.MS9 <- read_delim(file = 'data/Mackay 2021-03/MS9-LPT0047_LOG_0014.TXT',
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
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%OS", tz = 'GMT'),
         Timestamp_10min = floor_date(Timestamp, unit = "minute"))



MKY_AMB10.MS9.10min <- MKY_AMB10.MS9 %>% 
  group_by(Timestamp_10min) %>% 
  summarise(Depth = mean(Depth, na.rm = TRUE), 
            Temp = mean(TempW, na.rm = TRUE),
            Tilt = mean(Tilt, na.rm = TRUE),
            PAR = mean(PAR, na.rm = TRUE), 
            Channel_0 = mean(Channel_0, na.rm = TRUE),
            Channel_1 = mean(Channel_1, na.rm = TRUE),
            Channel_2 = mean(Channel_2, na.rm = TRUE),
            Channel_3 = mean(Channel_3, na.rm = TRUE),
            Channel_4 = mean(Channel_4, na.rm = TRUE),
            Channel_5 = mean(Channel_5, na.rm = TRUE),
            Channel_6 = mean(Channel_6, na.rm = TRUE),
            Channel_7 = mean(Channel_7, na.rm = TRUE),
            Channel_8 = mean(Channel_8, na.rm = TRUE),
            .groups = 'drop')


MKY_AMB10.MS9.long <- MKY_AMB10.MS9.10min %>% 
  pivot_longer(cols = -Timestamp_10min,
               names_to = 'parameter', 
               values_to = 'value')

MKY_AMB10_MS9_plot <- MKY_AMB10.MS9.long  %>% 
  filter(Timestamp_10min >= as_datetime("2021-01-26 16:00:00") &
           Timestamp_10min <= as_datetime("2021-03-09 09:00:00")) %>% 
  filter(parameter %in% c('Depth', 'Temp', 'Tilt', 'PAR')) %>% 
  ggplot(aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter), cex = 0.2) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  ggtitle(label = 'MKY_AMB10 (Victor Island)') +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = 'output/Mackay 2021-03/plots/MKY_AMB10_MS9.png',
       plot = MKY_AMB10_MS9_plot,
       width = 21.2, 
       height = 15.9,
       units = 'cm')

