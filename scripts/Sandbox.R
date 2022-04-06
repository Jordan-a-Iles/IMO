# Script name: Wave height
# Purpose: Import NTU and MS9 logfiles
# 
# Author: Jordan Iles
# Date created:  2020-10-15
# Email: Jordan.iles@jcu.edu.au


#### Prepare the workspace ----
# rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)
library(oceanwaves)


files <- list.files(path = 'data/Bowen 2020-12')

# AP_AMB1 (Euri Creek) = "NTU-LPT0051_LOG_0010.TXT"
# AP_AMB4 (Camp Island) = "NTU-LPT0050_LOG_0004.TXT" 
# AP_AMB5 (Holbourne Island) = "NTU-LPT0049_LOG_0021.TXT" 

#### Import NTU logfile ----
NTU.log <- read_delim(file = 'data/Bowen 2020-12/NTU-LPT0051_LOG_0010.TXT',
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


NTU.log <- NTU.log %>% 
  mutate(Depth_Corr = prCorr(pt = Depth, Fs = 5, zpt = 0.3, M = 50))

# plot(NTU.log$Depth_Corr, type = 'l', col = 'red', 
#      ylab = 'Surface elevation, m')
# lines(NTU.log$Depth, col = 'blue')
# legend('topright',legend=c('Corrected','Raw'), col = c('red','blue'),
#        lty = 1)
# 


#### Oceanwaves package ----
testy <- NTU.log %>% 
  # filter(Timestamp_10min >= '2020-12-06 12:00' &
  #          Timestamp_10min <= '2020-12-06 12:30') %>%
  mutate(Burst_timestamp = factor(Timestamp_10min)) %>% 
  select(Timestamp_10min, Burst_timestamp, Depth_Corr)

burst <- NULL
waves <- NULL
waves.comp <- NULL
for (i in 1:length(levels(testy$Burst_timestamp))){
  tryCatch({
    burst <- testy %>% 
      filter(Burst_timestamp == levels(testy$Burst_timestamp)[i]) 
    waves <- unlist(waveStatsZC(burst$Depth_Corr, Fs = 5))
    waves$Timestamp_10min <- as_datetime(levels(testy$Burst_timestamp)[i])
  }, error = function(e){})
  waves.comp = bind_rows(waves.comp, waves)  
}

new <- waves.comp %>% 
  tibble() %>% 
  mutate(TS = as_datetime(Timestamp_10min)) %>% 
  pivot_longer(cols = c(Hsig, Hmean, H10, Hmax, Tmean, Tsig),
               names_to = 'parameter', 
               values_to = 'value')

wave_height_plot <- new %>% 
  filter(parameter %in% c('Hsig', 'Hmean', 'H10', 'Hmax')) %>% 
  ggplot(aes(x = TS, y = value)) +
  geom_line(aes(col = parameter), size = 0.2) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  scale_x_datetime(date_labels = '%d-%m-%Y', date_breaks = '7 days') +
  xlab(NULL) +
  ylab('Wave height (m)') +
  theme_bw()

ggsave(filename = 'output/plots/wave_height.png',
       plot = wave_height_plot,
       width = 15.9, 
       height = 15.9, 
       units = 'cm')


wave_period_plot <- new %>% 
  filter(parameter %in% c('Tsig', 'Tmean')) %>% 
  ggplot(aes(x = TS, y = value)) +
  geom_line(aes(col = parameter), size = 0.2) +
  facet_grid(parameter ~ .) +
  scale_x_datetime(date_labels = '%d-%m-%Y', date_breaks = '7 days') +
  xlab(NULL) +
  ylab('Wave period (s)') +
  theme_bw()

ggsave(filename = 'output/plots/wave_period.png',
       plot = wave_period_plot,
       width = 15.9, 
       height = 10.6, 
       units = 'cm')



#### calculate RMS values for each 10 min interval ----
#### below is using just the first 10 readings but should be expanded to all 50 
RMS <- NTU.log %>% 
  select(Timestamp, Timestamp_10min, Depth) %>% 
  group_by(Timestamp_10min) %>% 
  mutate(burst_id = paste0('v', 1:n())) %>% 
  pivot_wider(id_cols = Timestamp_10min,
              names_from = burst_id,
              values_from = Depth) 

RMS10 <- RMS %>% 
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

#### resample the 5hz pressure data to 1 hz so as to compare to MGL...
RMS10rs <- RMS %>% 
  mutate(Depth = mean(v1:v10, na.rm = TRUE), 
         RMS = ((((v1 - Depth)^2) + 
                   ((v6 - Depth)^2) + 
                   ((v11 - Depth)^2) + 
                   ((v16 - Depth)^2) + 
                   ((v21 - Depth)^2) + 
                   ((v26 - Depth)^2) + 
                   ((v31 - Depth)^2) +
                   ((v36 - Depth)^2) + 
                   ((v41 - Depth)^2) + 
                   ((v46 - Depth)^2))/10)^0.5) 


RMS10.long <- RMS10 %>% 
  select(Timestamp_10min, Depth, RMS) %>% 
  pivot_longer(cols = c(Depth, RMS),
               names_to = 'parameter', 
               values_to = 'value')

ggplot(RMS10.long, aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  theme_bw(base_size = 16)


RMS50 <- RMS %>% 
  mutate(Depth = mean(v1:v50, na.rm = TRUE), 
         RMS = ((((v1 - Depth)^2) + 
                   ((v2 - Depth)^2) + 
                   ((v3 - Depth)^2) + 
                   ((v4 - Depth)^2) + 
                   ((v5 - Depth)^2) + 
                   ((v6 - Depth)^2) + 
                   ((v7 - Depth)^2) +
                   ((v8 - Depth)^2) + 
                   ((v9 - Depth)^2) + 
                   ((v10 - Depth)^2) + 
                   ((v11 - Depth)^2) + 
                   ((v12 - Depth)^2) + 
                   ((v13 - Depth)^2) + 
                   ((v14 - Depth)^2) + 
                   ((v15 - Depth)^2) +
                   ((v16 - Depth)^2) + 
                   ((v17 - Depth)^2) + 
                   ((v18 - Depth)^2) + 
                   ((v19 - Depth)^2) + 
                   ((v20 - Depth)^2) + 
                   ((v21 - Depth)^2) + 
                   ((v22 - Depth)^2) + 
                   ((v23 - Depth)^2) +
                   ((v24 - Depth)^2) + 
                   ((v25 - Depth)^2) + 
                   ((v26 - Depth)^2) + 
                   ((v27 - Depth)^2) + 
                   ((v28 - Depth)^2) + 
                   ((v29 - Depth)^2) + 
                   ((v30 - Depth)^2) + 
                   ((v31 - Depth)^2) +
                   ((v32 - Depth)^2) + 
                   ((v33 - Depth)^2) + 
                   ((v34 - Depth)^2) + 
                   ((v35 - Depth)^2) + 
                   ((v36 - Depth)^2) + 
                   ((v37 - Depth)^2) + 
                   ((v38 - Depth)^2) + 
                   ((v39 - Depth)^2) +
                   ((v40 - Depth)^2) + 
                   ((v41 - Depth)^2) + 
                   ((v42 - Depth)^2) + 
                   ((v43 - Depth)^2) + 
                   ((v44 - Depth)^2) + 
                   ((v45 - Depth)^2) + 
                   ((v46 - Depth)^2) + 
                   ((v47 - Depth)^2) + 
                   ((v48 - Depth)^2) + 
                   ((v49 - Depth)^2) +
                   ((v50 - Depth)^2))/50)^0.5) 


RMS50.long <- RMS50 %>% 
  select(Timestamp_10min, Depth, RMS) %>% 
  pivot_longer(cols = c(Depth, RMS),
               names_to = 'parameter', 
               values_to = 'value')

ggplot(RMS50.long, aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  theme_bw(base_size = 16)

RMS.comparison <- left_join(x = RMS10.long, 
                            y = RMS50.long, 
                            by = Timestamp_10min)




RMS50 <- RMS50 %>% 
  select(Timestamp_10min, RMS) %>% 
  rename('RMS50' = RMS)

RMS10 <- RMS10 %>% 
  select(Timestamp_10min, RMS) %>% 
  rename('RMS10' = RMS)

RMS10rs <- RMS10rs %>% 
  select(Timestamp_10min, RMS) %>% 
  rename('RMS10rs' = RMS)

RMS.comp <- full_join(RMS10, RMS50, by = 'Timestamp_10min') %>% 
  pivot_longer(cols = c('RMS10', 'RMS50'),
               names_to = 'parameter',
               values_to = 'RMS')

ggplot(RMS.comp, aes(x = Timestamp_10min, y = RMS)) +
  geom_line(aes(col = parameter)) +
  facet_grid(parameter ~ .) +
  theme_bw()


RMS.comp2 <- full_join(RMS10, RMS50, by = 'Timestamp_10min') 

ggplot(RMS.comp2, aes(x = RMS10, y = RMS50)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, col = 'red') +
  theme_bw()
  


grr <- left_join(waves.comp, RMS50, by = 'Timestamp_10min') %>% 
  left_join(., RMS10, by = 'Timestamp_10min') %>% 
  left_join(., RMS10rs, by = 'Timestamp_10min') %>% 
  mutate(RMSdiff = RMS50 - RMS10rs)

grr.long <- grr %>% 
  pivot_longer(cols = c('Hsig', 'Hmean', 'H10', 'Hmax', 'Tmean', 'Tsig', 'RMS50', 'RMS10', 'RMS10rs'),
                        names_to = 'parameter', 
                        values_to = 'value')

p1 <- grr.long  %>% 
  filter(parameter %in% c('Hsig', 'RMS50', 'RMS10', 'RMS10rs')) %>% 
  ggplot(aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_grid(parameter ~ .) +
  theme_bw()

p2 <- grr.long  %>% 
  filter(parameter %in% c('RMS10rs', 'RMS10', 'RMS50')) %>% 
  ggplot(aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter), size = 0.2) +
  facet_grid(parameter ~ .) +
  xlab(NULL) +
  ylab('RMS water depth (m)') +
  scale_x_datetime(date_labels = '%d-%m-%Y', date_breaks = '7 days') +
  theme_bw()

ggsave(filename = 'output/plots/IMO_RMS.png',
       plot = p2, 
       width = 15.9, 
       height = 10.6,
       units = 'cm')
  
ggplot(grr, aes(x = Timestamp_10min, y = RMSdiff)) +
  geom_line()

tidal <- NTU.log %>% 
  filter(Timestamp_10min >= '2020-11-06 00:00' &
           Timestamp_10min <= '2020-12-14 23:50') %>%
  group_by(Timestamp_10min) %>% 
  summarise(Depth = mean(Depth, na.rm = TRUE),
            .groups = 'drop') %>% 
  mutate(Depth_mean = mean(Depth, na.rm = TRUE),
         Tidal_height = Depth - Depth_mean)
  
tidal_plot <- ggplot(tidal, aes(x = Timestamp_10min, y = Tidal_height)) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_line(col = 'blue') +
  # annotate('text', x = min(tidal$Timestamp_10min), y = 0.1,
  #          label = 'Mean Sea Level', col = 'red', hjust = 0) +
  xlab(NULL) + 
  ylab('Tidal height (m)') +
  scale_x_datetime(date_labels = '%d-%m-%Y', date_breaks = '7 days') +
  theme_linedraw() +
  theme(panel.grid = element_blank())

ggsave(filename = 'output/plots/IMO_tidal_height.png',
       plot = tidal_plot, 
       width = 15.9, 
       height = 7.95,
       units = 'cm')
