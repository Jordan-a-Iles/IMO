# Script name: Data import
# Purpose: Import and process NTU logfiles 

# Author: Jordan Iles
# Date created:  2020-10-15
# Date modified: 2021-08-03
# Email: Jordan.iles@jcu.edu.au


#### Prepare the workspace ----
# rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)

rm(list = ls())

#### USER INPUT ----
filepath <- 'data/Bowen 2021-05/'
outpath <- 'output/Bowen 2021-05/'
outplot <- paste0(outpath, 'plots/')
files <- list.files(path = filepath)

filename <- select.list(choices = files, 
                    title = "Select NTU logfile:",
                    graphics = TRUE)
cat("filename = ", filename)

site_code <- select.list(choices = c("AP_AMB1", "AP_AMB4", "AP_AMB5",
                                     "MKY_AMB1", "MKY_AMB3B", "MKY_AMB5", "MKY_AMB10", "MKY_CAM1",
                                     "WP_AMB1", "WP_AMB2", "WP_AMB4"),
                         title = paste0("Select site_code for ", filename),
                         graphics = TRUE)
cat('site_code = ', site_code)

t_start <- as_datetime(readline(prompt = "Deployment start (YYYY-MM-DD HH:MM:SS):"))
cat('Deployment start = ', as.character(t_start))

t_end <- as_datetime(readline(prompt = "Deployment end (YYYY-MM-DD HH:MM:SS):"))
cat('Deployment end = ', as.character(t_end))

cat(paste0("--- LOGFILE SUMMARY---", '\n', 
           "file: ", filepath, filename, '\n', 
           "site_code: ", site_code, '\n',
           "deployment start:", t_start, '\n',
           "deployment end: ", t_end, '\n',
           "deployment duration: ", t_end - t_start), 'days')


#### Import NTU logfile ----
NTUlog <- read_delim(file = paste(filepath, filename, sep = '/'),
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

NTUlog.10min <- NTUlog %>% 
  filter(Timestamp_10min >= t_start + minutes(30) &
           Timestamp_10min <= t_end - minutes(30)) %>%
  group_by(Timestamp_10min) %>% 
  summarise(Voltage = mean(Voltage, na.rm = TRUE),
            DepthW = mean(Depth, na.rm = TRUE),
            TempW = mean(TempW, na.rm = TRUE),
            Tilt = mean(Tilt, na.rm = TRUE),
            COUNTS = mean(COUNTS, na.rm = TRUE),
            NTU = mean(NTU, na.rm = TRUE),
            .groups = 'drop') %>% 
  mutate(site_code = site_code,
         file = paste0(filepath, filename)) 


#### calculate RMS values for each 10 min interval
#### below is using just the first 10 readings but should be expanded to all 50 
NTUlog.RMS <- NTUlog %>% 
  filter(Timestamp_10min >= t_start + minutes(30) &
           Timestamp_10min <= t_end - minutes(30)) %>%
  select(Timestamp, Timestamp_10min, Depth) %>% 
  group_by(Timestamp_10min) %>% 
  mutate(burst_id = paste0('v', 1:n())) %>% 
  pivot_wider(id_cols = Timestamp_10min,
              names_from = burst_id,
              values_from = Depth)  

NTUlog.RMS.10min <- NTUlog.RMS %>% 
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


NTUlog.10min <- left_join(x = NTUlog.10min, 
                          y = NTUlog.RMS.10min, 
                          by = "Timestamp_10min") %>% 
  relocate(site_code, Timestamp_10min) %>% 
  relocate(file, .after = last_col())


NTUlog.long <- NTUlog.10min %>% 
  pivot_longer(cols = c(-site_code, -Timestamp_10min, -file),
               names_to = 'parameter', 
               values_to = 'value')


NTUlog_plot <- NTUlog.long  %>% 
  filter(!parameter == 'COUNTS') %>% 
  ggplot(aes(x = Timestamp_10min, y = value)) +
  geom_line(aes(col = parameter), cex = 0.2) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  ggtitle(label = site_code) +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = paste0(outplot, site_code, '_rawNTU_', as_date(t_start), '.png'),
       plot = NTUlog_plot,
       width = 21.2, 
       height = 15.9,
       units = 'cm')

write_excel_csv(x = NTUlog.10min,
                path = paste0(outpath, site_code, '_rawNTU_', as_date(t_start), '.csv'))

#### END ----



