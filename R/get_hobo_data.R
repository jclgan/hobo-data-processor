#' Read in HOBO data
#' 
#' 
#' 

get_hobo_data <- function(path_to_data, logger_type, select_station = "all", scale = "hour"){
  
  library(tidyverse)
  
  file_list <- list.files(path = path_to_data, recursive = FALSE, full.names = TRUE, pattern = "\\.csv$")
  
  data_list <- list()
  
  for (file in file_list) {
    print(paste("Reading", basename(file)))
  
    dat <- suppressWarnings(suppressMessages(read_csv(file)))

    dat <- dat %>%
      mutate(site_station_code = as.factor(site_station_code),
             timestamp = as.POSIXct(timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
             logger_sn = as.factor(logger_sn),
             across(starts_with("ref_"), as.numeric),
             across(any_of("baro_sn"), as.factor),
             across(contains("_qaqc_code"), as.factor),
             across(contains("_qaqc_adj"), as.factor),
             across(contains("_qaqc_note"), as.character)
             )

    data_list[[file]] <- dat
  }
  
  data <- bind_rows(data_list)
  
  # Filter by station if specified
  if(select_station != "all"){
    data <- data %>% 
      filter(site_station_code == select_station)
  }
  
  # Summarize by time scale if not hourly
  if(scale != "hour"){
    data$time_group <- NA
    if(scale == "day"){
      data$time_group <- date(data$timestamp)
    }
    if(scale == "week"){
      data$time_group <- week(data$timestamp)
    }
    if(scale == "month"){
      data$time_group <- month(data$timestamp)
    }
    if(scale == "year"){
      data$time_group <- year(data$timestamp)
    }
    data <- data %>% 
      group_by(site_station_code, time_group)
    
    if(logger_type == "U20_baro"){
      data <- data %>% 
        summarize(mean_airpress_kPa = mean(airpress_kPa_U20, na.rm = TRUE),
                  mean_airtemp_C = mean(airtemp_C_U20_adj, na.rm = TRUE),
                  min_airpress_kPa = min(airpress_kPa_U20, na.rm = TRUE),
                  min_airtemp_C = min(airtemp_C_U20_adj, na.rm = TRUE),
                  max_airpress_kPa = max(airpress_kPa_U20, na.rm = TRUE),
                  max_airtemp_C = max(airtemp_C_U20_adj, na.rm = TRUE))
    }
    if(logger_type == "U20_wl"){
      data <- data %>% 
        summarize(mean_waterlevel_m = mean(waterlevel_m_U20_adj, na.rm = TRUE),
                  mean_watertemp_C = mean(watertemp_C_U20_adj, na.rm = TRUE),
                  max_waterlevel_m = max(waterlevel_m_U20_adj, na.rm = TRUE),
                  max_watertemp_C = max(watertemp_C_U20_adj, na.rm = TRUE),
                  min_waterlevel_m = min(waterlevel_m_U20_adj, na.rm = TRUE),
                  min_watertemp_C = min(watertemp_C_U20_adj, na.rm = TRUE))
    }
    if(logger_type == "U26"){
      data <- data %>% 
        summarize(mean_DO_mgL = mean(DO_mgL_U26_adj, na.rm = TRUE),
                  mean_DO_percsat = mean(DO_percsat_U26_adj, na.rm = TRUE),
                  mean_watertemp_C = mean(watertemp_C_U26_adj, na.rm = TRUE),
                  min_DO_mgL = min(DO_mgL_U26_adj, na.rm = TRUE),
                  min_DO_percsat = min(DO_percsat_U26_adj, na.rm = TRUE),
                  min_watertemp_C = min(watertemp_C_U26_adj, na.rm = TRUE),
                  max_DO_mgL = max(DO_mgL_U26_adj, na.rm = TRUE),
                  max_DO_percsat = max(DO_percsat_U26_adj, na.rm = TRUE),
                  max_watertemp_C = max(watertemp_C_U26_adj, na.rm = TRUE))
    }
    if(logger_type == "U24"){
      data <- data %>% 
        summarize(mean_conduct_uScm = mean(conduct_uScm_U24_adj, na.rm = TRUE),
                  mean_spc_uScm = mean(spc_uScm_adj, na.rm = TRUE),
                  mean_watertemp_C = mean(watertemp_C_U24_adj, na.rm = TRUE),
                  max_conduct_uScm = max(conduct_uScm_U24_adj, na.rm = TRUE),
                  max_spc_uScm = max(spc_uScm_adj, na.rm = TRUE),
                  max_watertemp_C = max(watertemp_C_U24_adj, na.rm = TRUE),
                  min_conduct_uScm = min(conduct_uScm_U24_adj, na.rm = TRUE),
                  min_spc_uScm = min(spc_uScm_adj, na.rm = TRUE),
                  min_watertemp_C = min(watertemp_C_U24_adj, na.rm = TRUE))
    }
    
    if(scale == "day") {
      colnames(data)[colnames(data) == 'time_group'] <- 'date'
      
    }
    if(scale == "week") {
      colnames(data)[colnames(data) == 'time_group'] <- 'week'
    }
    if(scale == "month") {
      colnames(data)[colnames(data) == 'time_group'] <- 'month'
    }
    if(scale == "year") {
      colnames(data)[colnames(data) == 'time_group'] <- 'year'
    }
    
    data <- data %>% mutate(across(where(is.numeric), ~round(., 2)))
  } # end of temporal summary

  return(data)
}
