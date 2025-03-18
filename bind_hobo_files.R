#' Bind the raw csv output of Hobo data logger files
#' 
#' @param raw_path Folder containing the raw csv files
#' @param out_path Folder where you want the bound csv of all sites to be saved
#' @param meta_file Filepath of the logger deployment csv file
#' @param logger_type Specify "U20_waterlevel", "U20_baro", "U24_conductivity", or "U26_dissox"

bind_hobo_files <- function(raw_path, out_path, meta_file, logger_type) {
  library(tidyverse)
  
  # Read in the logger deployment file
  metadat <- read_csv(meta_file) %>% 
    mutate(timestamp_deploy = as.POSIXct(timestamp_deploy, format = "%m/%d/%Y %H:%M", tz = "UTC"),
           timestamp_remove = as.POSIXct(timestamp_remove, format = "%m/%d/%Y %H:%M", tz = "UTC"),
           sn = as.character(sn)) %>% 
    separate(site_station_code, into = c("site_name", "site_type", "station_num"), sep = "_", remove = FALSE)
  
  ## Read in the raw logger data files
  # Get all csv files in the raw data directory
  file_list <- list.files(path = raw_path, recursive = FALSE, full.names = TRUE, pattern = "\\.csv$") # extract file names ending in csv
  
  all_data <- data.frame()
  
  # Loop through each csv file
  for (file in file_list) {
    data <- read.csv(file, skip = 1, header = TRUE, stringsAsFactors = FALSE)
    
    # Extract the logger serial number from the third column header
    data$sn <- str_extract(names(data)[3], "(?<=SEN\\.S\\.N\\.\\.)\\d+")
    
    data <- data %>% 
      select(2:4, sn)
    
    # Rename the columns based on the parameter as stated by the column 2 header
    read_col <- colnames(data)[2]
    
    if (logger_type == "U20_waterlevel") {
      colnames(data) <- c("timestamp", "waterpress_kPa_U20", "watertemp_C_U20", "sn")
    } else if (logger_type == "U20_baro") {
      colnames(data) <- c("timestamp", "airpress_kPa_U20", "airtemp_C_U20", "sn")
    } else if (logger_type == "U26_dissox") {
      colnames(data) <- c("timestamp", "DO_mgL_U26", "watertemp_C_U26", "sn")
    } else if (logger_type == "U24_conductivity") {
      colnames(data) <- c("timestamp", "conduct_uScm_U24", "watertemp_C_U24", "sn")
    } else {
      print("Invalid logger type. Make sure to specify 'U20_waterlevel', 'U20_baro', 'U24_conductivity', or 'U26_dissox' to properly format column headers")
      stop()
    }
    
    # Remove rows with missing values (from logger messages)
    data <- data[complete.cases(data), ]
    
    # Format timestamp
    # NOTE: all times are in PDT (setting timezone to UTC tricks R to ignore daylight savings time)
    data$timestamp <-as.POSIXct(data$timestamp, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
    
    if(any(is.na(data$timestamp))) {
      stop("NAs produced in timestamp. Make sure the timestamp column in all csvs are formatted as mm/dd/yyyy hh:mm:ss")
    }
      # warning('NAs produced in timestamp. Make sure the timestamp column in all csvs are formatted as mm/dd/yyyy hh:mm:ss')
  
    # Create unique rows of logger-site pairs
    logger_site <- metadat %>% 
      filter(sn == data$sn[1]) %>% 
      distinct(site_station_code, sn, .keep_all = TRUE)
    
    # Join the site & logger metadata
    data <- data %>% 
      inner_join(select(logger_site,
                       sn,
                       parameter = metric,
                       site_station_code,
                       site_type),
                by = "sn")
    
    # Identify the site & logger sn in the metadata file
    meta_match <- metadat %>% 
      filter(site_station_code == unique(data$site_station_code) & sn == unique(data$sn))
    
    print(paste("Processing", "logger", unique(data$sn), "from station", unique(data$site_station_code)))
    
    # Trim the logger data to timestamps between deployment (+ 15 minutes to account for disturbance) and download
    if (last(meta_match$status) == "removed" & is.na(last(meta_match$timestamp_remove))) {
      stop("Check logger deployment log. Status is 'removed' but removal timestamp is missing.")
    } else {
      data_trimmed <- data %>% 
        filter(
          timestamp > (first(meta_match$timestamp_deploy) + 15*60),
          if (last(meta_match$status) == "logging") TRUE 
          else timestamp <= last(meta_match$timestamp_remove)
        )
    }

    # Append the data to the all_data data frame
    all_data <- rbind(all_data, data_trimmed)
  }
  
  # Write the data to a csv
  current_date <- Sys.Date()
  site_type <- all_data$site_type[1]
  year <- year(all_data$timestamp[1])
  param <- data$parameter[1]
  filename <- paste0("DM_", site_type, "_", year, "_", param, "_", "all_RAW", ".csv")
  
  write_csv(all_data, file.path(out_path, filename))
  
  print(paste("Writing to csv", filename))
  
  # Return the final data frame
  return(all_data)
}


# baro_press <- bind_hobo_files(raw_path = here("Data", "barometric-pressure", "2024", "raw"),
#                               out_path = here("Data", "barometric-pressure", "2024", "intermediate"),
#                               meta_file = here("Data", "site-attribute", "Deadman_logger_deployments_20241113.csv"),
#                               site_file = here("Data", "site-attribute", "Deadman_sites_20241112.csv"))
# 
# waterlev_ST <- bind_hobo_files(raw_path = here("Data", "water-level", "2024", "raw", "ST"),
#                                out_path = here("Data", "water-level", "2024", "intermediate"),
#                                meta_file = here("Data", "site-attribute", "Deadman_logger_deployments_20241113.csv"),
#                                site_file = here("Data", "site-attribute", "Deadman_sites_20241112.csv"))
# 
# waterlev_OC <- bind_hobo_files(raw_path = here("Data", "water-level", "2024", "raw", "OC"),
#                                out_path = here("Data", "water-level", "2024", "intermediate"),
#                                meta_file = here("Data", "site-attribute", "Deadman_logger_deployments_20241113.csv"),
#                                site_file = here("Data", "site-attribute", "Deadman_sites_20241112.csv"))
# 
# DO_ST <- bind_hobo_files(raw_path = here("Data", "dissolved-oxygen", "2024", "raw", "ST"),
#                          out_path = here("Data", "dissolved-oxygen", "2024", "intermediate"),
#                          meta_file = here("Data", "site-attribute", "Deadman_logger_deployments_20241113.csv"),
#                          site_file = here("Data", "site-attribute", "Deadman_sites_20241112.csv"))
# 
# DO_OC <- bind_hobo_files(raw_path = here("Data", "dissolved-oxygen", "2024", "raw", "OC"),
#                          out_path = here("Data", "dissolved-oxygen", "2024", "intermediate"),
#                          meta_file = here("Data", "site-attribute", "Deadman_logger_deployments_20241113.csv"),
#                          site_file = here("Data", "site-attribute", "Deadman_sites_20241112.csv"))
# 
# conductivity <- bind_hobo_files(raw_path = here("Data", "conductivity", "2024", "raw"),
#                                 out_path = here("Data", "conductivity", "2024", "intermediate"),
#                                 meta_file = here("Data", "site-attribute", "Deadman_logger_deployments_20241113.csv"),
#                                 site_file = here("Data", "site-attribute", "Deadman_sites_20241112.csv"))



