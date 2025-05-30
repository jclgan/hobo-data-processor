#' Bind HOBO Files
#'
#' Trims raw CSV output from HOBO data loggers to the deployment window defined in a metadata file,
#' then binds the trimmed files into a single data frame for further processing.
#'
#' @param raw_path File path. Folder containing the raw CSV files exported from HOBO loggers.
#' @param out_path File path. Folder where the bound CSV of all processed sites will be saved.
#' @param meta_file File path. Path to the metadata CSV file containing logger deployment details. 
#'   Must include the columns: \code{sn}, \code{site_station_code}, \code{site_type}, 
#'   \code{timestamp_deploy}, and \code{timestamp_remove}. 
#'   The \code{site_station_code} column must follow the format: 
#'   \code{<site_name>_<site_type>_<station_num>} (e.g., \code{"DEAD_ST_10"}), with all components as character strings separated by underscores.
#' @param project_code Character. Short code used to prefix the filename of the bound data (e.g., \code{"DM"}).
#' @param logger_type Character. Type of logger used. Must be one of: \code{"U20_wl"}, \code{"U20_baro"}, \code{"U24_cond"}, or \code{"U26_do"}.
#'
#' @return A data frame of combined logger data, with a CSV file also written to \code{out_path}.
#' @export
#'
#' @examples
#' all_WL <- bind_hobo_files(
#'   raw_path = here("Data", "water-level", "ST", "2025", "raw"),
#'   out_path = here("Data", "water-level", "ST", "2025", "intermediate"),
#'   meta_file = here("Data", "site-attribute", "Deadman_logger_deployments_20250411.csv"),
#'   project_code = "DM",
#'   logger_type = "U20_wl"
#' )

bind_hobo_files <- function(raw_path, out_path, project_code, meta_file, logger_type) {
  library(tidyverse)
  
  # Read in the logger deployment file and check for column requirements
  metadat <- read_csv(meta_file)
  
  ## Check for required columns
  required_cols <- c("sn", "site_station_code", "timestamp_deploy", "timestamp_remove")
  missing_cols <- setdiff(required_cols, names(metadat))
  if (length(missing_cols) > 0) {
    stop("The metadata file is missing the following required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  ## Check for site_station_code format
  valid_ssc <- grepl("^[^_]+_[^_]+_[^_]+$", metadat$site_station_code)
  if (any(!valid_ssc)) {
    bad_ssc <- unique(metadat$site_station_code[!valid_ssc])
    stop("Invalid format in 'site_station_code' column. Expected format is 'siteName_siteType_stationNum'. Problematic entries: ",
         paste(bad_ssc, collapse = ", "))
  }
  
  ## Format timestamps and create aux columns
  metadat <- metadat %>% 
    mutate(timestamp_deploy = as.POSIXct(timestamp_deploy, format = "%m/%d/%Y %H:%M", tz = "UTC"),
           timestamp_remove = as.POSIXct(timestamp_remove, format = "%m/%d/%Y %H:%M", tz = "UTC"),
           sn = as.character(sn)) %>% 
    separate(site_station_code, into = c("site_name", "site_type", "station_num"), sep = "_", remove = FALSE)
  
  # Read in the raw logger data files
  ## Get all csv files in the raw data directory
  file_list <- list.files(path = raw_path, recursive = FALSE, full.names = TRUE, pattern = "\\.csv$") # extract file names ending in csv
  
  if (length(file_list) == 0) {
    stop("Folder is empty or nonexistent. Check the folder path is correct.")
  }
  
  all_data <- data.frame()
  
  ## Loop through each csv file
  for (file in file_list) {
    data <- read.csv(file, skip = 1, header = TRUE, stringsAsFactors = FALSE)
    
    # Extract the logger serial number from the third column header
    data$sn <- str_extract(names(data)[3], "(?<=SEN\\.S\\.N\\.\\.)\\d+")
    
    # For U24 conductivity loggers
    # If the data have both Low Range and Full Range columns, drop the Full Range column
    if (any(grepl("Low.Range", names(data))) && any(grepl("Full.Range", names(data)))) {
      data <- data %>% 
        select(-matches("Full.Range"))
    }
    
    data <- data %>% 
      select(2:4, sn)
    
    # Rename the columns based on the parameter as stated by the column 2 header
    read_col <- colnames(data)[2]
    
    if (logger_type == "U20_wl") {
      colnames(data) <- c("timestamp", "waterpress_kPa_U20", "watertemp_C_U20", "sn")
    } else if (logger_type == "U20_baro") {
      colnames(data) <- c("timestamp", "airpress_kPa_U20", "airtemp_C_U20", "sn")
    } else if (logger_type == "U26_do") {
      colnames(data) <- c("timestamp", "DO_mgL_U26", "watertemp_C_U26", "sn")
    } else if (logger_type == "U24_cond") {
      colnames(data) <- c("timestamp", "conduct_uScm_U24", "watertemp_C_U24", "sn")
    } else {
      stop("Invalid logger type. Make sure to specify 'U20_wl', 'U20_baro', 'U24_cond', or 'U26_do' to properly format column headers")
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
  
    logger_start <- min(data$timestamp, na.rm = TRUE)
    logger_end <- max(data$timestamp, na.rm = TRUE)
    current_sn <- unique(data$sn)
    last_data_time <- max(data$timestamp)
    
    # Create unique rows of logger-site pairs
    logger_site <- metadat %>% 
      filter(sn == current_sn, last_data_time >= timestamp_deploy) %>% 
      distinct(site_station_code, sn, .keep_all = TRUE)
    
    # If multiple rows match, keep the one with the latest deploy time
    logger_site <- logger_site %>%
      arrange(desc(timestamp_deploy)) %>%
      slice(1)
    
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
  site_type <- unique(all_data$site_type)
  param <- unique(data$parameter)
  
  years <- sort(unique(year(all_data$timestamp)))
  if (length(years) == 1) {
    year <- as.character(years)
  } else {
    year <- paste0(min(years), "-", max(years))
  }
  
  if(logger_type == "U20_baro") {
    filename <- paste0(project_code, "_all_", year, "_", param, "_", "raw", ".csv")
  }
  
  else {
    filename <- paste0(project_code, "_all_", site_type, "_", year, "_", param, "_", "raw", ".csv")
  }
  
  if (!dir.exists(out_path)) {
    stop("Output folder does not exist. Check the specified path is correct.")
  }
  
  write_csv(all_data, file.path(out_path, filename))
  
  print(paste("Writing to csv", filename))
  
  # Return the final data frame
  return(all_data)
}