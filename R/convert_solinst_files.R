#' Convert raw csv output from Solinst Level Loggers to match the csv output from HOBO U20 water level loggers
#' 
#' Make sure that the raw output from the Solinst file uses kPa for level
#' 
#' @param input_folder The folder containing the Solinst csv files
#' @param output_folder The desired folder for the converted files (should be with the other related Hobo files)
#' 
#' Written 2024/11/13 by Julian Gan 

convert_solinst_files <- function(input_folder, output_folder) {
  library(dplyr)
  library(readr)
  library(lubridate)
  
  # Get a list of all CSV files in the input folder
  files <- list.files(input_folder, pattern = "*.csv", full.names = TRUE)
  
  # Process each csv file individually
  solinst_to_hobo <- function(file) {
    # Extract the serial number from the raw data file
    suppressWarnings({
      dat_sn <- read_csv(file)
    })
    
    sn <- data.frame(dat_sn)[1,1]
    
    # Trim all metadata, preserving original row headers
    dat <- read_csv(file, skip = 13)
    
    if (!inherits(dat$Date, "Date")) {
      dat$Date <- as.Date(parse_date_time(dat$Date, orders = c("mdy", "ymd", "m/d/Y", "Y-m-d")))
    }
    
    dat <- dat %>% 
      mutate(`#` = row_number(),
             Date = format(Date, "%m/%d/%y"),
             Time = format(format(strptime(Time, format = "%H:%M"), "%I:%M:%S %p")),
             `Date Time`= paste(Date, Time)
      ) %>% 
      select(`#`,
             `Date Time`,
             LEVEL,
             TEMPERATURE) %>% 
      rename(setNames("LEVEL", paste0("Abs Pres, kPa (LGR S/N: ", sn, ", SEN S/N: ", sn, ")")),
             setNames("TEMPERATURE", paste0("Temp, C (LGR S/N: ", sn, ", SEN S/N: ", sn, ")")))
    
    # Write the processed data to the output folder
    new_filename <- sub("_solinst", "", basename(file))
    output_file <- file.path(output_folder, new_filename)
    
    # Writes a line of text before the data table so it mimics the format of the Hobo logger output
    temp_file <- tempfile()
    
    cat(paste("Converted Solinst", sn, Sys.Date()), file = temp_file, sep = "\n")
    
    write.table(dat, temp_file, sep = ",", row.names = FALSE, append = TRUE)
    
    file.rename(temp_file, output_file)
    
    print(paste("Converted", file))
  }
  
  # Apply the function to each file
  lapply(files, solinst_to_hobo)
}

convert_solinst_files(input_folder = here("Data", "water-level", "raw", "OC", "Solinst", "unprocessed"),
                      output_folder = here("Data", "water-level", "raw", "OC"))
