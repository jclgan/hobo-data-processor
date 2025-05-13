#' Add baro logger reading to water level & dissolved oxygen files
#' Iteratively adds matching baro logger data based on timestamp and closest geographic position
#' If the input file has data when there is missing baro logger data, the script will search for
#' the next closest logger.
#' 
#' @param input_data Filepath or data frame of the primary logger file to which the baro file will be joined
#' @param baro_file Filepath of the barometric logger data
#' @param site_file Filepath of the site file containing coordinates (in order to determine the location of the closets baro logger)
#' @param out_path Directory to which the compiled data frame will be saved
#' 
#' @@returns Data frame of the compiled logger file and saves a csv to the same directory as the input file

# library(here)
# input_file <- here("Data", "water-level", "2024", "intermediate", "DM_ST_2024_waterlevel_all_QAQC.csv")
# baro_file <- here("Data", "barometric-pressure", "2024", "intermediate", "DM_ST_2024_barometric_all_QAQC.csv")
# site_file <- here("Data", "site-attribute", "Deadman_sites_20241112.csv")

join_nearest_baro <- function(input_data, baro_file, site_file, select_station = "all", var_airpress_kPa = "airpress_kPa_U20", var_airtemp_C = "airtemp_C_U20") {
  
  library(tidyverse)
  library(geosphere)
  
  # Filter station if specified
  if(select_station != "all") {
    input_data <- input_data %>% 
      filter(site_station_code == select_station) }
  
  baro_dat <- suppressMessages(read_csv(baro_file))
  
  # user input baro data names to local names
  names(baro_dat)[names(baro_dat) == var_airpress_kPa ] <- "airpress_kPa"
  names(baro_dat)[names(baro_dat) == var_airtemp_C ] <- "airtemp_C"
  baro_dat$site_station_code <- as.character(baro_dat$site_station_code)
  
  # create QAQC columns if missing
  if (!("baro_qaqc_code" %in% names(baro_dat)) && !("baro_qaqc_note" %in% names(baro_dat))) {
    baro_dat$baro_qaqc_code <- NA
    baro_dat$baro_qaqc_note <- NA
  }
  
  sites <- suppressMessages(read_csv(site_file))
  if ("latitude" %in% names(sites)) names(sites)[names(sites) == "latitude"] <- "lat"
  if ("longitude" %in% names(sites)) names(sites)[names(sites) == "longitude"] <- "lon"
  sites <- sites %>% 
    select(site_station_code, lat, lon)
  
  # If measurements are taken more frequently than every hour, 
  # keep only the measurements taken at the top (or closest to) the hour
  dat <- input_data %>% 
    mutate(hour = floor_date(timestamp, "hour"),
           mins = abs(as.numeric(difftime(timestamp, hour, units = "mins")))) %>%
    group_by(site_station_code, hour) %>%
    slice_min(mins, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(timestamp = hour) %>%
    select(-hour, -mins)

  # Join sites to dat and baro_dat to get lat & long for each site
  dat <- left_join(dat, sites, by = "site_station_code")
  baro_dat <- left_join(baro_dat, sites, by = "site_station_code")
  
  # Get unique pairs of sites
  dat_sites <- unique(dat$site_station_code)
  baro_sites <- unique(baro_dat$site_station_code)
  
  ## Determine distance between loggers ----

  # Calculate distance between each pair of sites
  distances <- data.frame()
  for(i in dat_sites) {
    for(j in baro_sites) {
      dist <- distm(c(sites$lon[sites$site_station_code == i], sites$lat[sites$site_station_code == i]), 
                    c(sites$lon[sites$site_station_code == j], sites$lat[sites$site_station_code == j]))
      distances <- rbind(distances, data.frame(dat_site = i, 
                                               baro_site = j, 
                                               distance = dist))
    }
  }
  
  # Sort distances in ascending order
  distances <- distances %>% arrange(distance)
  
  # Get the absolute nearest (primary) baro site for each site in dat
  abs_closest_baro_sites <- distances %>% group_by(dat_site) %>% slice(which.min(distance))
  
  dat <- left_join(dat, abs_closest_baro_sites, by = c("site_station_code" = "dat_site")) %>% 
    rename(primary_baro = baro_site) %>% 
    select(-distance)
  
  print("Determining nearest baro loggers for each record (this may take a minute)...")
  
  # Create a progress bar
  pb <- txtProgressBar(min = 0, max = nrow(dat), style = 3)
  
  # Add nearest_baro column to dat
  dat$nearest_baro <- NA
  
  # Initialize variable to track current site
  current_site <- NA
  
  for(i in 1:nrow(dat)) {
    this_site <- dat$site_station_code[i]
    
    # Print message only when site changes
    if (!identical(this_site, current_site)) {
      cat("\nProcessing", this_site, "\n")
      current_site <- this_site
    }
    
    # Get the distances for the current site
    site_distances <- filter(distances, dat_site == dat$site_station_code[i])
    
    for(j in 1:nrow(site_distances)) {
      # Get the baro_dat rows for the current closest site and matching timestamp
      baro_rows <- filter(baro_dat, site_station_code == site_distances$baro_site[j], timestamp == dat$timestamp[i])
      
      # If there's a matching timestamp, set the nearest_baro and break the loop
      if(nrow(baro_rows) > 0) {
        dat$nearest_baro[i] <- site_distances$baro_site[j]
        break
      }
    }
    
    # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  
  # Close progress bar
  close(pb)
  
  
  ## Create linear models of each pairing of baro sites ----
  # To determine coefficients for extrapolating pressure & temperature data
  
  print("Extrapolating baro logger data...")
  
  baro_site_pairs <- expand.grid(unique(baro_dat$site_station_code), unique(baro_dat$site_station_code)) %>% 
    filter(Var1 != Var2)
  
  ### Air pressure  ----
  pres_coefficients <- data.frame(site1 = character(), site2 = character(), intercept = numeric(), slope = numeric())

  # For each pair of sites
  for(i in 1:nrow(baro_site_pairs)) {
    # Get the airpress_kPa values for the two sites
    site1_pdata <- baro_dat %>% 
      filter(site_station_code == baro_site_pairs$Var1[i]) %>% 
      select(timestamp,
             airpress.site1 = airpress_kPa)
    site2_pdata <- baro_dat %>% 
      filter(site_station_code == baro_site_pairs$Var2[i]) %>% 
      select(timestamp,
             airpress.site2 = airpress_kPa)
    
    # Join the data for the two sites on timestamp
    joined_pdata <- inner_join(site1_pdata, site2_pdata, by = "timestamp")
    
    # Skip if there's no matching timestamp
    if(nrow(joined_pdata) == 0) next
    
    # Create a simple linear model
    p_model <- lm(airpress.site1 ~ airpress.site2, data = joined_pdata)
    
    # Extract the coefficients and add them to the coefficients data frame
    pcoef <- coef(p_model)
    pres_coefficients <- rbind(pres_coefficients, data.frame(site1 = baro_site_pairs$Var1[i], 
                                                             site2 = baro_site_pairs$Var2[i], 
                                                             intercept = pcoef[1], 
                                                             slope = pcoef[2]))
  }
  
  ### Temperature  ----
  temp_coefficients <- data.frame(site1 = character(), site2 = character(), intercept = numeric(), slope = numeric())
  
  # For each pair of sites
  for(i in 1:nrow(baro_site_pairs)) {
    # Get the airtemp_C_U20_adj values for the two sites
    site1_tdata <- baro_dat %>% 
      filter(site_station_code == baro_site_pairs$Var1[i]) %>% 
      select(timestamp,
             airtemp.site1 = airtemp_C)
    site2_tdata <- baro_dat %>% 
      filter(site_station_code == baro_site_pairs$Var2[i]) %>% 
      select(timestamp,
             airtemp.site2 = airtemp_C)
    
    # Join the data for the two sites on timestamp
    joined_tdata <- inner_join(site1_tdata, site2_tdata, by = "timestamp")
    
    # Skip if there's no matching timestamp
    if(nrow(joined_tdata) == 0) next
    
    # Create a simple linear model
    t_model <- lm(airtemp.site1 ~ airtemp.site2, data = joined_tdata)
    
    # Extract the coefficients and add them to the coefficients data frame
    tcoef <- coef(t_model)
    temp_coefficients <- rbind(temp_coefficients, data.frame(site1 = baro_site_pairs$Var1[i], 
                                                             site2 = baro_site_pairs$Var2[i], 
                                                             intercept = tcoef[1], 
                                                             slope = tcoef[2]))
  }
  
  # Join primary and nearest baro data to the input data ----
  
  dat_with_baro <- left_join(dat, select(baro_dat,
                                         timestamp,
                                         site_station_code,
                                         primary_baro_press = airpress_kPa,
                                         primary_baro_temp = airtemp_C),
                             by = c("timestamp", "primary_baro" = "site_station_code")) %>% 
    left_join(select(baro_dat,
                     timestamp,
                     site_station_code,
                     nearest_baro_press = airpress_kPa,
                     nearest_baro_temp = airtemp_C), 
              by = c("timestamp", "nearest_baro" = "site_station_code"))
  
  # Create an extrapolated baro pressure time series for the primary baro data
  # Based on using values from nearby baro loggers
  
  print("Adding extrapolated data to input file.")
  
  dat_with_baro$airpress_kPa_ext <- NA
  
  for(i in 1:nrow(dat_with_baro)) {
    # Find matching row in pres_coefficients
    coef <- pres_coefficients[pres_coefficients$site1 == dat_with_baro$primary_baro[i] & 
                           pres_coefficients$site2 == dat_with_baro$nearest_baro[i], ]
    # If there is a matching row
    if(nrow(coef) == 1) {
      # Calculate airpress_kPa_ext
      dat_with_baro$airpress_kPa_ext[i] <- coef$intercept + coef$slope * dat_with_baro$nearest_baro_press[i]
    }
  }
  
  dat_with_baro <- dat_with_baro %>% 
    mutate(airpress_kPa_adj = ifelse(!is.na(primary_baro_press), primary_baro_press, 
                                            airpress_kPa_ext))
  
  # ## Test
  # ext <- baro_dat %>%
  #   filter(site_station_code == "CRIS_ST_40") %>%
  #   mutate(pres_DEAD_ST_10 = pres_coefficients$intercept[1] + pres_coefficients$slope[1] * airpress_kPa_U20_adj)
  # 
  # ggplot(data = filter(dat_with_baro, site_station_code == dat_sites), aes(x = timestamp))+
  #   geom_line(aes(y = primary_baro_press, colour = "primary")) +
  #   geom_line(data = filter(baro_dat, site_station_code == "CRIS_ST_40"), aes(y = airpress_kPa_U20_adj, colour = "nearest")) +
  #   geom_line(data = ext, aes(y = pres_DEAD_ST_10, colour = "extrapolated")) +
  #   scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d") +
  #   scale_colour_manual(values = c("primary" = "red", "nearest" = "blue", "extrapolated" = "green")) +
  #   theme_classic()
  # 
  # plotly::ggplotly(p = ggplot2::last_plot())
  
  # Now do the same thing for temperature 
  
  dat_with_baro$airtemp_C_ext <- NA
  
  for(i in 1:nrow(dat_with_baro)) {
    # Find matching row in coefficients
    coef <- temp_coefficients[temp_coefficients$site1 == dat_with_baro$primary_baro[i] & 
                                temp_coefficients$site2 == dat_with_baro$nearest_baro[i], ]
    # If there is a matching row
    if(nrow(coef) == 1) {
      # Calculate airtemp_C_U20_ext
      dat_with_baro$airtemp_C_ext[i] <- coef$intercept + coef$slope * dat_with_baro$nearest_baro_temp[i]
    }
  }
  
  dat_with_baro <- dat_with_baro %>% 
    mutate(airtemp_C_adj = ifelse(!is.na(primary_baro_temp), primary_baro_temp, airtemp_C_ext))

  # ## Test
  # ext <- baro_dat %>%
  #   filter(site_station_code == "CRIS_ST_40") %>%
  #   mutate(temp_DEAD_ST_10 = temp_coefficients$intercept[3] + temp_coefficients$slope[3] * airtemp_C_U20_adj)
  # 
  # ggplot(data = filter(dat_with_baro, site_station_code == site), aes(x = timestamp))+
  #   geom_line(aes(y = primary_baro_temp, colour = "primary")) +
  #   geom_line(data = filter(baro_dat, site_station_code == "CRIS_ST_40"), aes(y = airtemp_C_U20_adj, colour = "nearest")) +
  #   geom_line(data = ext, aes(y = temp_DEAD_ST_10, colour = "extrapolated")) +
  #   scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d") +
  #   scale_colour_manual(values = c("primary" = "red", "nearest" = "blue", "extrapolated" = "green")) +
  #   theme_classic()
  # 
  # plotly::ggplotly(p = ggplot2::last_plot())
  
  ## Add QA/QC back in ----
  dat_with_baro_qaqc <- left_join(dat_with_baro, select(baro_dat,
                                                        timestamp,
                                                        site_station_code,
                                                        baro_qaqc_code,
                                                        baro_qaqc_note),
                                  by = c("timestamp", "primary_baro" = "site_station_code")) %>% 
    filter(!is.na(nearest_baro))
  
  
  update_qaqc_cols <- function(col, new_comment) {
    ifelse(is.na(col) | col == "", new_comment, paste(col, new_comment, sep = "; "))
  }
  
  # Append QAQC messages
  dat_with_baro_qaqc <- dat_with_baro_qaqc %>% 
    mutate(baro_qaqc_code = ifelse(is.na(primary_baro_press) | is.na(primary_baro_temp), 
                                   update_qaqc_cols(baro_qaqc_code, "ALT_BARO_CORR"),
                                   baro_qaqc_code),
           baro_qaqc_note = ifelse(is.na(primary_baro_press) | is.na(primary_baro_temp), 
                                   update_qaqc_cols(baro_qaqc_note, paste("Extrapolated baro data based on", nearest_baro, "due to missing measurements")),
                                   baro_qaqc_note)
    )
  
  # rename variables to user input
  names(dat_with_baro_qaqc)[names(dat_with_baro_qaqc) ==  "airpress_kPa"] <- var_airpress_kPa
  names(dat_with_baro_qaqc)[names(dat_with_baro_qaqc) ==  "airpress_kPa_adj"] <- paste0(var_airpress_kPa, "_adj")
  names(dat_with_baro_qaqc)[names(dat_with_baro_qaqc) ==  "airtemp_C"] <- var_airtemp_C
  names(dat_with_baro_qaqc)[names(dat_with_baro_qaqc) ==  "airtemp_C_adj"] <- paste0(var_airtemp_C, "_adj")
  
  # # Write the file and return the data frame
  # site_type <- unique(dat_with_baro_qaqc$site_type)
  # 
  # years <- sort(unique(year(dat_with_baro_qaqc$timestamp)))
  # if (length(years) == 1) {
  #   year <- as.character(years)
  # } else {
  #   year <- paste0(min(years), "-", max(years))
  # }
  # 
  # param <- unique(dat_with_baro_qaqc$parameter)
  # 
  # filename <- paste0("DM_", site_type, "_", year, "_", param, "_", "all_BARO_COMPILED", ".csv")
  # 
  # write_csv(dat_with_baro_qaqc, file.path(out_path, filename))
  # 
  # print(paste("Writing to csv", filename))
  
  return(dat_with_baro_qaqc)
}
