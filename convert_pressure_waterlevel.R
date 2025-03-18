## Test me
# input_file <- list_WL$WL_DEAD_ST_10
# reference_file <- here("Data", "site-attribute", "DM_manual_stage_wl_measurements_20240923.csv")
# # select_station <- "DEAD_ST_10"
# reference_type <- "stage"

#' Source: https://www.onsetcomp.com/resources/tech-notes/barometric-compensation-method

convert_pressure_waterlevel <- function(input_data,
                                        reference_file,
                                        reference_type = "stage",
                                        select_measurement = 1,
                                        var_reference_waterlevel_m = "stage_m",
                                        var_reference_timestamp = "stage_timestamp",
                                        var_waterpress_kPa = "waterpress_kPa_U20",
                                        var_watertemp_C = "watertemp_C_U20",
                                        var_airpress_kPa = "airpress_kPa_U20_adj",
                                        var_airtemp_C = "airtemp_C_U20_adj") {
  
  # load packages
  require(tidyverse)
  require(fuzzyjoin)
  require(plotly)
  
  wl_dat <- input_data
  
  # rename input columns to local columns
  names(wl_dat)[names(wl_dat) == var_waterpress_kPa ] <- "waterpress_kPa_U20"
  names(wl_dat)[names(wl_dat) == var_airpress_kPa ] <- "airpress_kPa_U20_adj"
  names(wl_dat)[names(wl_dat) == var_watertemp_C ] <- "watertemp_C_U20"
  names(wl_dat)[names(wl_dat) == var_airtemp_C ] <- "airtemp_C_U20_adj"
  
  site <- unique(wl_dat$site_station_code)
  
  if (length(site) > 1) {
    stop("Ensure that only a single station has been selected")
  }
  
  time_range <- range(wl_dat$timestamp) # Get data range +/- 60 min at start/end to account for difference between logger deployment & measurement
  
  # Format the reference water level data
  ref_dat <- read_csv(reference_file) 
  
  names(ref_dat)[names(ref_dat) == var_reference_timestamp] <- 'ref_timestamp'
  names(ref_dat)[names(ref_dat) == var_reference_waterlevel_m] <- 'ref_m'
  
  ref_dat <- ref_dat %>% 
    mutate(ref_timestamp = as.POSIXct(ref_timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC"),
           ref_timestamp = round_date(ref_timestamp, unit = "hour"),
           timestamp = ref_timestamp) %>% 
    filter(site_station_code == site) %>% 
    filter(ref_timestamp >= (time_range[1] - 6*60*60) & ref_timestamp <= (time_range[2] + 6*60*60)) # 6-hour window between logger timestamps
  
  if(reference_type != "stage" & reference_type != "depth") {
    stop("Reference_type must be either 'stage' or 'depth'.")
  }
  
  # Correct default variable name from stage to depth if selected
  if(reference_type == "depth" & var_reference_waterlevel_m == "stage_m") {
    var_reference_waterlevel_m = "depth_m"
  }
  
  ref_dat <- ref_dat %>% 
    select(-site_station_code)
  
  if(length(ref_dat$ref_timestamp)==0){
    stop(paste(reference_type," data not found for", site, ". Check reference file or change reference_type."))
  }
  
  # Join manual stage/water level readings to pressure data frame.
  # Use a fuzzy join to account for differences between logger measurement and depth reading
  
  # This joins on exact matches between logger & reference timestamps
  wl_dat_ref1 <- left_join(wl_dat, ref_dat, by = "timestamp")
  
  # Where ref timestamps are beyond the logger data (e.g., just after deployment/retrieval)
  # create a second data frame and use a fuzzy join
  ref_unmatched <- left_join(ref_dat, wl_dat, by = "timestamp") %>% 
    filter(is.na(waterpress_kPa_U20)) %>% 
    select(ref_timestamp:site_comments)
  wl_dat_ref2 <- difference_left_join(wl_dat, ref_unmatched, 
                                      by = c("timestamp" = "ref_timestamp"),
                                      max_dist = 1) %>% # 1-hour window to join
    filter(!is.na(ref_timestamp))
  
  # if there is still no matching data
  if(length(wl_dat_ref2$timestamp) == 0) {
    wl_dat_ref2 <- difference_left_join(wl_dat, ref_unmatched, 
                                        by = c("timestamp" = "ref_timestamp"),
                                        max_dist = 2) %>% # 2-hour window to join
      filter(!is.na(ref_timestamp))
  }
  
  # Combine the exact and fuzzy joins into one data frame
  wl_dat_ref <- bind_rows(wl_dat_ref2, wl_dat_ref1) %>% 
    distinct(timestamp, .keep_all = TRUE) %>% 
    arrange(timestamp)

 
  ## RUN BAROMETRIC COMPENSATION ----
  
  ### Step 1 - Calculate fluid density of water ----
  
  # To compute this array, first the fluid density is computed. This is either determined 
  # by the user-selected density, or is computed from the temperature at the reference time,
  # via:
  #   
  # ρ = (999.83952 + 16.945176 Tref - 7.9870401e-03 Tref^2 - 46.170461e-06 Tref^3 + 
  #        105.56302e-09 Tref^4 - 280.54253e-12 Tref^5) / (1 + 16.879850e-03 Tref)  [1]
  
  wl_dat_ref <- wl_dat_ref %>% 
    mutate(density_kgm3 = (999.83952 + (16.945176 * (watertemp_C_U20)) - ((7.9870401e-03) * (watertemp_C_U20^2)) - ((46.170461e-06) * (watertemp_C_U20^3)) + 
                             ((105.56302e-09) * (watertemp_C_U20^4)) - ((280.54253e-12) * (watertemp_C_U20^5))) / (1 + (16.879850e-03) * watertemp_C_U20) )
    
  # Density is converted to lb/ft3 via:
  #   ρ = 0.0624279606 ρ    [2]
  
  wl_dat_ref <- wl_dat_ref %>%
    mutate(density_lbft3 = 0.0624279606 * density_kgm3)
  
  ### Step 2 - Convert Density to Sensor Depth ----
  
  # The array of downwell pressure values, P,  are then converted to a density dependent fluid depth array, D[], via:
  #   
  #   D[]= FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * P) / ρ           [3]
  # 
  # Where,
  # 
  FEET_TO_METERS = 0.3048
  KPA_TO_PSI = 0.1450377
  PSI_TO_PSF = 144.0
  
  ### Step 3 - Calculate Barometric Fluid Density ----
  # 
  # Next, the fluid density at the reference time is determined. This is either the user-entered density, 
  # or is computed from Equations 1 & 2, resulting in ρref.
  
  # ρ = (999.83952 + 16.945176 Tref - 7.9870401e-03 Tref^2 - 46.170461e-06 Tref^3 + 
  #        105.56302e-09 Tref^4 - 280.54253e-12 Tref^5) / (1 + 16.879850e-03 Tref)  [1]
  #   ρ = 0.0624279606 ρ    [2]
  
  # The pressure at the reference time is converted to a barometric “depth” using Equation 3, 
  # resulting in Dbaro0.
  
  #   D[]= FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * P) / ρ           [3]
  #
  
  wl_dat_ref <- wl_dat_ref %>%
    mutate(sensor_depth_m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * waterpress_kPa_U20)/ density_lbft3,
           baro_sensor_depth_m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * airpress_kPa_U20_adj)/ density_lbft3)
  
  ## Step 4 -  Determine reference values and compensation constant (k)
  # 
  # The density dependent depth value at the reference time is then extracted from the array:
  #   
  #   Dref = D[Ref Time]
  
  ## Define which manual stage measurement to use to as a reference level for the barometric compensation
  
  # Selects reference water level measurement from the nth row
  L_meas <- as.numeric(ref_dat[select_measurement,"ref_m"])
  
  # Selects corresponding timestamp from the nth row (may need to add/subtract an hour if logger was out of water)
  T_meas <- as.numeric(ref_dat[select_measurement,"ref_timestamp"])
  
  # Find the row with the closest timestamp within 60 minutes
  closest_index <- which.min(abs(difftime(wl_dat_ref$timestamp, T_meas, units = "mins")))
  
  if (abs(difftime(wl_dat_ref$timestamp[closest_index], T_meas, units = "mins")) <= 60) {
    # Select water level and barometric sensor depths at reference time
    D_ref <- wl_dat_ref$sensor_depth_m[closest_index]
    D_baro0 <- wl_dat_ref$baro_sensor_depth_m[closest_index]
  } else {
    stop(paste0("Cannot find a nearest timestamp to match using ", select_measurement, ". Try a different reference point."))
  }
  
  ## Define compensation constant (k)
  k <- L_meas - (D_ref - D_baro0)
  
  ### Step 5 - Determine Water Level by Applying Compensation Constant to Entire Dataset ----
  
  # At this point, the compensation constant is applied to each downwell barometric depth reading in the array,
  # D. An important step here is to determine the proper barometric pressure value to use.  Since the BCA does
  # not require that then barometric pressure channel have the same sample times as the downwell pressure channel,
  # individual values for barometric pressure, Pbaro, may sometimes be interpolated between the points closest to 
  # the downwell pressure value of interest. 
  # 
  # Loop through the entire downwell channel, applying the compensation constant to the density dependent fluid
  # depth values computed above. This is the step that adjusts the density dependent depth values for fluctuations 
  # in barometric pressure. This is determined by:
  #   
  #   Lreal[]= D[] – Dbaro[] + k       [5]
  # 
  # Where Lreal[] is an array of the actual water level values (from a fixed reference point), 
  # D is the density dependent fluid depth array computed earlier, Dbaro is the barometric depth at the time
  # index in the array (using Equation 3), and k is the compensation constant. The values of Lreal are stored 
  # in a new Water Level channel and added to the list of available channels to plot.
  
  wl_dat_ref <- wl_dat_ref %>%
    mutate(waterlevel_m_U20 = sensor_depth_m - baro_sensor_depth_m + k)
  
  # ## ADD FLAGS ----
  # 
  # wl_dat_ref <- wl_dat_ref %>% 
  #   mutate(wl_diff = waterlevel_m - dplyr::lag(waterlevel_m, default = waterlevel_m[1])) %>%  # calculates the difference in water level between the current and previous timestamp (hour)
  #   mutate(data_flag = case_when(abs(wl_diff) > disturbance_flag ~ "DISTURBANCE", # flag if water level changes by more than 0.1 m in 1 hour
  #                                watertemp_C_U20 < ice_flag ~ "ICE", # flag if water temperature is < 0.3 C
  #                                waterpress_kPa_U20 <= airpress_kPa_U20_adj ~ "DRY" # flag if water pressure is less than or equal to air pressure
  #                                )
  #          )
  
  ## REVIEW DATA ----
  
  # Plot manual measurements on top of trace
  # If measurements are wildly off try adding or subtracting an hour from the reference timestamp or selecting a different reference
  
  p <- ggplot(data = wl_dat_ref, aes(y = waterlevel_m_U20, x = timestamp))+
    geom_line(color ="#233d4d")+ 
    geom_point(aes(y = ref_m, x = ref_timestamp), color = "#fe7f2d", size = 4)+
    labs(title = paste("kPa to m conversion for:", site),
         y = "Water level (m)",
         x = "Timestamp") +
    theme_classic()
  
  print("Results returned as list: select [1] for waterlevel data in metres, [2] for reference waterlevel dataset, [3] for waterlevel plot to visually check reference values")
  
  print("Tip: Do reference water levels observed (orange points) align with the predicted water level (solid black line)? If so, great work! Continue onto QAQC. If not, try choosing a different reference water level (ie. select_measurement = 2, etc)")
  
  # Reverse renaming of variables
  names(wl_dat_ref)[names(wl_dat_ref) == "waterpress_kPa_U20" ] <- var_waterpress_kPa
  names(wl_dat_ref)[names(wl_dat_ref) == "airpress_kPa_U20_adj" ] <- var_airpress_kPa
  names(wl_dat_ref)[names(wl_dat_ref) == "watertemp_C_U20" ] <- var_watertemp_C
  names(wl_dat_ref)[names(wl_dat_ref) == "airtemp_C_U20_adj" ] <- var_airtemp_C

  
  return(list(wl_dat_ref, ref_dat, ggplotly(p)))
}
