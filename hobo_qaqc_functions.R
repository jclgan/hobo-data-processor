#' HOBO logger QA/QC  & plotting functions

#' CREATE OR UPDATE QA/QC COLUMNS
#' Function to check whether QAQC columns have existing comments and inserts new ones
#' or appends them to columns with codes & messages separated by a semicolon ;

update_qaqc_cols <- function(col, new_comment) {
  ifelse(is.na(col) | col == "", new_comment, paste(col, new_comment, sep = "; "))
}


#' Barometric pressure U20 QAQC
#' No adjustments to actual values
#' Flags air temperature if exceeds extreme climate maximums, but does not alter values

baro_qaqc <- function(input_data,
                      climate_station = "Kamloops",
                      var_airpress_kPa = "airpress_kPa_U20",
                      var_airtemp_C = "airtemp_C_U20") {
  
  require(weathercan)
  
  if (climate_station == "Kamloops") {
    climate_ID = "1163780"
  } else if (climate_station == "Blue River") {
    climate_ID = "1160899"
  } else {
    stop("Invalid climate station name. Please enter either 'Kamloops' or 'Blue River'.")
  }
  
  # Check to ensure the data is for only one station
  if(length(unique(input_data$site_station_code)) > 1) {
    stop("Ensure the data frame has only one station")
  } else {
    print(paste("Processing", unique(input_data$site_station_code)))
  }
  
  output_data <- input_data
  
  # Create _adj columns for variables if not already present
  if (!paste0(var_airpress_kPa, "_adj") %in% names(output_data)) {
    output_data[paste0(var_airpress_kPa, "_adj")] <- output_data[[var_airpress_kPa]]
  }
  if (!paste0(var_airtemp_C, "_adj") %in% names(output_data)) {
    output_data[paste0(var_airtemp_C, "_adj")] <- output_data[[var_airtemp_C]]
  }
  
  # Extract temperature extreme max/min values for closest weather station
  # with climate normal data
  clim_norms <- weathercan::normals_dl(climate_ids = climate_ID) # normals not yet downloadable via weathercan
  temp_ext <- clim_norms$normals[[1]] %>% 
    select(period, temp_daily_max, temp_daily_min, temp_extreme_max, temp_extreme_max_date, temp_extreme_min, temp_extreme_min_date) %>% 
    filter(period != "Year") %>% 
    mutate(month = match(period, month.abb))

  # Create QA/QC codes for values exceeding max/min temps from climate normals
  if (!"baro_qaqc_code" %in% names(output_data)) {
    output_data$baro_qaqc_code <- NA
  }
  if (!"baro_qaqc_note" %in% names(output_data)) {
    output_data$baro_qaqc_note <- NA
  }
  if (!"baro_qaqc_adj" %in% names(output_data)) {
    output_data$baro_qaqc_adj <- NA
  }
  
  output_data <- output_data %>% 
    mutate(month = month(timestamp)) %>% 
    rowwise() %>% 
    mutate(baro_qaqc_code = ifelse(airtemp_C_U20 > temp_ext$temp_extreme_max[month] | airtemp_C_U20 < temp_ext$temp_extreme_min[month],
                                   update_qaqc_cols(baro_qaqc_code, "TEMP_FLAG"),
                                   baro_qaqc_code),
    baro_qaqc_note = case_when(airtemp_C_U20 > temp_ext$temp_extreme_max[month] ~ update_qaqc_cols(baro_qaqc_note, 
                                                                                                   paste0("Air temp above temp extreme for ", clim_norms$station_name[1])),
                               airtemp_C_U20 < temp_ext$temp_extreme_min[month] ~ update_qaqc_cols(baro_qaqc_note, 
                                                                                                   paste0("Air temp below temp extreme for ", clim_norms$station_name[1])),
                               TRUE ~ baro_qaqc_note)
    ) %>% 
    select(-month) 
  
  return(output_data)
}


#' Water level QAQC
#' Change all negative water level values to zero, 
#' Flag when water pressure and air pressure are equal as potentially dry
#' Flag water temperatures below 0.3C as potentially in ice, and remove water temperature values below -1C.
#' Requires visual assessment of the data for ice spikes (e.g., increase in WL/pressure over winter; can be sudden or gradual)

waterlevel_qaqc <- function(input_data, 
                            var_waterlevel_m = "waterlevel_m_U20", 
                            var_watertemp_C = "watertemp_C_U20", 
                            var_waterpress_kPa = "waterpress_kPa_U20", 
                            var_airpress_kPa = "airpress_kPa_U20_adj", # Is _adj because should already have QC'd baro logger file attached
                            var_airtemp_C = "airtemp_C_U20_adj",
                            plot_only = FALSE) {
  
  output_data <- input_data
  
  site <- unique(output_data$site_station_code)
  
  # QAQC of both water level and water temperature
  if(var_waterlevel_m != "none") {
    # Rename user input local columns to local names
    names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
    names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"
    names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
    names(output_data)[names(output_data) == var_airpress_kPa ] <- "airpress_kPa"
    names(output_data)[names(output_data) == var_waterpress_kPa ] <- "waterpress_kPa"
    
    # Create _adj columns
    names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"
    names(output_data)[names(output_data) == paste0(var_watertemp_C, "_adj" )] <- "watertemp_C_adj"
    
    # Perform automated QA/QC if you're not just plotting the data
    if(plot_only == FALSE) {
      
      # Fill _adj columns and create qa columns
      output_data <- output_data %>% 
        mutate(waterlevel_m_adj = waterlevel_m,
               watertemp_C_adj = watertemp_C,
               wl_qaqc_note = NA,
               wl_qaqc_adj = NA,
               wl_qaqc_code = NA)
      
      ## If water level changes by more than 0.1 m in 1 hour, then flag as DISTURBANCE (e.g., logger adjusted or got knocked over)
      for(i in 1:(length(output_data$timestamp)-1)){
        if(!is.na(output_data$waterlevel_m[i])){
          wl_diff <- output_data$waterlevel_m[i] - output_data$waterlevel_m[i+1]
          if(abs(wl_diff) > 0.1) {  
            output_data$wl_qaqc_code[i] <- "FLAG_DISTURBANCE" #
          }
        }
      }
      
      ## Additional flags
      for(i in 1:length(output_data$timestamp)) {
        if(!is.na(output_data$waterlevel_m[i])){
          
          if(output_data$waterlevel_m[i] < 0) {
            output_data$wl_qaqc_note[i] <-  "corrected to zero"
            output_data$wl_qaqc_adj[i] <-  "REPLACED"
            output_data$wl_qaqc_code[i] <- "NEGATIVE_WL"
            output_data$waterlevel_m_adj[i] <- 0
          }
          # if water pressure is equal to or less than air pressure, then flag as DRY
          if(output_data$waterpress_kPa[i] <= output_data$airpress_kPa[i]) {
            output_data$wl_qaqc_code[i] <- "FLAG_DRY"
          }
          
          # if water temperature is less than 0.3C, then flag as ICE
          if(output_data$watertemp_C[i] < 0.3){
            output_data$wl_qaqc_code[i] <- "FLAG_ICE"
          }
          
          # if water temperature is less than -1, then remove
          if(output_data$watertemp_C[i]< -1){
            output_data$watertemp_C_adj[i] <- NA
            output_data$wl_qaqc_note[i] <-  "water temp out of range"
            output_data$wl_qaqc_adj[i] <-  "REMOVED"
            output_data$wl_qaqc_code[i] <- "LOGGER_ICE"
          }
          
          # if water temperature is between 0 and -1, then change to 0
          if(output_data$watertemp_C[i]<0 & output_data$watertemp_C[i]>-1){
            output_data$wl_qaqc_note[i] <-  "corrected to zero"
            output_data$wl_qaqc_adj[i] <-  "REPLACED"
            output_data$wl_qaqc_code[i] <- "NEGATIVE_WT"
            output_data$watertemp_C_adj[i] <- 0
          }
        } # end of "if data not NA"
      } # end of loop for QAQC flags
    } # end of plot_only = FALSE loop
    
    p <- ggplot(data=output_data, aes(x = timestamp))+
      geom_line(aes(y = waterlevel_m, color = "waterlevel_m"), alpha = 0.5)+ 
      geom_line(aes(y = waterlevel_m_adj, color = "waterlevel_m_adj"))+
      geom_point(data = output_data %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG", wl_qaqc_code)),
                 aes(y = 0, shape = as.factor(wl_qaqc_code)),
                 color = "black")+
      scale_color_manual(values = c("waterlevel_m" = "#233d4d",
                                    "waterlevel_m_adj" = "#233d4d"))+
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4))+
      labs(title = paste("QAQC for:", site),
           y = "Water level (m)",
           x = "Timestamp", 
           color = "",
           shape = "") +
      theme_classic()
    
    
    q <- ggplot(data=output_data, aes(x = timestamp))+
      geom_line(aes(y = airtemp_C, color = "airtemp_C"), alpha = 0.5) +
      geom_line(aes(y = watertemp_C, color = "watertemp_C"), alpha = 0.5)+
      geom_line(aes(y = as.numeric(watertemp_C_adj), color = "watertemp_C_adj"))+
      scale_color_manual(values = c("watertemp_C" = "#619b8a" , 
                                    "watertemp_C_adj" = "#619b8a" ,
                                    "airtemp_C" = "#fe7f2d"))+
      labs(title = site,
           y = "Temperature (C)",
           x = "Timestamp", 
           color = "",
           shape = "") +
      theme_classic()
    
    
    # return variables to user input naming
    names(output_data)[names(output_data) == "waterlevel_m"] <- var_waterlevel_m 
    names(output_data)[names(output_data) ==  "watertemp_C"] <- var_watertemp_C
    names(output_data)[names(output_data) == "airtemp_C"] <- var_airtemp_C
    names(output_data)[names(output_data) == "airpress_kPa"] <- var_airpress_kPa
    names(output_data)[names(output_data) == "waterlevel_m_adj"] <- paste0(var_waterlevel_m, "_adj")
    names(output_data)[names(output_data) ==  "watertemp_C_adj"] <- paste0(var_watertemp_C, "_adj")
    names(output_data)[names(output_data) ==  "waterpress_kPa"] <-var_waterpress_kPa 
    
    
    if(plot_only == FALSE) {
      print("Results returned as list: select [1] for waterlevel data with QAQC flags, [2] waterlevel data to visually assess for dry/ice/disturbance periods")
      return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05)))
    }
    if(plot_only == TRUE){
      return(plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05))
    }
  } # end of var_waterlevel_m != "none"
  
  # QAQC of water temperature ONLY
  else {
    # Rename user input local columns to local names
    names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"
    names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
    names(output_data)[names(output_data) == var_airpress_kPa ] <- "airpress_kPa"
    names(output_data)[names(output_data) == var_waterpress_kPa ] <- "waterpress_kPa"
    
    # Create _adj column
    names(output_data)[names(output_data) == paste0(var_watertemp_C, "_adj" )] <- "watertemp_C_adj"
    
    # Perform automated QA/QC if you're not just plotting the data
    if(plot_only == FALSE) {
      
      # Fill _adj columns and create qa columns
      output_data <- output_data %>% 
        mutate(watertemp_C_adj = watertemp_C,
               wl_qaqc_note = NA,
               wl_qaqc_adj = NA,
               wl_qaqc_code = NA)
      
      
      for(i in 1:length(output_data$timestamp)) {
        # if water pressure is equal to or less than air pressure, then flag as DRY
        if(output_data$waterpress_kPa[i] <= output_data$airpress_kPa[i]) {
          output_data$wl_qaqc_code[i] <- "FLAG_DRY"
        }
        
        # if water temperature is less than 0.3C, then flag as ICE
        if(output_data$watertemp_C[i] < 0.3){
          output_data$wl_qaqc_code[i] <- "FLAG_ICE"
        }
        
        # if water temperature is less than -1, then remove
        if(output_data$watertemp_C[i]< -1){
          output_data$watertemp_C_adj[i] <- NA
          output_data$wl_qaqc_note[i] <-  "water temp out of range"
          output_data$wl_qaqc_adj[i] <-  "REMOVED"
          output_data$wl_qaqc_code[i] <- "LOGGER_ICE"
        }
        
        # if water temperature is between 0 and -1, then change to 0
        if(output_data$watertemp_C[i]<0 & output_data$watertemp_C[i]>-1){
          output_data$wl_qaqc_note[i] <-  "corrected to zero"
          output_data$wl_qaqc_adj[i] <-  "REPLACED"
          output_data$wl_qaqc_code[i] <- "NEGATIVE_WT"
          output_data$watertemp_C_adj[i] <- 0
        }
      } # end of loop for QAQC flags
    } # end of plot_only = FALSE loop
    
    p <- ggplot(data=output_data, aes(x = timestamp))+
      geom_line(aes(y = waterpress_kPa, color = "waterpress_kPa"), alpha = 0.5)+ 
      geom_line(aes(y = airpress_kPa, color = "airpress_kPa"))+
      geom_point(data = output_data %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG", wl_qaqc_code)),
                 aes(y = 0, shape = as.factor(wl_qaqc_code)),
                 color = "black")+
      scale_color_manual(values = c("waterpress_kPa" = "#233d4d",
                                    "airpress_kPa" = "#233d4d"))+
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4))+
      labs(title = paste("QAQC for:", site),
           y = "Water pressure (kPa)",
           x = "Timestamp", 
           color = "",
           shape = "") +
      theme_classic()
    
    
    q <- ggplot(data=output_data, aes(x = timestamp))+
      geom_line(aes(y = airtemp_C, color = "airtemp_C"), alpha = 0.5) +
      geom_line(aes(y = watertemp_C, color = "watertemp_C"), alpha = 0.5)+
      geom_line(aes(y = as.numeric(watertemp_C_adj), color = "watertemp_C_adj"))+
      scale_color_manual(values = c("watertemp_C" = "#619b8a" , 
                                    "watertemp_C_adj" = "#619b8a" ,
                                    "airtemp_C" = "#fe7f2d"))+
      labs(title = site,
           y = "Temperature (C)",
           x = "Timestamp", 
           color = "",
           shape = "") +
      theme_classic()
    
    
    # return variables to user input naming
    names(output_data)[names(output_data) ==  "watertemp_C"] <- var_watertemp_C
    names(output_data)[names(output_data) == "airtemp_C"] <- var_airtemp_C
    names(output_data)[names(output_data) == "airpress_kPa"] <- var_airpress_kPa
    names(output_data)[names(output_data) == "waterlevel_m_adj"] <- paste0(var_waterlevel_m, "_adj")
    names(output_data)[names(output_data) ==  "watertemp_C_adj"] <- paste0(var_watertemp_C, "_adj")
    names(output_data)[names(output_data) ==  "waterpress_kPa"] <-var_waterpress_kPa 
    
    
    if(plot_only == FALSE) {
      print("Results returned as list: select [1] for waterlevel data with QAQC flags, [2] waterlevel data to visually assess for dry/ice/disturbance periods")
      return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05)))
    }
    if(plot_only == TRUE){
      return(plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05))
    }
  } # end of var_waterlevel_m == "none"
}

 
#' Dissolved oxygen U26 QAQC
#' Flag U26 error values. Sensors will read -888.88 when cap is either expired or not detected

dissox_qaqc <- function(input_data, 
                        plot_only = FALSE, 
                        var_DO_mgL = "DO_mgL_U26", 
                        var_DO_percsat = "DO_percsat_U26", 
                        var_watertemp_C_1 = "watertemp_C_U26", 
                        var_waterlevel_m = "none", 
                        var_watertemp_C_2 = "none", 
                        var_airtemp_C = "airtemp_C_U20_adj") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  
  output_data <- input_data
  
  site <- unique(output_data$site_station_code)
  
  names(output_data)[names(output_data) == var_DO_mgL ] <- "DO_mgL"
  names(output_data)[names(output_data) == var_watertemp_C_1 ] <- "watertemp_C_1"
  names(output_data)[names(output_data) == var_DO_percsat ] <- "DO_percsat"
  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
  
  if(var_waterlevel_m != "none") {
    names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  }
  
  if(var_watertemp_C_2 != "none") {
    names(output_data)[names(output_data) == var_watertemp_C_2 ] <- "watertemp_C_2"
  }
  
  if(plot_only == FALSE) {
    
    # make new adj columns
    suppressWarnings({
    if(length(output_data$DO_mgL_adj)==0) {
      output_data$DO_mgL_adj <- output_data$DO_mgL
    }
    if(length(output_data$DO_percsat_adj)==0) {
      output_data$DO_percsat_adj <- output_data$DO_percsat
    }
    if(length(output_data$watertemp_C_1_adj)==0) {
      output_data$watertemp_C_1_adj <- output_data$watertemp_C_1
    }
    # if qaqc_code column does not already exist, make a new one
    if(length(output_data$DO_qaqc_code)==0) {
      output_data$DO_qaqc_code <- NA
    }
    if(length(output_data$DO_qaqc_adj)==0) {
      output_data$DO_qaqc_adj <- NA
    }
    if(length(output_data$DO_qaqc_note)==0) {
      output_data$DO_qaqc_note <- NA
    }
    })
    
    #calc air to water temp diff
    output_data$air_watertemp_diff <- abs(output_data$airtemp_C - output_data$watertemp_C_1)
    
    # if second watertemp value assigned, calculate difference between loggers
    if(var_watertemp_C_2!="none") {
      output_data$watertemp_diff <- abs(output_data$watertemp_C_1 - output_data$watertemp_C_2)
    }
    
    for(i in 1:length(output_data$timestamp)) {
      
      # Water temp outliers
      if(!is.na(output_data$watertemp_C_1[i])) {
        if(output_data$watertemp_C_1[i] < 0.3 & output_data$watertemp_C_1[i] > 0){
          output_data$DO_qaqc_code[i] <- "FLAG_ICE"
        }
        if(output_data$watertemp_C_1[i]<0 & output_data$watertemp_C_1[i]> -1){
          output_data$watertemp_C_1_adj[i] <- 0
          output_data$DO_qaqc_code[i] <- "NEGATIVE_TEMP"
          output_data$DO_qaqc_adj[i] <- "REPLACED"
          output_data$DO_qaqc_note[i] <- "Water temp within bounds of 0 to -1 corrected to 0"
        }
        if(output_data$watertemp_C_1[i]<= -1){
          output_data$watertemp_C_1_adj[i] <- NA
          output_data$DO_qaqc_note[i] <-  "water temp less than or equal to -1 removed"
          output_data$DO_qaqc_adj[i] <-  "REMOVED"
          output_data$DO_qaqc_code[i] <- "LOGGER_ICE"
        }
      } # end if watertemp data available
      
      #DO outliers
      if(!is.na(output_data$DO_mgL[i])) {
        if(output_data$DO_mgL[i]<0 & output_data$DO_mgL[i]>= -1) {
          output_data$DO_mgL_adj[i] <- 0
          output_data$DO_percsat_adj[i] <- 0
          output_data$DO_qaqc_code[i] <- "NEGATIVE_DO"
          output_data$DO_qaqc_adj[i] <- "REPLACED"
          output_data$DO_qaqc_note[i] <- "DO within bounds of 0 to -1 corrected to 0"
        }
        if(output_data$DO_mgL[i]< -1) {
          output_data$DO_mgL_adj[i] <- NA
          output_data$DO_percsat_adj[i] <- NA
          output_data$DO_qaqc_code[i] <- "NEGATIVE_DO"
          output_data$DO_qaqc_adj[i] <- "REMOVED"
          output_data$DO_qaqc_note[i] <- "DO less than -1 removed"
        }
        if(output_data$DO_mgL[i] > 18) {
          output_data$DO_mgL_adj[i] <- NA
          output_data$DO_percsat_adj[i] <- NA
          output_data$DO_qaqc_code[i] <- "OUTLIER_DO"
          output_data$DO_qaqc_adj[i] <- "REMOVED"
          output_data$DO_qaqc_note[i] <- "DO greater than 18 mg/L removed"
        }
      }
        
      # DO error codes
      if(!is.na(output_data$DO_mgL[i])) {
        # drop error codes
        if(output_data$DO_mgL[i] == "-888.88") {
          output_data$DO_qaqc_code[i] <- "DO_ERROR"
          output_data$DO_qaqc_adj[i] <- "REMOVED"
          output_data$DO_qaqc_note[i] <- "Logger error code"
          output_data$DO_mgL_adj[i] <- NA
          output_data$DO_percsat_adj[i] <- NA
        } 
        if(output_data$watertemp_C_1[i] == "-888.88") {
          output_data$DO_qaqc_code[i] <- "TEMP_ERROR"
          output_data$DO_qaqc_adj[i] <- "REMOVED"
          output_data$DO_qaqc_note[i] <- "Logger error code"
          output_data$watertemp_C_1_adj[i] <- NA
        } 
      } # end of if do dat available
    } # end of i loop
    
    # if water temp within 2 degrees of air temp and over 100% air sat for 12 consecutive hours
    
    print("Determining dry periods...")
    counter <- 0
    threshold <- 11
    
    for (i in 1:length(output_data$timestamp)) {
      if (output_data$air_watertemp_diff[i] <= 2 && output_data$DO_percsat[i] >= 100) {
        counter <- counter + 1 
      } else {
        counter <- 0
      }
      
      if (counter >= threshold) {
        output_data$DO_qaqc_code[(i - threshold + 1):i] <- "FLAG_DRY"
      }
    }
    
  } else {
    names(output_data)[names(output_data) == paste0(var_DO_mgL, "_adj")] <-  "DO_mgL_adj"
    names(output_data)[names(output_data) == paste0(var_DO_percsat, "_adj")] <-  "DO_percsat_adj"
    names(output_data)[names(output_data) ==  paste0(var_watertemp_C_1, "_adj")] <- "watertemp_C_1_adj"
  }
  
  
  o <- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = DO_mgL, color = "DO_mgL"), alpha = 0.5)+ 
    geom_line(aes(y = DO_mgL_adj, color = "DO_mgL_adj"))+
    geom_point(data = output_data %>% drop_na(DO_qaqc_code) %>% filter(grepl("FLAG", DO_qaqc_code)),
               aes(y = 0, shape = as.factor(DO_qaqc_code)),
               color = "black")+
    scale_color_manual(values = c("DO_mgL" = "#a1c181",
                                  "DO_mgL_adj" = "#a1c181"))+
    scale_shape_manual(values = c("FLAG_ICE" = 1,
                                  "FLAG_DISTURBANCE" = 2,
                                  "FLAG_DRY" = 4))+
    labs(title = paste("QAQC for:", site),
         y = "Dissolved Oxygen (mg/L)",
         x = "Timestamp", 
         color = "",
         shape = "") +
    theme_classic()
  
  if(var_waterlevel_m != "none"){
    p <- ggplot(data=output_data, aes(x = timestamp))+
      geom_line(aes(y = waterlevel_m, color = "waterlevel_m"))+ 
      geom_point(data = output_data %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG", wl_qaqc_code)),
                 aes(y = 0, shape = as.factor(wl_qaqc_code)),
                 color = "black")+
      scale_color_manual(values = c("waterlevel_m" = "#233d4d"))+
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4))+
      labs(title = paste("QAQC for:", site),
           y = "Water level (m)",
           x = "Timestamp", 
           color = "",
           shape = "") +
      theme_classic()
  }
  
  q <- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = airtemp_C, color = "airtemp_C"), alpha = 0.5) +
    geom_line(aes(y = watertemp_C_1, color = "watertemp_C_1"), alpha = 0.5)+
    geom_line(aes(y = as.numeric(watertemp_C_1_adj), color = "watertemp_C_1_adj"))+
    scale_color_manual(values = c("watertemp_C_1" = "#619b8a" , 
                                  "watertemp_C_1_adj" = "#619b8a" ,
                                  "airtemp_C" = "#fe7f2d"))+
    labs(title = site,
         y = "Temperature (C)",
         x = "Timestamp", 
         color = "",
         shape = "") +
    theme_classic()
  
  # return variables to user input naming
  names(output_data)[names(output_data) == "DO_mgL" ] <-  var_DO_mgL
  names(output_data)[names(output_data) == "DO_mgL_adj" ] <-  paste0(var_DO_mgL, "_adj")
  names(output_data)[names(output_data) == "DO_percsat" ] <-  var_DO_percsat
  names(output_data)[names(output_data) == "DO_percsat_adj" ] <-  paste0(var_DO_percsat, "_adj")
  names(output_data)[names(output_data) == "watertemp_C_1" ] <-  var_watertemp_C_1
  names(output_data)[names(output_data) == "watertemp_C_1_adj" ] <-  paste0(var_watertemp_C_1, "_adj")
  names(output_data)[names(output_data) ==  "airtemp_C"] <-  var_airtemp_C
  
  
  if(var_watertemp_C_2 != "none") {
    names(output_data)[names(output_data) == "watertemp_C_2" ] <-  var_watertemp_C_2
  }
  
  if(plot_only == FALSE) {
    if(var_waterlevel_m != "none") {
      names(output_data)[names(output_data) == "waterlevel_m" ] <-  var_waterlevel_m
      print("Results returned as list: select [1] for dissolved oxygen data with QAQC flags, [2] dissolved oxygen data to visually assess for dry/ice/disturbance periods")
      return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.3, 0.3, 0.3), nrows = 3,shareX = TRUE, margin = 0.05)))
    } else{
      print("Results returned as list: select [1] for dissolved oxygen data with QAQC flags, [2] dissolved oxygen data to visually assess for dry/ice/disturbance periods")
      return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05)))
    }
  } # end of plot only = TRUE
  if(plot_only == TRUE){
    if(var_waterlevel_m != "none") {
      names(output_data)[names(output_data) == "waterlevel_m" ] <-  var_waterlevel_m
      return(plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.3, 0.3, 0.3), nrows = 3,shareX = TRUE, margin = 0.05))
    } else{
      return(plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05))
    }  
  } # end of plot_only = TRUE
}
#end of dissox_qaqc function  


#' Conductivity U24 QAQC
#'

#' CONDUCTIVITY LOGGER DRIFT CORRECTION & QA/QC

#' @example
#' library(here)
#' reference_file = here("Data", "site-attribute", "Deadman_YSI_waterchem_20241030.csv")
#' input_data = read_csv(here("Data", "conductivity", "2024", "OC", "intermediate", "DM_OC_2024_conductivity_all_RAW.csv")) %>% 
#'   filter(site_station_code == "BROU_OC_30")
#' conductivity_qaqc(input_data = read_csv(here("Data", "conductivity", "2024", "OC", "intermediate", "DM_OC_2024_conductivity_all_RAW.csv")) %>% 
#'                     filter(site_station_code == "BROU_OC_30"),
#'                   reference_file = here("Data", "site-attribute", "Deadman_YSI_waterchem_20241030.csv"))

conductivity_qaqc <- function(input_data, 
                              plot_only = FALSE,
                              reference_file, 
                              drop_ref = NA, # specify either a positive integer or a vector c(x, y, etc.)
                              ref_deploy_offset = 1, # hours to search between deployment and YSI measurement (between 0 and 2)
                              var_conduct_uScm = "conduct_uScm_U24", 
                              var_watertemp_C = "watertemp_C_U24",
                              var_airtemp_C = NA,
                              var_waterlevel_m = NA,
                              var_ref_conduct_uScm = "cond_uscm_ysi",
                              var_ref_watertemp_C = "watertemp_C_ysi"){
  # load packages
  require(tidyverse)
  require(fuzzyjoin)
  require(plotly)
  
  
  cdat <- input_data
  
  site <- unique(cdat$site_station_code)
  
  if (length(site) > 1) {
    stop("Ensure that only a single station has been selected")
  }
  
  # rename input columns to local columns
  names(cdat)[names(cdat) == var_conduct_uScm ] <- "conduct_uScm"
  names(cdat)[names(cdat) == var_watertemp_C ] <- "watertemp_C"
  names(cdat)[names(cdat) == paste0(var_conduct_uScm, "_adj") ] <- "conduct_uScm_adj"
  names(cdat)[names(cdat) == paste0(var_watertemp_C, "_adj" )] <- "watertemp_C_adj"
  
  if(!is.na(var_airtemp_C)){
    names(cdat)[names(cdat) == var_airtemp_C ] <- "airtemp_C"
  }
  
  # Perform drift correction and automated QA/QC if you're not just plotting the data
  if(plot_only == FALSE){
    cdat <- cdat %>% 
      mutate(timestamp = round_date(timestamp, unit = "hour"), # round to the nearest hour (same for YSI) so ref_deploy_offset integer represents an hour and not minutes
             conduct_uScm_adj = conduct_uScm,
             watertemp_C_adj = watertemp_C,
             co_qaqc_note = NA_character_,
             co_qaqc_adj = NA_character_,
             co_qaqc_code = NA_character_)
    
    time_range <- range(cdat$timestamp)
    
    ## CORRECT FOR DRIFT BASED ON YSI MEASUREMENTS
    ref_dat <- read_csv(reference_file) 
    
    # rename input columns to local columns
    names(ref_dat)[names(ref_dat) == var_ref_conduct_uScm ] <- "ref_conduct_uScm"
    names(ref_dat)[names(ref_dat) == var_ref_watertemp_C ] <- "ref_watertemp_C"
    
    ref_dat <- ref_dat %>% 
      mutate(ref_timestamp = as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M", tz = "UTC"),
             ref_timestamp = round_date(ref_timestamp, unit = "hour"),
             timestamp = ref_timestamp) %>% 
      filter(site_station_code == site) %>% 
      filter(ref_timestamp >= (time_range[1] - 60*120) & ref_timestamp <= (time_range[2] + 60*120)) %>%  # up to 2 hrs between logger timestamps
      select(ref_timestamp,
             timestamp,
             ref_conduct_uScm,
             ref_watertemp_C,
             ref_spc_uScm = spc_uscm_ysi)
    
    # drop rows if specified
    if(!all(is.na(drop_ref))) {
      ref_dat <- ref_dat[-drop_ref, , drop = FALSE]
    }
    
    # Join manual YSI measurments to conductivity data frame
    cdat_ref.1 <- left_join(cdat, ref_dat, by = "timestamp")
    
    # Where ref timestamps are beyond the logger data (e.g., just after deployment/retrieval)
    # create a second data frame and use a fuzzy join
    ref_unmatched <- left_join(ref_dat, cdat, by = "timestamp") %>% 
      filter(is.na(conduct_uScm)) %>% 
      select(starts_with(("ref_")))
    
    cdat_ref.2 <- difference_left_join(cdat, ref_unmatched,
                                       by = c("timestamp" = "ref_timestamp"),
                                       # distance_col = "distance",
                                       max_dist = ref_deploy_offset) %>% 
      filter(!is.na(ref_timestamp))
    
    total_time <- difftime(max(cdat$timestamp), min(cdat$timestamp), units = "hours")
    
    cdat_ref <- bind_rows(cdat_ref.2, cdat_ref.1) %>% 
      distinct(timestamp, .keep_all = TRUE) %>% 
      arrange(timestamp)
    
    # If there is no initial reference measurement taken at the start of the data frame
    # Then create a "dummy" reference measurement set to the values of the first logger timestamp
    # This is used to back-calculate drift assuming that logger calibration is perfect at the start of logging
    
    if (is.na(cdat_ref$ref_conduct_uScm[1])) {
      cdat_ref$ref_conduct_uScm[1] <- cdat_ref$conduct_uScm[1]
      cdat_ref$ref_watertemp_C[1] <- cdat_ref$watertemp_C[1]
      cdat_ref$ref_timestamp[1] <- cdat_ref$timestamp[1]
      
      warning("No initial YSI reference point found. Estimating drift based on perfect calibration at the start of the logger data file.")
    }
    
    ## DETERMINE DRIFT CORRECTION FACTOR
    ## Drift is calculated as the % difference between the reference & logger measurement
    
    # Isolate the reference (YSI) measurements to calculate total differences
    diff <- cdat_ref %>%
      filter(!is.na(ref_conduct_uScm)) %>% 
      mutate(ref_time_intv = as.numeric(timestamp - lag(timestamp))*24, # Number of hours between each reference measurement
             ref_time_intv = replace_na(ref_time_intv, 0), # Only applies to the first measurement
             per_c_offset = ref_conduct_uScm/conduct_uScm, # Percent offset between reference & logger measurements
             per_t_offset = ref_watertemp_C/watertemp_C,
             c_offset_diff = per_c_offset - lag(per_c_offset), # Difference between offsets for sequential pairs of reference measurements
             c_offset_diff = replace_na(c_offset_diff, 0),
             t_offset_diff = per_t_offset - lag(per_t_offset),
             t_offset_diff = replace_na(t_offset_diff, 0))
    
    # Join the reference difference measurements with the logger data frame
    cdat_ref <- left_join(cdat_ref, 
                          select(diff, 
                                 ref_timestamp, ref_time_intv, 
                                 per_c_offset, c_offset_diff, 
                                 per_t_offset, t_offset_diff),
                          by = "ref_timestamp") 
    
    ## Calculate the time ratio
    # This is the proportion of time that has passed since the last reference measurement
    # divided by the total amount of time elapsed between ref measurements
    cdat_ref <- cdat_ref %>% 
      fill(ref_time_intv, .direction = "up") %>% 
      mutate(ref_timestamp2 = ref_timestamp) %>% 
      fill(ref_timestamp2, .direction = "down") %>% 
      mutate(time_elapsed = as.numeric(difftime(timestamp, ref_timestamp2, units = "hours")),
             time_ratio = time_elapsed/ref_time_intv,
             time_ratio = replace_na(time_ratio,0),
             time_ratio = ifelse(is.infinite(time_ratio), 0, time_ratio)) # Replace NAs and infinite values (e.g, caused by dividing by 0) in 'time_ratio'
    
    ## Calculate the offset ratio
    # Fill NAs in the initial differences between reference & logger measurements (offsets)
    # And the total difference between pairs of offsets
    cdat_ref <- cdat_ref %>% 
      fill(per_c_offset, .direction = "down") %>% 
      fill(per_t_offset, .direction = "down") %>% 
      fill(c_offset_diff, .direction = "up") %>% 
      fill(t_offset_diff, .direction = "up")
    
    ## Calculate drift relative to time elapsed
    cdat <- cdat_ref %>% 
      mutate(c_drift = per_c_offset + c_offset_diff*time_ratio,
             t_drift = per_t_offset + t_offset_diff*time_ratio) %>% 
      # If there are no subsequent reference measurements, use the last known % offset
      fill(c_drift, .direction = "down") %>% 
      fill(t_drift, .direction = "down") %>% 
      # Calculate the drift-corrected conductivity & water temperature measurements
      mutate(conduct_uScm_adj = conduct_uScm*c_drift,
             watertemp_C_adj = watertemp_C*t_drift)
    
    
    ## DATA QA/QC FLAGS
    
    # Disturbance flag if conductivity changes by more than 200 uS/cm in an hour
    for(i in 1:(length(cdat$timestamp)-1)) {
      if(!is.na(cdat$conduct_uScm[i])) {
        cdiff <- cdat$conduct_uScm[i] - cdat$conduct_uScm[i+1]
        if(abs(cdiff) > 200) {
          cdat$co_qaqc_code[i] <- "FLAG_DISTURBANCE"
        }
      }
    }
    
    # Additional flags
    for(i in 1:(length(cdat$timestamp))) {
      if(!is.na(cdat$conduct_uScm[i])) {
        
        if(is.na(var_airtemp_C)){
          # if conductivity reads less than 10 uS/cm, then flag as dry
          # only applies if there is no air temp column
          if(cdat$conduct_uScm[i] < 10) {
            cdat$co_qaqc_code[i] <- "FLAG_DRY"
          }
        }
        
        # if water temperature is less than 0.3C, then flag as ice
        if(cdat$watertemp_C[i] < 0.3){
          cdat$co_qaqc_code[i] <- "FLAG_ICE"
        }
        
        # if water temperature is less than -1C, then remove
        if(cdat$watertemp_C[i]< -1){
          cdat$watertemp_C_adj[i] <- NA
          cdat$co_qaqc_note[i] <-  "water temp out of range"
          cdat$co_qaqc_adj[i] <-  "REMOVED"
          cdat$co_qaqc_code[i] <- "LOGGER_ICE"
        }
        
        # if water temperature is between 0C and -1C, then change to 0
        if(cdat$watertemp_C[i]<0 & cdat$watertemp_C[i]>-1){
          cdat$co_qaqc_note[i] <-  "corrected to zero"
          cdat$co_qaqc_adj[i] <-  "REPLACED"
          cdat$co_qaqc_code[i] <- "NEGATIVE_WT"
          cdat$watertemp_C_adj[i] <- 0
        }
      } # end of if data not NA
    } # end of additional flags
    
    # Flag dry periods using air temperature
    if(!is.na(var_airtemp_C)){
      # calc air to water temp diff
      cdat$air_watertemp_diff <- abs(cdat$airtemp_C - cdat$watertemp_C)
      
      # if water temp within 2 degrees of air temp and < 10 uS/cm for 12 consecutive hours:
      print("Determining dry periods...")
      counter <- 0
      threshold <- 12
      
      for (i in 1:length(cdat$timestamp)) {
        if (cdat$air_watertemp_diff[i] <= 2 && cdat$conduct_uScm[i] < 10) {
          counter <- counter + 1 
        } else {
          counter <- 0
        }
        
        if (counter >= threshold) {
          cdat$co_qaqc_code[(i - threshold):i] <- "FLAG_DRY"
        }
      }
    } # end of air temp dry flag

  } # end of plot_only = FALSE
  
  
  ## PLOT DATA
  p <- ggplot(cdat, aes(x = timestamp)) +
    geom_line(aes(y = conduct_uScm, colour = "conduct_uScm"), alpha = 0.5)+
    geom_line(aes(y = conduct_uScm_adj, colour = "conduct_uScm_adj"))+
    geom_point(aes(y = ref_conduct_uScm, colour = "ref_conduct"), alpha = 0.5)
  labs(title = paste("QAQC for:", site),
       y = "Conductivity (uS/cm)",
       x = "Timestamp", 
       color = "",
       shape = "") +
    theme_classic()
  
  # Plot if data contains water level
  # Use the QAQC codes from the water level data instead of the conductivity data
  if (!is.na(var_waterlevel_m)){
    
    names(cdat)[names(cdat) == var_waterlevel_m ] <- "waterlevel_m"
    
    p <- p +
      geom_line(data = cdat, aes(y = waterlevel_m*100, colour = "waterlevel_m"), linewidth = 0.3) +
      geom_point(data = cdat %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG|LOGGER", wl_qaqc_code)),
                 aes(y = 0, shape = as.factor(wl_qaqc_code)),
                 color = "black") +
      scale_colour_manual(values = c("conduct_uScm" = "#00008B",
                                     "conduct_uScm_adj" = "#00008B",
                                     "ref_conduct" = "darkred",
                                     "waterlevel_m" = "black")) +
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "LOGGER_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "LOGGER_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4,
                                    "LOGGER_DRY" = 4))
  } 
  
  # No water level data
  # Use QAQC codes for conductivity
  else {
    p <- p +
      geom_point(data = cdat %>% drop_na(co_qaqc_code) %>% filter(grepl("FLAG", co_qaqc_code)),
                 aes(y = 0, shape = as.factor(co_qaqc_code)),
                 color = "black") +
      scale_colour_manual(values = c("conduct_uScm" = "#00008B",
                                     "conduct_uScm_adj" = "#00008B",
                                     "ref_conduct" = "darkred")) +
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4))
  }
  
  q <- ggplot(cdat, aes(x = timestamp)) +
    geom_line(aes(y = watertemp_C, colour = "watertemp_C"), alpha = 0.5)+
    geom_line(aes(y = watertemp_C_adj, colour = "watertemp_C_adj"))+
    geom_point(aes(y = ref_watertemp_C, colour = "ref_watertemp"))+
    labs(title = site,
         y = "Temperature (C)",
         x = "Timestamp", 
         color = "",
         shape = "") +
    theme_classic()
  
  # Plot if the data contain air temperature
  if(!is.na(var_airtemp_C)){
    q <- q +
      geom_line(aes(y = airtemp_C, colour = "airtemp_C"), linewidth = 0.3, alpha = 0.5)+
      scale_colour_manual(values = c("watertemp_C" = "#619b8a",
                                     "watertemp_C_adj" = "#619b8a",
                                     "airtemp_C" = "lightblue",
                                     "ref_watertemp" = "green"))
  }
  else {
    q <- q +
      scale_colour_manual(values = c("watertemp_C" = "#619b8a",
                                     "watertemp_C_adj" = "#619b8a",
                                     "ref_watertemp" = "green"))
  }
  
  # return variables to user input naming
  names(cdat)[names(cdat) == "conduct_uScm"] <- var_conduct_uScm
  names(cdat)[names(cdat) == "watertemp_C"] <- var_watertemp_C
  names(cdat)[names(cdat) == "conduct_uScm_adj"] <- paste0(var_conduct_uScm, "_adj")
  names(cdat)[names(cdat) == "watertemp_C_adj"] <- paste0(var_watertemp_C, "_adj")
  if ("airtemp_C" %in% names(cdat)) names(cdat)[names(cdat) == "airtemp_C"] <- var_airtemp_C
  if ("waterlevel_m" %in% names(cdat)) names(cdat)[names(cdat) == "waterlevel_m"] <- var_waterlevel_m
  
  if(plot_only == FALSE) {
    print("Results returned as list: select [1] for conductivity data with QAQC flags, [2] reference YSI measurements, [3] conductivity data to visually assess for dry/ice/disturbance periods")
    return(list(cdat, ref_dat, plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05)))
  }
  if(plot_only == TRUE){
    return(plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05))
  }
}


#' Correct water level for disturbance period and adjustment
#'
#' Adjust water level up or down due to a disturbance or manual correction
#' @param ref_time The timestamp at which you want the section's water level to be corrected to
#' @param timestamp_end Date and time of first observation following cable break; timestamp must be formatted the same as site_data
#' @param timestamp_start Date and time of first observation before cable break; timestamp must be formatted the same as site_data
#' @param waterlevel_data The dataset with waterlevel in metres
#' @param select_station Unique identifier code for station
#' @return Dataset where stage_adj or depth_adj are shifted up or down after the cable break, qaqc_code set to LOGGER_DISTURBANCE, qaqc_adj shows the value adjusted
#' @examples 
#' data("wl_flags")
#' site_data <- adjust_waterlevel_cablebreak(timestamp_start= as.POSIXct("2022-05-02 05:00:00", tz = "UTC"), timestamp_end= as.POSIXct("2022-05-02 21:00:00", tz = "UTC"), waterlevel_data = wl_flags) 
#' @export

correct_waterlevel <- function(input_data, ref_time, section_start, section_end, shift, reason, var_waterlevel_m = "waterlevel_m_U20") {
  
  output_data <- input_data
  timestamp_ref <- as.POSIXct(ref_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  timestamp_start <- as.POSIXct(section_start, format = "%Y-%m-%d %H:%M", tz = "UTC")
  timestamp_end <- as.POSIXct(section_end, format = "%Y-%m-%d %H:%M", tz = "UTC") 
  
  #user input naming
  names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"
  
  # Calculate correction value
  # If you want to shift the period prior to logger adjustment
  if(timestamp_ref < timestamp_start){
    ref_wl <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_ref], digits = 3) 
    wrong_wl <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_start], digits = 3) 
    offset <- wrong_wl-ref_wl
  }
  
  # If you want to shift the period after logger adjustment
  else if(timestamp_ref > timestamp_end){
    ref_wl <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_ref], digits = 3) 
    wrong_wl <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_end], digits = 3) 
    offset <- ref_wl-wrong_wl
  }
  
  for(i in 1:length(output_data$timestamp)) {
    # correct logger level
    if(output_data$timestamp[i] >= timestamp_start & output_data$timestamp[i] <= timestamp_end) {
      
      if(reason == "disturbance") {
        output_data$wl_qaqc_code[i] <- "LOGGER_DISTURBANCE"
      } else if(reason == "adjustment") {
        output_data$wl_qaqc_code[i] <- "LOGGER_MANUAL_ADJ"
      } else {
        stop("Error: specify reason 'disturbance' or 'adjustment'.")
      }
      
      output_data$wl_qaqc_adj[i] <-  "CORRECTED"
      
      # use _adj if present, 
      if(!is.na(output_data$waterlevel_m_adj[i])) {
        if(shift == "down") {
          output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m_adj[i] - offset}
        else if(shift == "up") {
          output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m_adj[i] + offset}
        else{
          stop("Must specify whether to shift values 'up' or 'dowm'.")}
      } # if _adj not present, use col default
      else {
        if(shift == "down") {
          output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m[i] - offset}
        else if(shift == "up") {
          output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m[i] + offset}
        else{
          stop("Must specify whether to shift values 'up' or 'dowm'.")}
      }
    }
  }
  
  # reverse user input naming
  names(output_data)[names(output_data) ==  "waterlevel_m"] <-  var_waterlevel_m
  names(output_data)[names(output_data) ==  "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj")
  
  return(output_data)
}


#' Adjust reference water level 
#'
#' Adjust water level to a new zero/reference level by shifting all values up or down
#' @param timestamp_start Date and time of start of section for new zero/ reference level - this will be the new zero measurement for water level; Timestamp must be formatted the same as site_data.
#' @param timestamp_end Date and time of end of section for new zero/ reference level - this will be the new zero measurement for water level; Timestamp must be formatted the same as site_data
#' @param waterlevel_data The dataset with waterlevel in metres
#' @return Dataset where waterlevel_m_adj are shifted up or down to desired correction, qcqa_code set to MANUAL_ERROR, and qaqc_adj states numerical change to raw value
#' @examples 
#' data("wl_flags")
#' site_data <- adjust_waterlevel_zero(timestamp_start= as.POSIXct("2022-02-14 05:00:00", tz = "UTC"), timestamp_end= as.POSIXct("2022-05-02 21:00:00", tz = "UTC"), waterlevel_data = wl_flags) 
#' @export

adjust_wl_ref <- function(input_data, start_time, end_time, shift, var_waterlevel_m = "waterlevel_m_U20") {
  library(dplyr)
  
  output_data <- input_data
  timestamp_start <- as.POSIXct(start_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  timestamp_end <- as.POSIXct(end_time, format = "%Y-%m-%d %H:%M", tz = "UTC") 
  
  #user input naming
  names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"
  
  
  # get avg dry water level for selected time range
  corr_zero <- output_data %>%
    filter(timestamp > timestamp_start & timestamp < timestamp_end)
  corr_zero <- round(abs(mean(corr_zero$waterlevel_m, na.rm = TRUE)), digits = 3)
  
  
  # correct error in stage measurement for all obs at site
  for(i in 1:length(output_data$timestamp)) {
    output_data$wl_qaqc_adj[i] <- "SHIFT_CORRECTION"
    # use stage or stage
    # use adj if full, if NA use water_level_m
    if(shift == "up") {
      output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m[i]+corr_zero+0.1 #logger height 0.1 m above bottom
      output_data$wl_qaqc_note[i] <-  paste((corr_zero), "m added to WL")
    }
    if(shift == "down") {
      output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m[i]-corr_zero+0.1 #logger height 0.1 m above bottom
      output_data$wl_qaqc_note[i] <-  paste((corr_zero), " m subtracted from WL")
    }
  }
  
  # revert naming
  names(output_data)[names(output_data) == "waterlevel_m"] <-  var_waterlevel_m 
  names(output_data)[names(output_data) ==  "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj")
  
  return(output_data)
}


#' Correct water level for spikes
#'
#' Remove water level values within a spike, and add the average change in water level per timestamp between start and end
#' @param timestamp_start Date and time at start of  spike; timestamp must be formatted the same as site_data
#' @param timestamp_end date and time end of spike; timestamp must be formatted the same as site_data
#' @param waterlevel_data The dataset with waterlevel in metres
#' @param select_station Unique identifier code for station
#' @param reason_to_adjust Type of event that caused spike; Either ice or disturbance
#' @return Dataset where stage_adj or depth_adj is flattened within the spike, and qaqc_adj notes AVERAGE
#' @examples 
#' data("wl_flags")
#' site_data <- adjust_waterlevel_spike(timestamp_start= as.POSIXct("2022-10-09 05:00:00", tz = "UTC"), timestamp_end= as.POSIXct("2022-10-12 21:00:00", tz = "UTC"), waterlevel_data = wl_flags, reason_to_adjust = "ice") 
#' @export

adjust_wl_spike <- function(input_data, start_time, end_time, reason, var_waterlevel_m = "waterlevel_m_U20", var_watertemp_C = "watertemp_C_U20") {
  
  output_data <- input_data
  
  timestamp_start <- as.POSIXct(start_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  timestamp_end <- as.POSIXct(end_time, format = "%Y-%m-%d %H:%M", tz = "UTC") 
  
  #user input naming
  names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"
  
  # correct to average
  event_start <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_start], digits = 3) 
  event_end <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_end], digits = 3) 
  corr_event <- event_end-event_start
  n_event <- as.numeric(difftime(timestamp_end, timestamp_start, units = "hours"))
  corr_event <- corr_event/n_event
  
  for(i in 1:length(output_data$timestamp)) {
    
    # remove spikes
    if(output_data$timestamp[i] >= timestamp_start & output_data$timestamp[i] <= timestamp_end) {
      output_data$wl_qaqc_adj[i] <-  "AVERAGE"
      output_data$wl_qaqc_note[i] <- "spike flattened to average between before and after event"
      # use adj if full, if NA use water_level_m
      if(!is.na(output_data$waterlevel_m_adj[i])) {
        output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m_adj[output_data$timestamp == timestamp_start]+corr_event*(i-which(output_data$timestamp == timestamp_start))
      } else {
        output_data$waterlevel_m[i] <- output_data$waterlevel_m[output_data$timestamp == timestamp_start]+corr_event*(i-which(output_data$timestamp == timestamp_start))
      }
      if(reason == "ice") {
        output_data$wl_qaqc_code[i] <- "LOGGER_ICE"
      }
      if(reason == "disturbance") {
        output_data$wl_qaqc_code[i] <- "LOGGER_DISTURBANCE"
      }
      if(reason != "ice" & reason != "disturbance") {
        print("reason_to_adjust not recognized. Must be either 'ice' or 'disturbance'.")
      }
      
    } 
  }
  # reverse user input naming
  names(output_data)[names(output_data) ==  "waterlevel_m"] <-  var_waterlevel_m
  names(output_data)[names(output_data) ==  "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj")
  
  
  return(output_data)
}


#' Logger removal or dry
#' Manually removes logger data from QC and plotting
#' This is done for when there a logger has been temporarily removed from site during its deployment or is dry
#' @param input_data The site-specific data frame
#' @param logger_type Specify U20_baro, U20_water, U24, or U26
#' @param start_time The start of data scrubbing
#' @param end_time The end of data scrubbing
#' @param reason Specify "ice", "dry", "disturbance", or "removed"
#' @param apply_all Default FALSE. Applies only to reason=="dry". Specify TRUE to apply NA to the entire dataset and not just the range defined by start_time and end_time. 
#' @param flag_only Default FALSE. Applies only to reason=="disturbance". Specify TRUE to only flag the data and not remove it. 
#' @return Site dataframe with adjusted columns

adjust_logger_NA <- function(input_data, 
                             logger_type, 
                             start_time, 
                             end_time,
                             reason,
                             apply_all = FALSE, 
                             flag_only = FALSE,
                             var_waterlevel_m = "waterlevel_m_U20", 
                             var_watertemp_C = "watertemp_C_U20", 
                             var_airtemp_C = "airtemp_C_U20",
                             var_DO_mgL = "DO_mgL_U26", 
                             var_DO_percsat = "DO_percsat_U26",
                             var_conduct_uScm = "conduct_uScm_U24") {
  
  library(tidyverse)
  
  timestamp_start <- as.POSIXct(start_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  timestamp_end <- as.POSIXct(end_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  output_data <- input_data %>% 
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M", tz = "UTC"))
  
  ## U20 Water Level Logger adjustments
  if (logger_type == "U20_water") {
    # rename user input variables
    names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
    names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"
    names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
    # correct user entry error
    if(var_watertemp_C == "watertemp_C_U26"){
      var_watertemp_C == "watertemp_C_U20"
    }
    names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"
    names(output_data)[names(output_data) == paste0(var_watertemp_C, "_adj") ] <- "watertemp_C_adj"
    
    # remove data if logger is in ice
    if(reason == "ice") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$waterlevel_m_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$wl_qaqc_code[i] <- "LOGGER_ICE"
          output_data$wl_qaqc_adj[i] <- "REMOVED"
          output_data$wl_qaqc_note[i] <- "WL data removed due to logger in ice"
        }
      }
    } 
    
    # remove data if logger has been disturbed
    if(reason == "disturbance") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          if(flag_only == FALSE) {
            output_data$waterlevel_m_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$wl_qaqc_code[i] <- "LOGGER_DISTURBANCE"
            output_data$wl_qaqc_adj[i] <- "REMOVED"
            output_data$wl_qaqc_note[i] <- "Data removed due to disturbance to logger"
          } # end of flag_only = FALSE
          else {
            output_data$DO_qaqc_code[i] <- "LOGGER_DISTURBANCE"
          }
        }
      }
    }
    
    # remove data if logger has been removed, but still logging
    if(reason == "removed") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$waterlevel_m_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$wl_qaqc_code[i] <- "LOGGER_REMOVED"
          output_data$wl_qaqc_adj[i] <- "REMOVED"
          output_data$wl_qaqc_note[i] <- "WL data removed due to logger being manually removed"
        }
      }
    }
    
    # note all values below dry water level as dry
    if(reason == "dry") {
      if(apply_all == TRUE) {
        # get highest wl in designated dry period
        dry_range <- which(!is.na(output_data$waterlevel_m) & 
                             output_data$timestamp >= timestamp_start & 
                             output_data$timestamp <= timestamp_end)
        
        dry_wl <- max(output_data$waterlevel_m[dry_range], na.rm = TRUE)
        # make all wl values at or below this
        for(i in 1:length(output_data$timestamp)){
          if(!is.na(output_data$waterlevel_m_adj[i]) & output_data$waterlevel_m_adj[i]<=dry_wl){
            output_data$waterlevel_m_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$wl_qaqc_code[i] <- "LOGGER_DRY"
            output_data$wl_qaqc_adj[i] <- "REMOVED"
            output_data$wl_qaqc_note[i] <- paste("Data removed due to dry logger at waterlevel below", dry_wl, "m")          }
        }
      } # end of apply_all = TRUE
      
      else {
        for(i in 1:length(output_data$timestamp)) {
          if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
            output_data$waterlevel_m_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$wl_qaqc_code[i] <- "LOGGER_DRY"
            output_data$wl_qaqc_adj[i] <- "REMOVED"
            output_data$wl_qaqc_note[i] <- "Data removed due to dry logger"
          }
        }
      } # end of apply_all = FALSE
    } # end of reason == "dry"
    
  } # end of logger_type = U20_water 
  
  
  ## U26 Dissolved Oxygen Logger adjustments
  # rename user input variables
  if (logger_type == "U26") {
    names(output_data)[names(output_data) == var_DO_mgL ] <- "DO_mgL"
    names(output_data)[names(output_data) == paste0(var_DO_mgL, "_adj") ] <- "DO_mgL_adj"
    names(output_data)[names(output_data) == var_DO_percsat ] <- "DO_percsat"
    names(output_data)[names(output_data) == paste0(var_DO_percsat, "_adj") ] <- "DO_percsat_adj"
    names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
    # correct user entry error
    if(var_watertemp_C=="watertemp_C_U20"){
      var_watertemp_C <- "watertemp_C_U26"
    } 
    names(output_data)[names(output_data) ==  var_watertemp_C] <- "watertemp_C"
    names(output_data)[names(output_data) ==  paste0(var_watertemp_C, "_adj")] <- "watertemp_C_adj"
    
    if(var_waterlevel_m!="none"){
      names(output_data)[names(output_data) ==  var_waterlevel_m] <- "waterlevel_m"
    }
    
    
    # remove data if logger is in ice
    if(reason == "ice") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$DO_mgL_adj[i] <- NA
          output_data$DO_percsat_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$DO_qaqc_code[i] <- "LOGGER_ICE"
          output_data$DO_qaqc_adj[i] <- "REMOVED"
          output_data$DO_qaqc_note[i] <- "DO data removed due to logger in ice"
        }
      }
    } 
    
    # remove logger data due to disturbance
    if(reason == "disturbance") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          if(flag_only == FALSE) {
            output_data$DO_mgL_adj[i] <- NA
            output_data$DO_percsat_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$DO_qaqc_code[i] <- "LOGGER_DISTURBANCE"
            output_data$DO_qaqc_adj[i] <- "REMOVED"
            output_data$DO_qaqc_note[i] <- "DO data removed due to disturbance to logger"
          }
          else {
            output_data$DO_qaqc_code[i] <- "LOGGER_DISTURBANCE"
          }
        }
      }
    }
    
    # remove logger data due to manual removal
    if(reason == "removed") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$DO_mgL_adj[i] <- NA
          output_data$DO_percsat_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$DO_qaqc_code[i] <- "LOGGER_REMOVED"
          output_data$DO_qaqc_adj[i] <- "REMOVED"
          output_data$DO_qaqc_note[i] <- "DO data removed due to logger being manually removed"
        }
      }
    }
    
    # remove logger data due to dry period
    if(reason == "dry") {
      if(apply_all == TRUE) {
        if(var_waterlevel_m!="none"){
          # get highest wl in designated dry period
          dry_range <- which(!is.na(output_data$waterlevel_m) & 
                               output_data$timestamp >= timestamp_start & 
                               output_data$timestamp <= timestamp_end)
          
          dry_wl <- max(output_data$waterlevel_m[dry_range], na.rm = TRUE)
          # make all wl values at or below this
          for(i in 1:length(output_data$timestamp)) {
            if(!is.na(output_data$waterlevel_m[i]) & output_data$waterlevel_m[i]<=dry_wl) {
              output_data$DO_percsat_adj[i] <- NA
              output_data$DO_mgL_adj[i] <- NA
              output_data$watertemp_C_adj[i] <- NA
              output_data$DO_qaqc_code[i] <- "LOGGER_DRY"
              output_data$DO_qaqc_adj[i] <- "REMOVED"
              output_data$DO_qaqc_note[i] <- paste("DO data removed due to dry logger at waterlevel below", dry_wl, "m")
            }
          }
        } else{print("WARNING: var_waterlevel_m missing from input. Unable to apply to all data.")}
      } # end of apply_all = TRUE 
      
      else {
        for(i in 1:length(output_data$timestamp)) {
          if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
            if(flag_only == FALSE){
              output_data$DO_mgL_adj[i] <- NA
              output_data$DO_percsat_adj[i] <- NA
              output_data$watertemp_C_adj[i] <- NA
              output_data$DO_qaqc_code[i] <- "LOGGER_DRY"
              output_data$DO_qaqc_adj[i] <- "REMOVED"
              output_data$DO_qaqc_note[i] <- "DO data removed due to dry logger"
            }
            else {
              output_data$DO_qaqc_code[i] <- "FLAG_DRY"
            }
          }
        }
      } # end of apply_all == FALSE
    } # end of reason == "dry" 
    
  } # end of logger_type = U26
  
  
  ## U24 Conductivity Logger adjustments
  if (logger_type == "U24") {
    # rename user input variables
    names(output_data)[names(output_data) == var_conduct_uScm ] <- "conduct_uScm"
    names(output_data)[names(output_data) == paste0(var_conduct_uScm, "_adj") ] <- "conduct_uScm_adj"
    # correct user entry error
    if(var_watertemp_C == "watertemp_C_U20" | var_watertemp_C == "watertemp_C_U26"){
      var_watertemp_C == "watertemp_C_U24"
    }
    names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"
    names(output_data)[names(output_data) == paste0(var_watertemp_C, "_adj") ] <- "watertemp_C_adj"
    
    # remove data if logger is in ice
    if(reason == "ice") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$conduct_uScm_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$co_qaqc_code[i] <- "LOGGER_ICE"
          output_data$co_qaqc_adj[i] <- "REMOVED"
          output_data$co_qaqc_note[i] <- "Conductivity data removed due to logger in ice"
        }
      }
    } 
    
    # remove data if logger has been disturbed
    if(reason == "disturbance") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          if(flag_only == FALSE) {
            output_data$conduct_uScm_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$co_qaqc_code[i] <- "LOGGER_DISTURBANCE"
            output_data$co_qaqc_adj[i] <- "REMOVED"
            output_data$co_qaqc_note[i] <- "Data removed due to disturbance to logger"
          } # end of flag_only = FALSE
          else {
            output_data$co_qaqc_code[i] <- "LOGGER_DISTURBANCE"
          }
        }
      }
    }
    
    # remove data if logger has been removed, but still logging
    if(reason == "removed") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$conduct_uScm_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$co_qaqc_code[i] <- "LOGGER_REMOVED"
          output_data$co_qaqc_adj[i] <- "REMOVED"
          output_data$co_qaqc_note[i] <- "Data removed due to logger being manually removed"
        }
      }
    }
    
    # remove data if logger is dry
    if(reason == "dry") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$conduct_uScm_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$co_qaqc_code[i] <- "LOGGER_DRY"
          output_data$co_qaqc_adj[i] <- "REMOVED"
          output_data$co_qaqc_note[i] <- "Data removed due to dry logger"
        }
      }
    } # end of reason == "dry"
    
  } # end of logger_type = U24
  
  # return local column names to input data names
  names(output_data)[names(output_data) == "waterlevel_m" ] <-  var_waterlevel_m
  names(output_data)[names(output_data) == "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj") 
  names(output_data)[names(output_data) == "watertemp_C" ] <-  var_watertemp_C
  names(output_data)[names(output_data) ==  "watertemp_C_adj"] <- paste0(var_watertemp_C, "_adj")
  names(output_data)[names(output_data) ==  "DO_mgL"] <-  var_DO_mgL
  names(output_data)[names(output_data) ==  "DO_mgL_adj"] <-  paste0(var_DO_mgL, "_adj")
  names(output_data)[names(output_data) ==  "DO_percsat"] <-  var_DO_percsat
  names(output_data)[names(output_data) ==  "DO_percsat_adj"] <-  paste0(var_DO_percsat, "_adj")
  names(output_data)[names(output_data) ==  "airtemp_C"] <-  var_airtemp_C
  names(output_data)[names(output_data) == "conduct_uScm" ] <-  var_conduct_uScm
  names(output_data)[names(output_data) == "conduct_uScm_adj"] <-  paste0(var_conduct_uScm, "_adj")
  
  return(output_data)
}


#' Write clean data
#' 
#' 

write_clean_data <- function(input_data, logger_type, output_path, 
                             include_airtemp = TRUE,
                             var_airpress_kPa = "airpress_kPa_U20", 
                             var_airtemp_C = "airtemp_C_U20",
                             var_waterlevel_m = "waterlevel_m_U20", 
                             var_watertemp_C = "watertemp_C_U20", 
                             var_DO_mgL = "DO_mgL_U26", 
                             var_DO_percsat = "DO_percsat_U26",
                             var_conduct_uScm = "conduct_uScm_U24",
                             var_spc_uScm = "spc_uScm") {
  
  library(tidyverse)
  library(purrr)
  
  if(!(logger_type %in% c("U20_baro", "U20_wl", "U26", "U24"))) {
    stop(paste("Logger type", logger_type, "not recognized. Please input 'U20_baro', 'U20_wl', 'U24', or 'U26'"))
  }
  
  if(logger_type == "U20_baro"){
    logger_header <- "BARO"
    
    input_data <- input_data %>% select(c(site_station_code, 
                                          logger_sn = sn,
                                          timestamp, 
                                          var_airpress_kPa, paste0(var_airpress_kPa, "_adj"), 
                                          var_airtemp_C, paste0(var_airtemp_C, "_adj"), 
                                          baro_qaqc_code, baro_qaqc_adj, baro_qaqc_note))
  }
  
  if(logger_type == "U20_wl"){
    logger_header <- "WL"
    
    if(include_airtemp == TRUE){
      input_data <- input_data %>% select(c(site_station_code, 
                                            logger_sn = sn,
                                            timestamp, 
                                            var_waterlevel_m, paste0(var_waterlevel_m, "_adj"), 
                                            var_watertemp_C, paste0(var_watertemp_C, "_adj"), 
                                            ref_waterlevel_m = ref_m,
                                            paste0(var_airtemp_C, "_adj"),
                                            nearest_baro,
                                            baro_qaqc_code, baro_qaqc_note,
                                            wl_qaqc_code, wl_qaqc_adj, wl_qaqc_note))
    }
    else {
      input_data <- input_data %>% select(c(site_station_code, 
                                            timestamp, 
                                            logger_sn = sn,
                                            var_waterlevel_m, paste0(var_waterlevel_m, "_adj"), 
                                            var_watertemp_C, paste0(var_watertemp_C, "_adj"), 
                                            ref_waterlevel_m = ref_m,
                                            wl_qaqc_code, wl_qaqc_adj, wl_qaqc_note))
    }
  }
  
  if(logger_type == "U26"){
    logger_header <- "DO"
    # correct default header for water temp to match with DO logger
    if(var_watertemp_C=="watertemp_C_U20"){
      var_watertemp_C <- "watertemp_C_U26"
    }
    
    if(include_airtemp == TRUE){
      input_data <- input_data %>% select(c(site_station_code, 
                                            timestamp, 
                                            logger_sn = sn,
                                            var_DO_mgL, paste0(var_DO_mgL, "_adj"), 
                                            var_DO_percsat, paste0(var_DO_percsat, "_adj"), 
                                            var_watertemp_C, paste0(var_watertemp_C, "_adj"), 
                                            paste0(var_airtemp_C, "_adj"),
                                            nearest_baro,
                                            baro_qaqc_code, baro_qaqc_note,
                                            DO_qaqc_code, DO_qaqc_adj, DO_qaqc_note))
    }
    else{
      input_data <- input_data %>% select(c(site_station_code, 
                                            timestamp, 
                                            logger_sn = sn,
                                            var_DO_mgL, paste0(var_DO_mgL, "_adj"), 
                                            var_DO_percsat, paste0(var_DO_percsat, "_adj"), 
                                            var_watertemp_C, paste0(var_watertemp_C, "_adj"), 
                                            DO_qaqc_code, DO_qaqc_adj, DO_qaqc_note))
    }
  }
  
  if(logger_type == "U24"){
    logger_header <- "CO"
    input_data <- input_data %>% select(c(site_station_code, 
                                          timestamp, 
                                          logger_sn = sn,
                                          var_conduct_uScm, paste0(var_conduct_uScm, "_adj"), 
                                          var_watertemp_C, paste0(var_watertemp_C, "_adj"), 
                                          spc_uScm,
                                          ref_conduct_uScm,
                                          ref_watertemp_C,
                                          co_qaqc_code, co_qaqc_adj, co_qaqc_note))
  }

  
  # EXPORT CSV BY LOGGER TYPE LOCATION AND YEAR
  
  # some stations may have multiple logger files, one from a download in spring and another for a download in fall
  # this loop will split data by type, location and year for ease of access and processing
  
  input_data <- input_data %>% 
    mutate(year = as.character(year(timestamp)))
  
  # Split the data frame by site_station_code and year
  data_list <- split(input_data, list(input_data$site_station_code, input_data$year))
  
  write_to_csv <- function(list_element, element_name) {
    
    # Skip non-existent site-year combinations
    if(nrow(list_element) == 0) {
      return()
    }
    
    site_station_code <- unique(list_element$site_station_code)
    year <- as.character(unique(list_element$year))
    start_timestamp <- format(as.Date(min(list_element$timestamp)), "%Y%m%d")
    end_timestamp <- format(as.Date(max(list_element$timestamp)), "%Y%m%d")
    site_type <- substr(site_station_code, 6, 7)
    
    list_element <- select(list_element, -year)
    
    # Define the output path
    if(logger_type == "U20_baro"){
      output_directory <- file.path(output_path, as.character(year), "clean")
    } else {
      output_directory <- file.path(output_path, site_type, as.character(year), "clean")
    }
    
    # Create a directory for the year if it doesn't exist
    if (!dir.exists(output_directory)) {
      dir.create(output_directory)
    }
    
    file_name <- paste(site_station_code, logger_header, start_timestamp, end_timestamp, "v1.0.csv", sep = "_")
    
    # Write the data frame to a csv file
    print(paste("Writing", site_station_code, year, "to CSV in", output_directory))
    write_csv(list_element, file.path(output_directory, file_name))
  }
  
  walk2(data_list, names(data_list), write_to_csv)
}

  

