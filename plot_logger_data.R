plot_logger_data <- function(baro_path = "none",
                             waterlevel_path = "none",
                             dissox_path = "none",
                             conductivity_path = "none",
                             select_station = "all",
                             plot_temp = TRUE,
                             compare_float = FALSE,
                             var_airpress_kPa = "airpress_kPa_U20_adj",
                             var_airtemp_C = "airtemp_C_U20_adj",
                             var_waterlevel_m = "waterlevel_m_U20_adj", 
                             var_DO_mgL = "DO_mgL_U26_adj", 
                             var_DO_percsat = "DO_percsat_U26_adj",
                             var_spc_uScm = "spc_uScm_U24_adj",
                             var_watertemp_C_1 = "watertemp_C_U26_adj", 
                             var_watertemp_C_2 = "watertemp_C_U20_adj",
                             var_watertemp_C_3 = "watertemp_C_U24_adj") {
  
  require(tidyverse)
  require(cowplot)
  
  # if(baro_path != "none") {
  #   baro_csvs <- list.files(baro_path, pattern = "*.csv")
  #   
  #   list_baro_dat <- lapply(baro_csvs, function(x) {
  #     dat <- read_csv(paste(baro_path, x, sep = "/"))
  #     site <- unique(dat$site_station_code)
  #     if(length(site) == 1) {
  #       list(dat, name = site)
  #     } else {
  #       stop("More than one unique 'site_station_code' in file ", x)
  #     }
  #   })
  #   
  #   baro_dat <- bind_rows(list_baro_dat)
  #   
  #   if(select_station != "all") {
  #     p <- ggplot(data = baro_dat, aes(x = timestamp)) +
  #       geom_line(aes(y = airpress_kPa_U20))
  #     
  #   }
  # } else 
  
  ## Read in csvs and join data frames ----
  if(dissox_path != "none") {
    DO_csvs <- list.files(dissox_path, pattern = "*.csv")
    
    list_DO_dat <- list()
    
    for(x in DO_csvs) {
      
      dat <- suppressMessages(read_csv(paste(dissox_path, x, sep = "/")))
      
      site <- unique(dat$site_station_code)
      
      print(paste("Reading", x))
      
      if(length(site) != 1){
        stop("More than one unique 'site_station_code' in file ", x)
      }
      
      list_DO_dat[site] <- list(dat)
    }
    
    all_DO <- bind_rows(list_DO_dat)
    names(all_DO)[names(all_DO) == var_DO_mgL ] <- "DO_mgL"
    names(all_DO)[names(all_DO) == var_DO_percsat ] <- "DO_percsat"
    names(all_DO)[names(all_DO) == var_watertemp_C_1 ] <- "watertemp_C_1"
    names(all_DO)[names(all_DO) == var_airtemp_C ] <- "airtemp_C"
    dat <- all_DO
  } # end of reading DO files
  
  if(waterlevel_path != "none") {
    wl_csvs <- list.files(waterlevel_path, pattern = "*.csv")
    
    list_wl_dat <- list()
    
    for (x in wl_csvs) {
      # ref_waterlevel_m can trigger a parsing error due to a large amount of NAs
      # specify it to be read in as numeric
      col_types <- cols(
        "ref_waterlevel_m" = col_double()
      )
      
      dat <- suppressMessages(read_csv(paste(waterlevel_path, x, sep = "/"), col_types = col_types))
      
      # Check for parsing issues
      parsing_issues <- problems(dat)
      
      # # If there are any parsing issues, print them
      # if (nrow(parsing_issues) > 0) {
      #   print(paste("Parsing issues in file", x))
      #   print(parsing_issues)
      # }
      
      site <- unique(dat$site_station_code)
      
      print(paste("Reading", x))
      
      if(length(site) != 1){
        stop("More than one unique 'site_station_code' in file ", x)
      }
      
      list_wl_dat[site] <- list(dat)
    }
    
    all_wl <- bind_rows(list_wl_dat)
    names(all_wl)[names(all_wl) == var_waterlevel_m ] <- "waterlevel_m"
    names(all_wl)[names(all_wl) == var_watertemp_C_2 ] <- "watertemp_C_2"
    names(all_wl)[names(all_wl) == var_airtemp_C ] <- "airtemp_C"
    dat <- all_wl
  } # end of reading wl files
  
  if(conductivity_path != "none") {
    CO_csvs <- list.files(conductivity_path, pattern = "*.csv")
    
    list_CO_dat <- list()
    
    for(x in CO_csvs) {
      
      dat <- suppressWarnings(suppressMessages(read_csv(paste(conductivity_path, x, sep = "/"))))
      
      # Check for parsing issues
      parsing_issues <- problems(dat)
      
      # # If there are any parsing issues, print them
      # if (nrow(parsing_issues) > 0) {
      #   print(paste("Parsing issues in file", x))
      #   print(parsing_issues)
      # }
      
      site <- unique(dat$site_station_code)
      
      print(paste("Reading", x))
      
      if(length(site) != 1){
        stop("More than one unique 'site_station_code' in file ", x)
      }
      
      list_CO_dat[site] <- list(dat)
    }
    
    all_CO <- bind_rows(list_CO_dat)
    names(all_CO)[names(all_CO) == var_spc_uScm ] <- "spc_uScm"
    names(all_CO)[names(all_CO) == var_watertemp_C_3 ] <- "watertemp_C_3"
    dat <- all_CO
  } # end of reading conductivity files
  
  # Combine DO, water level, and conductivity files
  if(conductivity_path != "none" & waterlevel_path != "none" & dissox_path != "none"){
    all_DO <- all_DO %>% 
      select(timestamp,
             logger_sn,
             site_station_code,
             DO_mgL,
             DO_percsat,
             watertemp_C_1,
             DO_qaqc_code)
    all_wl <- all_wl %>% 
      select(timestamp,
             site_station_code,
             waterlevel_m,
             watertemp_C_2,
             airtemp_C,
             wl_qaqc_code)
    all_CO <- all_CO %>% 
      select(timestamp,
             site_station_code,
             spc_uScm,
             watertemp_C_3,
             co_qaqc_code)
    dat <- full_join(all_DO, all_wl,
                     by = c("timestamp", "site_station_code")) %>% 
      full_join(all_CO, by = c("timestamp", "site_station_code"))
  }
  
  # Combine DO & water level files
  if(conductivity_path == "none" & dissox_path != "none" & waterlevel_path != "none"){
    all_DO <- all_DO %>% 
      select(timestamp,
             logger_sn,
             site_station_code,
             DO_mgL,
             DO_percsat,
             watertemp_C_1,
             airtemp_C,
             DO_qaqc_code)
    all_wl <- all_wl %>% 
      select(timestamp,
             site_station_code,
             waterlevel_m,
             watertemp_C_2,
             airtemp_C_2 = airtemp_C,
             wl_qaqc_code)
    dat <- full_join(all_DO, all_wl,
                     by = c("timestamp", "site_station_code"))
  }
  
  # Combine conductivity & water level files
  if(dissox_path == "none" & conductivity_path != "none" & waterlevel_path != "none"){
    all_CO <- all_CO %>% 
      select(timestamp,
             logger_sn,
             site_station_code,
             spc_uScm,
             watertemp_C_3,
             co_qaqc_code)
    all_wl <- all_wl %>% 
      select(timestamp,
             site_station_code,
             waterlevel_m,
             watertemp_C_2,
             airtemp_C,
             wl_qaqc_code)
    dat <- full_join(all_CO, all_wl,
                     by = c("timestamp", "site_station_code"))
  }
  
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

  ## Site-specific plots ----
  
  if(select_station != "all"){
    
    site <- select_station
    
    dat_site <- filter(dat, site_station_code == site)
    
    # Extract floating station data if specified
    # Floating station is defined as station code "x1" where "x0" is the primary station
    # For example, "20" is the static logger and "21" is the floating logger 
    if(compare_float ==  TRUE){
      floating_site <- sub(".$", "1", site)
      
      if(!(floating_site %in% dat$site_station_code)) {
        stop(paste("No floating logger found for ", select_station))
      }
      
      dat_float <- dat %>%
        filter(site_station_code == floating_site)
      } 
    
    start_date <- floor_date(min(dat_site$timestamp), "month")
    end_date <- ceiling_date(max(dat_site$timestamp), "month")
    
    plots_list <- list()
    
    if(length(dat$timestamp) == 0) {
      stop(paste(select_station, "not found in folder. Make sure you've specified the site correctly."))
    }
    
    # Plot dissolved oxygen
    if(dissox_path != "none") {
      p <- ggplot(data = dat_site, aes(x = timestamp)) +
        scale_shape_manual(values = c("LOGGER_ICE" = 1,
                                      "LOGGER_DISTURBANCE" = 2,
                                      "LOGGER_DRY" = 4)) +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        labs(title = paste("Dissolved oxygen for:", site),
             y = "Dissolved oxygen (mg/L)",
             x = "Month",
             colour = "Logger type",
             shape = "Data flags") +
        theme_classic() +
        theme(legend.position = "top") 
      
      if(compare_float == TRUE) {
        p <- p +
          geom_line(aes(y = DO_mgL, colour = "Static")) +
          geom_line(data = dat_float, aes(y = DO_mgL, colour = "Floating")) +
          scale_colour_manual(values = c("Static" = "#006F25",
                                         "Floating" = alpha("#0CE455", 0.5)))
      } else {
        p <- p +
          geom_line(aes(y = DO_mgL), colour = "#006F25")
      }
      
      p <- p +
        geom_point(data = dat_site %>% 
                     drop_na(DO_qaqc_code) %>% 
                     filter(grepl("LOGGER", DO_qaqc_code)),
                   aes(y = 0, shape = as.factor(DO_qaqc_code)), colour = "black")
      
      # Dissolved oxygen % saturation
      q <- ggplot(data = dat_site, aes(x = timestamp)) +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        labs(title = paste("Dissolved oxygen for:", site),
             y = "Dissolved oxygen (% saturation)",
             x = "Month",
             colour = "Logger type") +
        theme_classic() +
        theme(legend.position = "top") 
      
      if(compare_float == TRUE) {
        q <- q +
          geom_line(aes(y = DO_percsat, colour = "Static")) +
          geom_line(data = dat_float, aes(y = DO_percsat, colour = "Floating")) +
          scale_colour_manual(values = c("Static" = "#5A0B3B",
                                         "Floating" = alpha("#FF1BA5", 0.5)))
      } else {
        q <- q +
          geom_line(aes(y = DO_percsat), colour = "#5A0B3B")
      }
      
      plots_list <- append(plots_list, list(p, q))
    } # end of U26 plots
    
    # Plot water level
    if(waterlevel_path != "none") {
      r <- ggplot(data = dat_site, aes(x = timestamp)) +
        geom_line(aes(y = waterlevel_m), colour = "#4A1486") +
        # geom_line(data = dat_site %>% 
        #             drop_na(wl_qaqc_code) %>% 
        #             filter(grepl("LOGGER", wl_qaqc_code)),
        #           aes(y = waterlevel_m), colour = "#42f5e6") +
        geom_point(data = dat_site %>% 
                     drop_na(wl_qaqc_code) %>% 
                     filter(grepl("LOGGER", wl_qaqc_code)),
                   aes(y = 0, shape = as.factor(wl_qaqc_code)), colour = "black") +
        scale_shape_manual(values = c("LOGGER_ICE" = 1,
                                      "LOGGER_DISTURBANCE" = 2,
                                      "LOGGER_DRY" = 4)) +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        labs(title = paste("Water level for:", site),
             y = "Water level (m)",
             x = "Month",
             colour = "",
             shape = "Data flags") +
        theme_classic() +
        theme(legend.position = "top") 
      
      plots_list <- append(plots_list, list(r))
    } # end of U20 plots
    
    # Plot water temperature
    if(plot_temp == TRUE){
      
      if(waterlevel_path != "none" | dissox_path != "none"){
        s <- ggplot(data = dat_site, aes(x = timestamp, group = logger_sn)) +
          geom_line(aes(y = airtemp_C, colour = "Air"), alpha = 0.25) +
          scale_x_datetime(limits = c(start_date, end_date),
                           breaks = "1 month",
                           date_labels = "%m") +
          labs(title = paste("Temperature for:", site),
               y = expression(paste("Temperature (", degree, "C)")),
               x = "Month",
               colour = "") +
          theme_classic() +
          theme(legend.position = "top") 
        
        if(waterlevel_path != "none"){
          s <- s +
            geom_line(aes(y = watertemp_C_2, colour = "Water (U20)"))
          
          ## U20, U26, & U24 sensors
          if(dissox_path != "none" & conductivity_path != "none"){
            if(compare_float == TRUE){
              s <- s +
                geom_line(aes(y = watertemp_C_1, colour = "Water (U26-static)")) +
                geom_line(data = dat_float, aes(y = watertemp_C_1, colour = "Water (U26-float)")) +
                scale_color_manual(values = c("Air" = "#FF7F00",
                                              "Water (U26-static)" = "#008080", 
                                              "Water (U26-float)" = "#08FFFF",
                                              "Water (U20)" = "#0856c4",
                                              "Water (U24)" = "#48D1CC"))
            }
            else{
              s <- s +
                geom_line(aes(y = watertemp_C_1, colour = "Water (U26)")) +
                geom_line(aes(y = watertemp_C_3, colour = "Water (U24)")) +
                scale_color_manual(values = c("Air" = "#FF7F00",
                                              "Water (U26)" = "#008080", 
                                              "Water (U20)" = "#0856c4",
                                              "Water (U24)" = "#48D1CC"))
            }
          }
          ## U20 & U26
          else if(dissox_path != "none"){
            if(compare_float == TRUE) {
              s <- s +
                geom_line(aes(y = watertemp_C_1, colour = "Water (U26-static)")) +
                geom_line(data = dat_float, aes(y = watertemp_C_1, colour = "Water (U26-float)")) +
                scale_color_manual(values = c("Air" = "#FF7F00",
                                              "Water (U26-static)" = "#008080", 
                                              "Water (U26-float)" = "#08FFFF",
                                              "Water (U20)" = "#0856c4")) + 
                guides(color = guide_legend(nrow = 2))
            }
            else {
              s <- s +
                geom_line(aes(y = watertemp_C_1, colour = "Water (U26)")) +
                scale_color_manual(values = c("Air" = "#FF7F00",
                                              "Water (U26)" = "#008080", 
                                              "Water (U20)" = "#0856c4"))
            }
          }
          ## U20 & U24
          else if(conductivity_path != "none"){
            s <- s +
              geom_line(aes(y = watertemp_C_2, colour = "Water (U20)")) +
              geom_line(aes(y = watertemp_C_3, colour = "Water (U24)")) +
              scale_color_manual(values = c("Air" = "#FF7F00",
                                            "Water (U24)" = "#48D1CC", 
                                            "Water (U20)" = "#0856c4"))
          }
          ## U20 only
          else{
            s <- s +
              geom_line(aes(y = watertemp_C_2, colour = "Water")) +
              scale_color_manual(values = c("Water" = "#0856c4", 
                                            "Air" = "#FF7F00"))
          }
        } # end of if U20 is present
        
        ## U26 only
        else if(dissox_path != "none"){
          if(compare_float == TRUE) {
            s <- s +
              geom_line(aes(y = watertemp_C_1, colour = "Water (U26-static)")) +
              geom_line(data = dat_float, aes(y = watertemp_C_1, colour = "Water (U26-float)")) +
              scale_color_manual(values = c("Air" = "#FF7F00",
                                            "Water (static)" = "#008080", 
                                            "Water (float)" = "#08FFFF")) + 
              guides(color = guide_legend(nrow = 2))
          }
          else {
            s <- s +
              geom_line(aes(y = watertemp_C_1, colour = "Water")) +
              scale_color_manual(values = c("Air" = "#FF7F00",
                                            "Water" = "#008080"))
          }
        }
      }
      
      # U24 only
      else{
        s <- s +
          geom_line(aes(y = watertemp_C_3), colour = "#008080") +
          scale_x_datetime(limits = c(start_date, end_date),
                           breaks = "1 month",
                           date_labels = "%m") +
          labs(title = paste("Temperature for:", site),
               y = expression(paste("Temperature (", degree, "C)")),
               x = "Month",
               colour = "") +
          theme_classic() +
          theme(legend.position = "top") 
      }
      
      plots_list <- append(plots_list, list(s))
    } # end of water temp plot
        
    
    # Plot conductivity
    if(conductivity_path != "none"){
      t <- ggplot(data = dat_site, aes(x = timestamp)) +
        geom_line(aes(y = spc_uScm), colour = "#1A85FF") +
        # geom_line(data = dat_site %>% 
        #             drop_na(co_qaqc_code) %>% 
        #             filter(grepl("LOGGER", co_qaqc_code)),
        #           aes(y = spc_uScm), colour = "#4FB6FF") +
        geom_point(data = dat_site %>% 
                     drop_na(co_qaqc_code) %>% 
                     filter(grepl("LOGGER", co_qaqc_code)),
                   aes(y = 0, shape = as.factor(wl_qaqc_code)), colour = "black") +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        scale_shape_manual(values = c("LOGGER_ICE" = 1,
                                      "LOGGER_DISTURBANCE" = 2,
                                      "LOGGER_DRY" = 4)) +
        labs(title = paste("Conductivity for:", site),
             y = expression(paste("Specific Conductance (", mu, "S/cm)")),
             x = "Month",
             colour = "",
             shape = "Data flags") +
        theme_classic() +
        theme(legend.position = "top") 
      
      plots_list <- append(plots_list, list(t))
    } # end of U24 plots
    
    
    combined_plots <- do.call(plot_grid, c(plots_list, ncol = 2))
    
    message("Specify [[1]] to return a data frame of the filtered data, [[2]] to return plots of all parameters, and [[3]] to plot all graphs individually")
    return(list(dat_site, combined_plots, plots_list))
  } # end of select_station != "all" (specify site)
  

  ## All-site plots by parameter ----
  else {
    # Exclude floating loggers
    if(compare_float != TRUE){
      dat <- dat %>% 
        filter(!grepl("1$", site_station_code))
    } 
    else
    {
      dat <- dat %>% 
        mutate(main_stn = str_sub(site_station_code, 1, -2))
      
      logger_pairs <- dat %>% 
        group_by(main_stn) %>% 
        filter(any(str_sub(site_station_code, -1, -1) == "0") & 
                 any(str_sub(site_station_code, -1, -1) == "1")) %>%
        pull(main_stn) %>% 
        unique
      
      dat <- dat %>% 
        filter(main_stn %in% logger_pairs) %>% 
        mutate(logger_type = case_when(str_sub(site_station_code, -1, -1) == "0" ~ "static",
                                       str_sub(site_station_code, -1, -1) == "1" ~ "floating",
                                       TRUE ~ NA))
    }
    
    plots_list <- list()
    plots_list <- append(plots_list, list(dat))
    
    start_date <- floor_date(min(dat$timestamp), "month")
    end_date <- ceiling_date(max(dat$timestamp), "month")
    
    # Plot dissolved oxygen
    if(dissox_path != "none") {
      p <- ggplot(data = dat, aes(x = timestamp, group = logger_sn)) +
        geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 8, ymax = 10), fill = "lightgrey", alpha = 0.25) +
        geom_line(aes(y = DO_mgL)) +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        scale_y_continuous(limits = c(0, 15),
                           breaks = seq(0, 15, by = 5),
                           minor_breaks = seq(0, 15, by = 2.5)) +
        labs(title = "Dissolved oxygen concentration",
             y = "Dissolved oxygen (mg/L)",
             x = "Month") +
        facet_wrap(~ site_station_code) +
        theme_classic()
      
      q <- ggplot(data = dat, aes(x = timestamp, group = logger_sn)) +
        geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 80, ymax = 100), fill = "lightgrey", alpha = 0.25) +
        geom_line(aes(y = DO_percsat)) +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        scale_y_continuous(limits = c(0, 150),
                           breaks = seq(0, 150, by = 50),
                           minor_breaks = seq(0, 150, by = 25)) +
        labs(title = "Dissolved oxygen saturation",
             y = "Dissolved oxygen (% saturation)",
             x = "Month") +
        facet_wrap(~ site_station_code) +
        theme_classic()
      
      plots_list <- c(plots_list, list(DO = p, DOsat = q))
    }
    
    # Plot water level and water temperature
    if(waterlevel_path != "none"){
      r <- ggplot(data = dat, aes(x = timestamp, y = waterlevel_m)) +
        geom_line() +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        labs(title = "Water level",
             y = "Water level (m)",
             x = "Month") +
        facet_wrap(~ site_station_code) +
        theme_classic()
      
      plots_list <- c(plots_list, list(waterlvl = r))
      
      # Water temperature plots
      ## U26, U24, and U20 sensors
      if(dissox_path != "none" & conductivity_path != "none") {
        s <- ggplot(data = dat, aes(x = timestamp, group = logger_sn)) +
          geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 10, ymax = 16), fill = "lightgrey", alpha = 0.25) +
          geom_line(aes(y = watertemp_C_2, colour = "Water (U20)")) +
          geom_line(aes(y = watertemp_C_1, colour = "Water (U26)")) +
          geom_line(aes(y = watertemp_C_3, colour = "Water (U24)")) +
          scale_color_manual(values = c("Water (U26)" = "#008080", 
                                        "Water (U20)" = "#0856c4",
                                        "Water (U24)" = "#FFD700")) +
          scale_x_datetime(limits = c(start_date, end_date),
                           breaks = "1 month",
                           date_labels = "%m") +
          labs(title = "Water temperature",
               y = expression(paste("Temperature (", degree, "C)")),
               x = "Month",
               colour = "") +
          facet_wrap(~ site_station_code) +
          theme_classic()
      }
      
      ## U26 and U20 sensors
      else if(dissox_path != "none") {
        s <- ggplot(data = dat, aes(x = timestamp, group = logger_sn)) +
          geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 10, ymax = 16), fill = "lightgrey", alpha = 0.25) +
          # geom_line(aes(y = airtemp_C, colour = "Air"), alpha = 0.25) +
          geom_line(aes(y = watertemp_C_2, colour = "U20")) +
          geom_line(aes(y = watertemp_C_1, colour = "U26")) +
          scale_color_manual(values = c("U26" = "#008080", 
                                        "U20" = "#0856c4")) +
          scale_x_datetime(limits = c(start_date, end_date),
                           breaks = "1 month",
                           date_labels = "%m") +
          labs(title = "Water temperature",
               y = expression(paste("Temperature (", degree, "C)")),
               x = "Month",
               colour = "") +
          facet_wrap(~ site_station_code) +
          theme_classic()
      }
      
      ## U24 and U20 sensors
      else if(conductivity_path != "none") {
        s <- ggplot(data = dat, aes(x = timestamp)) +
          geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 10, ymax = 16), fill = "lightgrey", alpha = 0.25) +
          # geom_line(aes(y = airtemp_C, colour = "Air"), alpha = 0.25) +
          geom_line(aes(y = watertemp_C_2, colour = "Water (U20)")) +
          geom_line(aes(y = watertemp_C_1, colour = "Water (U24)")) +
          scale_color_manual(values = c("Water (U24)" = "#FFD700", 
                                        "Water (U20)" = "#0856c4")) +
          scale_x_datetime(limits = c(start_date, end_date),
                           breaks = "1 month",
                           date_labels = "%m") +
          labs(title = "Water temperature",
               y = expression(paste("Temperature (", degree, "C)")),
               x = "Month",
               colour = "") +
          facet_wrap(~ site_station_code) +
          theme_classic()
      }
      
      ## U20 sensor only
      else {
        s <- ggplot(data = dat, aes(x = timestamp)) +
          geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 10, ymax = 16), fill = "lightgrey", alpha = 0.25) +
          geom_line(aes(y = airtemp_C,), alpha = 0.25) +
          geom_line(aes(y = watertemp_C_1)) +
          geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 16), fill = "lightgrey", alpha = 0.25) +
          scale_x_datetime(limits = c(start_date, end_date),
                           breaks = "1 month",
                           date_labels = "%m") +
          labs(title = "Water temperature",
               y = expression(paste("Temperature (", degree, "C)")),
               x = "Month",
               colour = "") +
          facet_wrap(~ site_station_code) +
          theme_classic()
      }
      
      plots_list <- c(plots_list, list(watertemp = s))
    } # end of water level plot
    
    # Plot conductivity
    if(conductivity_path != "none"){
      t <- ggplot(data = dat, aes(x = timestamp, y = spc_uScm)) +
        geom_line() +
        scale_x_datetime(limits = c(start_date, end_date),
                         breaks = "1 month",
                         date_labels = "%m") +
        labs(title = "Conductivity",
             y = expression(paste("Specific Conductance (", mu, "S/cm)")),
             x = "Month") +
        facet_wrap(~ site_station_code) +
        theme_classic()
      
      plots_list <- c(plots_list, list(conduct = t))
    }
    
    message("Specify [['DO']] to return plot for dissolved oxygen (mg/L), [['DOsat']] to return plot for dissolved oxygen saturation, [['waterlvl']] for water level, [['watertemp']] for water/air temperature, and [['conduct']] for conductivity.")
    
    return(plots_list)
  } # end of select_station == "all" (all sites)
}

  
  
  
#   ## Plot only water level/temperature data
#   if(waterlevel_path != "none") {
#     
#     wl_csvs <- list.files(waterlevel_path, pattern = "*.csv")
#     
#     list_wl_dat <- list()
#     
#     for (x in wl_csvs) {
#       # ref_waterlevel_m can trigger a parsing error due to a large amount of NAs
#       # specify it to be read in as numeric
#       col_types <- cols(
#         "ref_waterlevel_m" = col_double()
#       )
#       
#       dat <- suppressMessages(read_csv(paste(waterlevel_path, x, sep = "/"), col_types = col_types))
# 
#       # Check for parsing issues
#       parsing_issues <- problems(dat)
#       
#       # If there are any parsing issues, print them
#       if (nrow(parsing_issues) > 0) {
#         print(paste("Parsing issues in file", x))
#         print(parsing_issues)
#       }
#       
#       site <- unique(dat$site_station_code)
#       
#       print(paste("Reading", x))
#       
#       if(length(site) != 1){
#         stop("More than one unique 'site_station_code' in file ", x)
#       }
#       
#       list_wl_dat[site] <- list(dat)
#     }
#     
#     all_wl <- bind_rows(list_wl_dat)
#     
#     names(all_wl)[names(all_wl) == var_waterlevel_m ] <- "waterlevel_m"
#     names(all_wl)[names(all_wl) == var_watertemp_C_1 ] <- "watertemp_C"
#     names(all_wl)[names(all_wl) == var_airtemp_C ] <- "airtemp_C"
#     
#     if(select_station != "all"){
#       site <- select_station
#       
#       dat <- filter(all_wl, site_station_code == site)
#       
#       if(length(dat$timestamp) == 0) {
#         stop(paste(select_station, "not found in folder. Make sure you've specified the site correctly."))
#       }
#       
#       p <- ggplot(data = dat, aes(x = timestamp)) +
#         geom_line(aes(y = waterlevel_m, colour = "waterlevel_m")) +
#         geom_line(data = dat %>% 
#                     drop_na(wl_qaqc_code) %>% 
#                     filter(grepl("LOGGER", wl_qaqc_code)),
#                   aes(y = waterlevel_m,
#                       colour = "waterlevel_flag")) +
#         geom_point(data = dat %>% 
#                      drop_na(wl_qaqc_code) %>% 
#                      filter(grepl("LOGGER", wl_qaqc_code)),
#                    aes(y = 0, shape = as.factor(wl_qaqc_code)),
#                    colour = "black") +
#         scale_colour_manual(values = c("waterlevel_m" = "#4A1486",
#                                        "waterlevel_flag" = "#42f5e6")) +
#         scale_shape_manual(values = c("LOGGER_ICE" = 1,
#                                       "LOGGER_DISTURBANCE" = 2,
#                                       "LOGGER_DRY" = 4)) +
#         scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m-%d") +
#         labs(title = paste("Water level for:", site),
#              y = "Water level (m)",
#              x = "Month",
#              colour = "",
#              shape = "Data flags") +
#         theme_classic() +
#         theme(axis.text.x = element_text(angle = 45, hjust=1)) 
#       
#       q <- ggplot(data = dat, aes(x = timestamp)) +
#         geom_line(aes(y = airtemp_C, colour = "airtemp_C"), alpha = 0.25) +
#         geom_line(aes(y = watertemp_C, colour = "watertemp_C")) +
#         scale_color_manual(values = c("watertemp_C" = "#008080" , 
#                                       "airtemp_C" = "#E41A1C")) +
#         scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m-%d") +
#         labs(title = paste("Temperature for:", site),
#              y = expression(paste("Temperature (", degree, "C)")),
#              x = "Month", 
#              color = "") +
#         theme_classic() +
#         theme(axis.text.x = element_text(angle = 45, hjust=1)) 
#       
#       
#       combined_plots <- plot_grid(p, q, ncol = 1)
#       
#       print(combined_plots)
#     } # end of select_station != "all" (specify site)
#     
#     else{
#       p <- ggplot(data = all_wl, aes(x = timestamp, y = waterlevel_m)) +
#         geom_line(colour = "#4A1486") +
#         scale_x_datetime(date_breaks = "4 weeks", date_labels = "%m-%d") +
#         labs(title = "Water level",
#              y = "Water level (m)",
#              x = "Month") +
#         facet_wrap(~ site_station_code) +
#         theme_classic() +
#         theme(axis.text.x = element_text(angle = 45, hjust=1)) 
#       
#       q <- ggplot(data = all_wl, aes(x = timestamp)) +
#         geom_line(aes(y = airtemp_C, colour = "airtemp_C"), alpha = 0.25) +
#         geom_line(aes(y = watertemp_C, colour = "watertemp_C")) +
#         scale_color_manual(values = c("watertemp_C" = "#008080" , 
#                                       "airtemp_C" = "#E41A1C")) +
#         scale_x_datetime(date_breaks = "4 weeks", date_labels = "%m-%d") +
#         labs(title = "Water temperature",
#              y = expression(paste("Temperature (", degree, "C)")),
#              x = "Month",
#              colour = "") +
#         facet_wrap(~ site_station_code) +
#         theme_classic() +
#         theme(axis.text.x = element_text(angle = 45, hjust=1)) 
#       
#       message("Specify [[1]] to return plot for water level or [[2]] to return plot for water/air temperature")
#       
#       return(list(p, q))
#       
#     } # end of select_station == "all"
#   } # end of waterlevel plotting
#   
# }
