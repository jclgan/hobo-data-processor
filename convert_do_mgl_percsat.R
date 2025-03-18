#' Dissolved oxygen conversion
#'
#' Convert dissolved oxygen from mg/L to percent air saturation
#' @param input_data Dataframe of time series of dissolved oxygen in mg/L, temperature in degrees Celcius, and barometric pressure in kPa
#' @param var_watertemp_C The variable for watertemperature in degrees Celcius
#' @param  var_airpress_kPa The variable for barometric pressure in kPa
#' @param  var_do_mgL The variable for dissolved oxygen in mg per litre
#' @return output_data: data frame with dissolved oxygen in percent air saturation as a new column "do_percsat_U26"
#' @examples 
#' data("do.baro")
#' site_do_baro <- convert_do_mgL_percsat(site_df = do.baro)
#' @export

convert_DO_mgL_percsat <- function(input_data, var_watertemp_C = "watertemp_C_U26", var_airpress_kPa = "airpress_kPa_U20_adj", var_DO_mgL = "DO_mgL_U26") {
  #library(dplyr)
  
  # assign dataframe of hourly DO measurements in mg/L and barometric pressure measurements in kPa
  output_data <- input_data
  output_data$DO_percsat <- NA
  
  # change user input columns to local names
  names(output_data)[names(output_data) == var_DO_mgL ] <- "DO_mgL"
  names(output_data)[names(output_data) == var_airpress_kPa ] <- "airpress_kPa"
  names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"

  for(i in 1:length(output_data$timestamp)){
    if(!is.na(output_data$DO_mgL[i])){
      # skip logger error
      if(output_data$DO_mgL[i]== -888.88){
        output_data$DO_percsat[i] <- NA
      } else{
        # first convert temp (C -> K) and pressure (kPa -> atm)
        temp_K = 273.15 + output_data$watertemp_C[i] # absolute temperature, K
        press_atm = output_data$airpress_kPa[i]/101.325 
        
        # then calculate oxygen solubility in mg/L at 1 atm using the Benson and Krause Equations
        DO_1atm = exp(-139.34411+((1.575701*10^5)/temp_K)-((6.642308*10^7)/temp_K^2)+((1.243800*10^10)/temp_K^3)-(8.621949*10^11)/(temp_K^4))
        
        # get vapour pressure of water in atm, Benson and Krause (1980)
        u_atm = exp(11.8571-(3840.70/temp_K)-(216961/temp_K^2))
        
        # calculate theta related to the second virial coefficient of oxygen
        theta_o = 0.000975-1.426*10^(-5)*output_data$watertemp_C[i] + 6.436*10^(-8)*output_data$watertemp_C[i]^2 
        
        # calculate pressure factor
        F_p = ((press_atm-u_atm)*(1-theta_o*press_atm))/((1-u_atm)*(1-theta_o))
        
        # apply pressure correction to get do sat at present pressure
        DO_sat_real = DO_1atm*F_p
        
        # take ratio to get percent saturation
        output_data$DO_percsat[i]<- signif(100*(output_data$DO_mgL[i]/DO_sat_real),4)
        
      } # end of else - skip errors
    } # end of skip na values in DO_mgL
  } # end of for loop
  
  # #revert naming of columns to user input    
  names(output_data)[names(output_data) == "DO_mgL" ] <- var_DO_mgL
  names(output_data)[names(output_data) == "airpress_kPa" ] <- var_airpress_kPa
  names(output_data)[names(output_data) == "watertemp_C" ] <- var_watertemp_C
  
  # internal rename for our dataset: does not apply if do variable is named differently
  if(any(colnames(output_data)=="DO_mgL_U26")){
    names(output_data)[names(output_data) == "DO_percsat" ] <- "DO_percsat_U26"
  }
  
  
  return(output_data)
}

