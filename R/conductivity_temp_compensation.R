#' Conductivity Temperature Compensation
#'
#' Applies temperature correction to conductivity measurements to calculate specific conductance
#' standardized at 25°C. Uses a non-linear temperature compensation method for natural waters,
#' equivalent to ISO 7888.
#'
#' @param input_data Data frame. Contains the conductivity and temperature data.
#' @param comp_file File path. Path to the temperature compensation table (e.g., 
#'   \code{inst/extdata/tempcomp27888.csv}). This argument may be deprecated once the function is fully packaged,
#'   as the compensation data should be accessed internally via \code{system.file()}.
#' @param var_conduct_uScm Character. Name of the column containing conductivity values (in µS/cm).
#' @param var_watertemp_C Character. Name of the column containing temperature values (in °C).
#'
#' @return A data frame with an additional column containing the corrected specific conductance.
#' @export

conduct_temp_comp <- function(input_data,
                              comp_file,
                              var_conduct_uScm = "conduct_uScm_U24_adj",
                              var_watertemp_C = "watertemp_C_U24_adj") {
  
  require(tidyverse)
  
  ## Using non-linear Temperature Correction Factors
  ## From Mettler-Toledo Conductivity Guide
  tcomp <- as.data.frame(read_csv(comp_file))
  rownames(tcomp) <- tcomp[,1]
  tcomp <- tcomp %>% 
    pivot_longer(cols = `0`:`0.9`,
                 names_to = "Tsub",
                 values_to = "coef")
  tcomp <- tcomp %>% 
    mutate(temp = Temp_C +as.numeric(as.character(Tsub))) %>% 
    select(temp,
           coef) %>% 
    arrange(temp)
  
  model5 <- lm(coef ~ temp + I(temp^2) + I(temp^3) + I(temp^4) + I(temp^5), data = tcomp)
  
  coefficients <- coef(model5)
  
  tempcomp <- function(x) {
    coefficients[1] + 
      coefficients[2]*x + 
      coefficients[3]*x^2 + 
      coefficients[4]*x^3 + 
      coefficients[5]*x^4 + 
      coefficients[6]*x^5
  }
  
  ## Calculate specific conductance
  # rename input columns to local columns
  names(input_data)[names(input_data) == var_conduct_uScm ] <- "conduct_uScm"
  names(input_data)[names(input_data) == var_watertemp_C ] <- "watertemp_C"
  
  output_data <- input_data %>% 
    mutate(spc_uScm = tempcomp(watertemp_C) * conduct_uScm)
  
  # return variables to user input naming
  names(output_data)[names(output_data) == "conduct_uScm"] <- var_conduct_uScm
  names(output_data)[names(output_data) == "watertemp_C"] <- var_watertemp_C
  
  return(output_data)
}