#' CONDUCTIVITY TEMPERATURE COMPENSATION (SPECIFIC CONDUCTANCE)
#' 

conduct_temp_comp <- function(input_data,
                              comp_file,
                              var_conduct_uScm = "conduct_uScm_U24_adj",
                              var_watertemp_C = "watertemp_C_U24_adj") {
  
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