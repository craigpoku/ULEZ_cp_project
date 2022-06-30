#-----------Updated function that determines the CP based on ratio of previous to current point using the 2nd derivative of a univariant time series ----------

multi_var_ts_gradient_cp_detection_new = function(df, window_length_vector, df_header_code, 
                                              sd_value, date = TRUE){
  
  df_filter = df %>%
    filter(df_header == df_header_code)
  
  if(date==TRUE){
    roll_regression = rollRegres::roll_regres(value ~ date, df_filter, 
                                              width = window_length_vector,
                                              do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    
    roll_reformat_cp = as.data.frame(roll_regression$coefs) %>%
      rename(grad = date) %>%
      mutate(date = df_filter$date,
             r.squareds = roll_regression$r.squareds,
             data = df_filter$value,
             df_label = df_header_code,
             window_length_level = as.factor(window_length_vector),
             derv_2nd = as.numeric(pracma::gradient(grad)),
             cp = derv_2nd > mean(derv_2nd, na.rm = TRUE) + sd_value*sd(derv_2nd, na.rm = TRUE) |
                  derv_2nd < mean(derv_2nd, na.rm = TRUE) - sd_value*sd(derv_2nd, na.rm = TRUE),
             cp = as.numeric(cp),
             cp_noise_filter = ifelse(cp == 1 & lag(cp) == 0 & lead(cp) %in% c(1), cp, 0),
               cp_marker = lag(cp_noise_filter) < cp_noise_filter
      ) %>%rename("1. Input dataset" = data,
                  "2. Rolling gradient" = grad,
                  "3. 2nd derivative" = derv_2nd)%>%
      select(-"(Intercept)") %>%
      drop_na() %>%
      pivot_longer(-c(date, window_length_level,df_label, cp, cp_noise_filter, cp_marker), 
                   names_to = "variables")%>%
      mutate(variables = factor(variables, 
                                levels = c("1. Input dataset", "2. Rolling gradient", "3. 2nd derivative"
                                           , "r.squareds")))
  }
  else{
    roll_regression = rollRegres::roll_regres(value ~ index, df_filter, 
                                              width = window_length_vector,
                                              do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    
    roll_reformat_cp = as.data.frame(roll_regression$coefs) %>%
      rename(grad = index) %>%
      mutate(index = df_filter$index,
             r.squareds = roll_regression$r.squareds,
             data = df_filter$value,
             df_label = df_header_code,
             window_length_level = as.factor(window_length_vector),
             derv_2nd = as.numeric(pracma::gradient(grad)),
             #min(which(v > 100))
             cp = derv_2nd > mean(derv_2nd, na.rm = TRUE) + sd_value*sd(derv_2nd, na.rm = TRUE) |
                  derv_2nd < mean(derv_2nd, na.rm = TRUE) - sd_value*sd(derv_2nd, na.rm = TRUE),
             cp = as.numeric(cp),
             cp_noise_filter = ifelse(cp == 1 & lag(cp) == 0 & lead(cp) %in% c(1), cp, 0),
             cp_marker = lag(cp_noise_filter) < cp_noise_filter
      ) %>%rename("1. Input dataset" = data,
                  "2. Rolling gradient" = grad,
                  "3. 2nd derivative" = derv_2nd)%>%
      select(-"(Intercept)") %>%
      drop_na() %>%
      pivot_longer(-c(index, window_length_level,df_label, cp, cp_noise_filter, cp_marker), 
                   names_to = "variables")%>%
      mutate(variables = factor(variables, 
                                levels = c("1. Input dataset", "2. Rolling gradient", "3. 2nd derivative"
                                           , "r.squareds")))
  
    
  }
  return(roll_reformat_cp)
}

