##################################################
## Project: 
## Script purpose: Decline Functions
## Date:
## Author: Gordon Blasco
##################################################

############### load packages
library(tidyverse)
library(slider) # For sliding window functions
library(trend) # For Mann-Kendall test and Sen's slope functions


############### rolling average function
rolling_average <- function(data, 
                                 window, 
                                 time_col, 
                                 value_col, 
                                 new_col) {
  
  wind_val = (window - 1)/2
  
  data %>% 
    mutate({{new_col}} := slide_dbl({{value_col}}, mean, na_rm = TRUE, .before = wind_val, .after = wind_val))
  
}


############### rolling slope functions

# creates slope extraction function
slope_extract <- function(x, y) {
  
  model <- lm(y ~ x)
  
  slope_est <- round(as.double(coef(model)[2]),5)
  
  return(as.double(slope_est))
}


# function for rolling over the slope extraction function
mutate_rolling_slope <- function(data, 
                                 window, 
                                 time_col, 
                                 value_col, 
                                 new_col = slope) {
  
  wind_val = (window - 1)/2
  
  data %>% 
    mutate(
      {{new_col}} := slide2_dbl(.x = {{time_col}} ,
                                .y = {{value_col}},
                                ~slope_extract(x = .x, y = .y), 
                                .before = wind_val, 
                                .after = wind_val,
                                .complete = FALSE)
    )
  
  
}

# Wrap in a possible in order to prevent stopping if a failure occurs. 
rolling_slope <- possibly(mutate_rolling_slope, otherwise = NA, quiet = TRUE)

# step_2_test <- all_together_test[1:5] %>% 
#   map(., rolling_slope,
#       window = 11,
#       time_col = year,
#       value_col = meaned_quant,
#       new_col = slope)

############### slope classifier

pre_classify_slope <- function(data, 
                               threshold = 50,
                               value_column,
                               slope_column, 
                               new_col) {
  
  the_val <- threshold
  
  data %>% 
    mutate(threshold = {{value_column}}/{{slope_column}}) %>%
    mutate({{new_col}} := 
             case_when(threshold >=  the_val                 ~ "stable",
                       threshold <   the_val & threshold > 0 ~ "increasing", 
                       threshold <= -the_val                 ~ "stable",
                       threshold >  -the_val & threshold < 0 ~ "decreasing"))
  
}

classify_slope <- possibly(pre_classify_slope, otherwise = "NOPE")



############### event finder

event_finder_pre <- function(data, 
                             year_col, 
                             trend_col,
                             trend_type = "decline",
                             forgiveness_window,
                             min_duration) {
  
  this_type <- case_when(
    trend_type == "decline"~"decreasing",
    trend_type == "incline"~"increasing"
  )
  
  # group declines periods that fall within the forgivness window
  prep <- data %>%  
    filter({{trend_col}} == this_type) %>% 
    mutate(year_diff = c(1, diff({{year_col}}))) %>%
    mutate(event_id =  cumsum(year_diff >= forgiveness_window)) %>%
    group_by(event_id) %>%
    summarise(
      min_year = min({{year_col}}),
      max_year = max({{year_col}}),
      duration = n()) %>%
    mutate(years_between = min_year - (lag(max_year, n = 1L))) %>%
    ungroup()%>%
    mutate(years_between = if_else(is.na(years_between), 1, years_between)) %>% 
    #mutate(decline_id2 = cumsum(years_between >= forgiveness_window))
    filter(duration >= min_duration) %>% 
    select(-duration, -years_between) %>% 
    pivot_longer(cols = c("min_year", "max_year"), names_to = "min_mix", values_to = "year") %>% 
    group_by(event_id) %>% 
    expand(year = full_seq(year, 1)) %>% 
    ungroup() %>% 
    mutate(event_id = event_id + 1)

  
 data <- data %>% 
   left_join(prep, by = "year") 
  
  
  
}

event_finder <- possibly(event_finder_pre, otherwise = "event finder failed")




######### signifcance test
# add incline and decline 

significance_test <- function(data, 
                              year_col     = year,
                              event_col    = event_id,
                              slope_filter = -0.2,
                              tau_filter   = -0.6){
  
  #if slope and tau are positive switch sign
  
  ifelse(slope_filter > 0 & tau_filter > 0, type_event <-  "incline", type_event <-  "decline") 
    
  
  trends <- data %>% 
    filter(!is.na({{event_col}})) %>% 
    group_by({{event_col}}) %>% 
    mutate(
      mk_tau       =    mk.test(na.omit(quantity))$estimates["tau"],
      mk_p         =    mk.test(na.omit(quantity))$p.value,
      mk_S         =    mk.test(na.omit(quantity))$estimates["S"], # S statistic
      sens_slope   = sens.slope(na.omit(quantity))$estimates["Sen's slope"],
      sens_slope_z = sens.slope(na.omit(quantity))$statistic["z"], # Sen's slope z statistic
      sens_slope_p = sens.slope(na.omit(quantity))$p.value # Sen's slope p-value
    ) %>% 
    ungroup() #%#>% 
    
  if (type_event == "incline") {
    trends <- trends %>% 
      mutate({{event_col}} := if_else(
        mk_tau >= tau_filter & sens_slope >= slope_filter, {{event_col}}, NA_real_)) %>% 
      select({{year_col}}, {{event_col}}, mk_tau, mk_p, mk_S, sens_slope, sens_slope_z, sens_slope_p)
  }
  
  if (type_event == "decline") {
    trends <- trends %>% 
      mutate({{event_col}} := if_else(
        mk_tau <= tau_filter & sens_slope <= slope_filter, {{event_col}}, NA_real_)) %>% 
      select({{year_col}}, {{event_col}}, mk_tau, mk_p, mk_S, sens_slope, sens_slope_z, sens_slope_p)
  }
    

  data %>% 
    select(-{{event_col}}) %>% 
    left_join(trends, by = "year")
}


