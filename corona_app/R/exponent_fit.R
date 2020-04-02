# This creates an exponential fit to add to the charts

# calculates best fit model
exponent_fit <- function(data, 
                         train_set_cutoff = c(Sys.Date()-20, Sys.Date()-3)){
  train <- data %>% 
    filter(reporting_date <= train_set_cutoff[2] &
             reporting_date >= train_set_cutoff[1])
  
  train %>% 
    pivot_longer(cols = dead:total,
                 names_to = "series",
                 values_to = "cases") %>% 
    nest(data = c(reporting_date, cases)) %>% 
    mutate(corona_lm = map(data, ~{
      lm(formula = log1p(cases) ~ reporting_date, data = .x)
    })
    ) %>% 
    mutate(model_extract = map(corona_lm, ~{tibble(r_squared = summary(.x)$r.squared,
                                              intercept = .x$coefficients[1],
                                              beta_date = .x$coefficients[2])})) %>% 
    unnest(model_extract)
}

# returns predictions
exponent_predict <- function(data,
                             train_set_cutoff = c(Sys.Date()-20, Sys.Date()-3), 
                             test_set_range = Sys.Date() + 3){
  
  exp_fit <- exponent_fit(data, train_set_cutoff)
  
  exp_fit %>% 
    select(series, intercept, beta_date) %>% 
    crossing(day = as.numeric(seq(train_set_cutoff[1], test_set_range, by = "1 day"))*24*60*60) %>% 
    mutate(prediction = exp(day*beta_date + intercept)) %>% 
    select(series, day, prediction) %>% 
    mutate(series_type = paste0("fit: ", series)) %>% 
    mutate(day = as.POSIXct(as.numeric(day), origin = "1970-01-01")) %>% 
    rename(reporting_date = day) %>% 
    select(-series) %>% 
    rename(value = prediction)
  
}
