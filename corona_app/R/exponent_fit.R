# This creates an exponential fit to add to the charts

exponent_fit <- function(data, 
                         train_set_cutoff = c(Sys.Date()-20, Sys.Date()-3), 
                         h = Sys.Date()){
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
    )
}
