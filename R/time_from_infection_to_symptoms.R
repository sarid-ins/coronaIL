# Analyzes the time from infection to symptoms.

library(tidyverse)

# Two source I'm using here:
# https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.10.2000180#html_fulltext
# https://www.acpjournals.org/doi/10.7326/M20-0504

asymptomatic_rate <- 0.8

onset_distribution <- tibble(q = seq(0, 20, by = 0.1)) %>% 
  mutate(log_normal = asymptomatic_rate*plnorm(q, meanlog = 1.621, sdlog = 0.418),
         gamma = asymptomatic_rate*pgamma(q, shape = 5.807, rate = 0.948),
         weibull = asymptomatic_rate*pweibull(q, shape = 2.453, scale = 6.258)) %>% 
  pivot_longer(-q) 

onset_distribution %>% 
  ggplot(aes(x = q, y = plnorm)) + 
  geom_point() + 
  coord_cartesian(ylim = c(0,1))

