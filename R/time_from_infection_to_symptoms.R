# Analyzes the time from infection to symptoms.

library(tidyverse)

# Two source I'm using here:
# https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.10.2000180#html_fulltext  (see Appendix Table 2)
# https://www.acpjournals.org/doi/10.7326/M20-0504

theme_set(saridr::theme_sarid(plot.title = element_text(hjust = 1)))

symptomatic_rate <- 1 # place a value of 1 for the conditioned distribution

onset_distribution <- tibble(q = seq(0, 20, by = 0.1)) %>% 
  mutate(log_normal = symptomatic_rate*plnorm(q, meanlog = 1.621, sdlog = 0.418),
         gamma = symptomatic_rate*pgamma(q, shape = 5.807, rate = 0.948),
         weibull = symptomatic_rate*pweibull(q, shape = 2.453, scale = 6.258)) %>% 
  pivot_longer(-q) 

onset_distribution %>% 
  ggplot(aes(x = q, color = name, y = value)) + 
  geom_line() +
  coord_cartesian(ylim = c(0,1)) + 
  xlab("ימים מרגע החשיפה") + 
  ylab("הסתברות להופעת סימפטומים \n(התפלגות מצטברת)") + 
  guides(color = guide_legend("\u202bהערכות*")) + 
  scale_y_continuous(labels = scales::percent_format(1)) + 
  ggtitle("\u202bזמן עד הופעת סימפטומים ל-COVID-19") + 
  labs(caption = str_wrap("Based on Lauer S. A. et al, The Incubation Period of Coronavirus Disease 2019 (COVID-19) From Publicly Reported Confirmed Cases: Estimation and Application, Annals of Internal Medicine (May 2020).", width = 80)) + 
  theme(plot.title = element_text(hjust = 1)) + 
  geom_segment(x = 5, xend = 5.8, y = 0.5, yend = 0.5, color = "black", size = 0.25, arrow = arrow(ends = "both", length = unit(0.1, "inches"))) + 
  geom_vline(xintercept = 14)
  # geom_segment(x = 5, xend = 5, y = 0, yend = 0.5, color = "black", size = 0.25) +
  # geom_segment(x = 5.8, xend = 5.8, y = 0, yend = 0.5, color = "black", size = 0.25)

# infected_probability
# t = times so far without appearance of symptoms
# infection_intensity = the probability to get infected (i.e., sort of exposure event intensity)
# asymptomatic_rate = the probability that symptoms never appear given infection

# the function uses the bayes formula to compute the probability that you are infected even though symptoms did not appear by time t
# Let $A(t)=Symptoms didn't appear by time t$, so by Baye's rule:
# $P(infection | A(t)) = P(A(t) | infection) * \frac{P(infection)}{P(A(t))}
# Note that $P(A(t)) = P(A(t) | no infection) * P(no infection) + P(A(t) | infection) * P(infection)$
# And that $P(A(t) | no infection) = 1$, then we get
# $P(A(t)) = 1-P(infection) + P(A(t) | infection)*P(infection)$
# Finally we get
# $P(infection | A(t)) = P(A(t) | infection) * P(infection) / (1-P(infection) + P(A(t) | infection)*P(infection))$
# Which is what we use in our function.

infected_probability <- function(t, infection_intensity = 0.4, asymptomatic_rate = 0.2){
  
  P_A_t_infection <- 1-plnorm(t, meanlog = 1.621, sdlog = 0.418)*(1 - asymptomatic_rate)
  
  P_A_t <- (1 - infection_intensity) + P_A_t_infection*infection_intensity
  
  P_infection_A_t <- (P_A_t_infection * infection_intensity)/(P_A_t)
  
  return(P_infection_A_t)
  
}

infected_prop_chart <- crossing(infection_intensity = c(0.02, 0.04, 0.06, 0.08),
       t = seq(0, 16, 0.01), asymptomatic_rate = 0.2) %>% 
  mutate(infected_prob = infected_probability(t, infection_intensity, asymptomatic_rate)) %>% 
  ggplot(aes(color = factor(infection_intensity), x = t, y = infected_prob)) + 
  geom_line(size = 0.8) +
  guides(color = guide_legend("עוצמת החשיפה")) + 
  xlab("\u202bזמן שעבר מרגע החשיפה (בימים)") + 
  ylab("ההסתברות שחלית בקורונה") +
  coord_cartesian(xlim = c(2,16)) + 
  scale_color_ordinal(labels = paste0((1:4)*2, "%")) +
  scale_y_continuous(label = scales::percent_format(1), breaks = seq(0, 0.1, 0.02)) + 
  scale_x_continuous(breaks = seq(0, 16, by = 2))
