library(rvest)
library(httr)
library(stringr)
library(tidyverse)
options(digits = 3)

# first_round_table = web_page %>% html_element(".mw-parser-output > div:nth-child(18) > table:nth-child(1)")
# first_round_dataframe = html_table(first_round_table)

url = "https://es.wikipedia.org/wiki/Anexo:Sondeos_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Colombia_de_2022"
web_page =  url %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html() 
clean_second_round_dataframe = function(second_round_dataframe) {
  remove_number_from_pollster_name = function(pollster_name) {
    return(str_replace(pollster_name, regex("\\[[:digit:]+]"), ""))
  }
  percentage_value_to_decimal = function(percentage_value) {
    decimal_value = str_replace(percentage_value, "%", "")
    decimal_value = str_replace(decimal_value, ",", ".")
    return(as.numeric(decimal_value)/ 100)
  }
  add_spread_estimate = function(dataframe) {
    dataframe = dataframe %>% 
      mutate(spread = petro - gutierrez)
  }
  
  second_round_dataframe_headers = 
    c("pollster", "date", "sample_size", "gutierrez", "petro", "blank_vote", "none", "no_answer", "moe", "source")
  colnames(second_round_dataframe) = second_round_dataframe_headers
  second_round_dataframe = second_round_dataframe %>%
    mutate(source = remove_number_from_pollster_name(as.character(source)))
  numeric_columns = c("gutierrez", "petro", "blank_vote", "none", "no_answer", "moe")
  for (column in numeric_columns) {
    second_round_dataframe[column] = sapply(second_round_dataframe[column], percentage_value_to_decimal)
  }
  second_round_dataframe = add_spread_estimate(second_round_dataframe)
  second_round_dataframe = second_round_dataframe %>% 
    select(pollster, sample_size, date, petro, gutierrez, spread, source)
  return(second_round_dataframe)
}

second_round_table_css_selector = "table.wikitable:nth-child(37)"
second_round_table = web_page %>% html_element(second_round_table_css_selector)
second_round_dataframe = html_table(second_round_table)
second_round_dataframe = clean_second_round_dataframe(second_round_dataframe)

# We will provide an estimate for the proportion of votes for Gustavo Petro
# let's make an estimate of the spread mean, se and a combined ci
# Note: If more polling data is provided we should consider having a single
# poll per pollster or source
results = second_round_dataframe %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - (qnorm(0.975) * se), end = avg + (qnorm(0.975) * se))

# taken from 1994-2018 (new constitution on 1991)
historical_spread_mean = 0.18

# Bayes Theorem
mu = 0
tau = historical_spread_mean
general_bias = 0.025 # can be estimated later with historical polling data
sigma = sqrt((results$se) ^ 2 + (general_bias ^ 2))
Y = results$avg
B = sigma^2 / (sigma^2 + tau^2)
posterior_mean = B * mu + (1 - B) * Y
posterior_se = sqrt(1 / ((1 / sigma^2) + (1 / tau^2)))
c(posterior_mean - (posterior_se * qnorm(0.975)), 
  posterior_mean + (posterior_se * qnorm(0.975)))
1 - pnorm(0, posterior_mean, posterior_se)
