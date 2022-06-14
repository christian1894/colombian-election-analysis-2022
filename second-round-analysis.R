library(rvest)
library(httr)
library(stringr)
library(tidyverse)
library(ggridges)
library(readr)
library(lubridate)
options(digits = 3)

# first_round_table = web_page %>% html_element(".mw-parser-output > div:nth-child(18) > table:nth-child(1)")
# first_round_dataframe = html_table(first_round_table)

dataframe_name = "second-round-polls.csv"
fetch_remote_dataframe = function() {
  url = "https://es.wikipedia.org/wiki/Anexo:Sondeos_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Colombia_de_2022"
  web_page =  url %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
    read_html() 
  second_round_table_css_selector = "table.wikitable:nth-child(26)"
  second_round_table = web_page %>% html_element(second_round_table_css_selector)
  second_round_dataframe = html_table(second_round_table)
  return(second_round_dataframe)
}
clean_second_round_dataframe = function(second_round_dataframe) {
  remove_number_from_pollster_name = function(pollster_name) {
    return(str_replace(pollster_name, regex("\\[[:digit:]+]"), ""))
  }
  percentage_value_to_decimal = function(percentage_value) {
    decimal_value = str_replace(percentage_value, "%", "")
    decimal_value = str_replace(decimal_value, ",", ".")
    return(as.numeric(decimal_value)/ 100)
  }
  format_date = function(date) {
    return(str_split(date, "al ")[[1]][2])
  }
  add_spread_estimate = function(second_round_dataframe) {
    second_round_dataframe = second_round_dataframe %>% 
      mutate(spread = petro - hernandez)
  }
  
  remove_unused_rows = function() {
    return(second_round_dataframe[-c(1,1),])
  }
  select_current_month_polls = function(second_round_dataframe) {
    now = Sys.Date()
    beginning_of_month = floor_date(ymd(now), 'month')
    second_round_dataframe = second_round_dataframe %>%
      filter(as_date(dmy(date)) >= beginning_of_month)
    return(second_round_dataframe)
  }
  
  second_round_dataframe = remove_unused_rows()
  second_round_dataframe_headers = 
    c("pollster", "date", "sample_size", "moe", "petro", "hernandez", "blank_vote", "none", "no_answer", "source")
  colnames(second_round_dataframe) = second_round_dataframe_headers
  second_round_dataframe = second_round_dataframe %>%
    mutate(source = remove_number_from_pollster_name(as.character(source)))
  numeric_columns = c("hernandez", "petro", "blank_vote", "none", "no_answer", "moe")
  for (column in numeric_columns) {
    second_round_dataframe[column] = sapply(second_round_dataframe[column], percentage_value_to_decimal)
  }
  second_round_dataframe = add_spread_estimate(second_round_dataframe)
  second_round_dataframe$date = sapply(second_round_dataframe$date, format_date)
  second_round_dataframe = select_current_month_polls(second_round_dataframe)
  second_round_dataframe = second_round_dataframe %>% 
    select(pollster, sample_size, date, petro, hernandez, spread, source)
  return(second_round_dataframe)
}
save_dataframe = function(dataframe, filename) {
  write.csv(dataframe, file = filename, row.names=FALSE)
  print("Dataframe saved")
}
get_second_round_dataframe = function() {
  dataframe = fetch_remote_dataframe()
  dataframe = clean_second_round_dataframe(dataframe)
  return(dataframe)
}

second_round_dataframe = as.data.frame(read_csv(dataframe_name))

# Avoiding Pollster Bias - One Poll Per Pollster
second_round_dataframe = second_round_dataframe %>%
  group_by(source) %>%
  filter(date == max(date)) %>%
  ungroup()

# We will provide an estimate for the proportion of votes for Gustavo Petro
# let's make an estimate of the spread mean, se and a combined ci
# Note: If more polling data is provided we should consider having a single
# poll per pollster or source
combined_estimates = second_round_dataframe %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - (qnorm(0.975) * se), end = avg + (qnorm(0.975) * se))

# taken from 1994-2018 (new constitution on 1991)
historical_spread_mean = 0.18

# Bayes Theorem
mu = 0
tau = historical_spread_mean
general_bias = 0.0535 # estimated using 1994-2018 polling data
sigma = sqrt((combined_estimates$se) ^ 2 + (general_bias ^ 2))
Y = combined_estimates$avg
B = sigma^2 / (sigma^2 + tau^2)
posterior_mean = B * mu + (1 - B) * Y
posterior_se = sqrt(1 / ((1 / sigma^2) + (1 / tau^2)))
d_hat_ci = c(posterior_mean - (posterior_se * qnorm(0.975)), 
  posterior_mean + (posterior_se * qnorm(0.975)))
d_hat = 1 - pnorm(0, posterior_mean, posterior_se)

results = as.data.frame(rnorm(1000, posterior_mean, posterior_se))
colnames(results) = "spread"
results = results %>%
  mutate(petro_wins = spread > 0)

winner_plot = results %>% 
  ggplot(aes(spread, fill = petro_wins)) + 
  geom_histogram(binwidth = 0.01, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", col = "red") +
  labs(x = NULL,
       y = NULL,
       fill = "Winner",
       title ="2022 Colombian Election Second Round Winner",
       caption = paste("Last Updated: ", Sys.Date(), " - Source: Art of Code"),
       alt = "2022 Colombian Election Second Round Winner") +
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                           labels=c("Rodolfo Hernández", "Gustavo Petro")) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
