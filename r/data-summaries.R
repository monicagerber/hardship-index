
# Data for Joan

library(tidyverse)

load("data-derived/acs_15_5yr_hardship.RData")

sample_size <- displaytable %>%
    filter(!is.na(community)) %>%
    group_by(community) %>%
    summarise(n = n(),
              pop = sum(pop_total))

mytable <- displaytable %>%
    filter(!is.na(community)) %>%
    group_by(community) %>%
    summarise_at(vars(percapita_income, percent_not_hs_grad, percent_unemployed,
                      percent_dependent, percent_poverty, percent_crowding,
                      hardship_index, ma_percentile), funs(mean, sd)) %>%
    left_join(sample_size, by = "community") %>%
    select(community, n, pop, hardship_index_mean, hardship_index_sd,
           ma_percentile_mean, ma_percentile_sd,
           percapita_income_mean, percapita_income_sd,
           percent_not_hs_grad_mean, percent_not_hs_grad_sd,
           percent_unemployed_mean, percent_unemployed_sd,
           percent_dependent_mean, percent_dependent_sd,
           percent_poverty_mean, percent_poverty_sd,
           percent_crowding_mean, percent_crowding_sd) %>%
    write_csv("data-summaries/index_avg_by_community.csv", na = "")
    

table_by_tract <- displaytable %>%
    filter(!is.na(community)) %>%
    group_by(community) %>%
    select(community, hardship_index, everything()) %>%
    arrange(community, hardship_index)
