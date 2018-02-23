

# Data Tables for Joan

library(tidyverse)


load("data-derived/acs_15_5yr_hardship.RData")


displaytable %>%
    select(community, census_tract, county, hardship_index, ma_percentile, everything()) %>%
    write_csv("data-summaries/index_all_tracts.csv", na = "")


mydata <- displaytable %>%
    filter(!is.na(community)) %>%
    select(community, census_tract, county, hardship_index, ma_percentile, everything()) %>%
    write_csv("data-summaries/index_by_census_tract.csv", na = "")

mydata_pop <- displaytable %>%
    filter(!is.na(community)) %>%
    group_by(community) %>%
    summarise(n = n(),
              total_population = sum(pop_total))

mydata_grouped <- displaytable %>%
    filter(!is.na(community)) %>%
    group_by(community) %>%
    summarise_at(vars(percapita_income, percent_not_hs_grad, percent_unemployed, percent_dependent,
                      percent_crowding, percent_poverty, hardship_index, ma_percentile), 
                 funs(mean, sd)) %>%
    select(community, 
           hardship_index_mean, hardship_index_sd,
           ma_percentile_mean, ma_percentile_sd,
           percapita_income_mean, percapita_income_sd,
           percent_not_hs_grad_mean, percent_not_hs_grad_sd,
           percent_unemployed_mean, percent_unemployed_sd,
           percent_dependent_mean, percent_dependent_sd,
           percent_poverty_mean, percent_poverty_sd,
           percent_crowding_mean, percent_crowding_sd) %>%
    left_join(mydata_pop, by = "community") %>%
    write_csv("data-summaries/index_avg_by_community.csv", na = "")


