

library(tidyverse)

# Data by Census Tract -----------------------------------------------------------------------------

#income
inc_dat <- read_csv("data-raw/ACS_15_5YR_B19301_income/ACS_15_5YR_B19301_with_ann.csv") %>%
    rename(income_est = HD01_VD01,
           income_moe = HD02_VD01,
           geo_display = `GEO.display-label`) %>%
    filter(row_number() != 1) %>%
    mutate(income_est = as.numeric(income_est),
           income_moe = as.numeric(income_moe))
# age
age_dat <- read_csv("data-raw/ACS_15_5YR_S0101_age/ACS_15_5YR_S0101_with_ann.csv") %>%
    select(GEO.id2,
           age18orgreater_est = HC01_EST_VC28,
           age18orgreater_moe = HC01_MOE_VC28,
           age65orgreater_est = HC01_EST_VC31,
           age65orgreater_moe = HC01_MOE_VC31) %>%
    filter(row_number() != 1) %>%
    mutate(age18orgreater_est = as.numeric(age18orgreater_est),
           age18orgreater_moe = as.numeric(age18orgreater_moe),
           age65orgreater_est = as.numeric(age65orgreater_est),
           age65orgreater_moe = as.numeric(age65orgreater_moe)) %>%
    mutate(age_under18_over65 = 100 - (age18orgreater_est - age65orgreater_est))

# education
edu_dat <- read_csv("data-raw/ACS_15_5YR_S1501_education/ACS_15_5YR_S1501_with_ann.csv") %>%
    select(GEO.id2,
           lesshs_est = HC02_EST_VC09,
           lesshs_moe = HC02_MOE_VC09,
           lessdiploma_est = HC02_EST_VC10,
           lessdiploma_moe = HC02_MOE_VC10) %>%
    filter(row_number() != 1) %>%
    mutate(lesshs_est = as.numeric(lesshs_est),
           lesshs_moe = as.numeric(lesshs_moe),
           lessdiploma_est = as.numeric(lessdiploma_est),
           lessdiploma_moe = as.numeric(lessdiploma_moe),
           nothsgrad = lesshs_est + lessdiploma_est)
    
# poverty
pov_dat <- read_csv("data-raw/ACS_15_5YR_S1701_poverty/ACS_15_5YR_S1701_with_ann.csv") %>%
    select(GEO.id2,
           pov_est = HC03_EST_VC01,
           pov_moe = HC03_MOE_VC01) %>%
    filter(row_number() != 1) %>%
    mutate(pov_est = as.numeric(pov_est),
           pov_moe = as.numeric(pov_moe))

# employment
emp_dat <- read_csv("data-raw/ACS_15_5YR_S2301_employment/ACS_15_5YR_S2301_with_ann.csv") %>%
    select(GEO.id2,
           unemploy_est = HC04_EST_VC01,
           unemploy_moe = HC04_MOE_VC01) %>%
    filter(row_number() != 1) %>%
    mutate(unemploy_est = as.numeric(unemploy_est),
           unemploy_moe = as.numeric(unemploy_moe))

# get population totals
pop_dat <- read_csv("data-raw/ACS_15_5YR_S0101_age/ACS_15_5YR_S0101_with_ann.csv") %>%
    filter(row_number() != 1) %>%
    mutate(pop_total = as.numeric(HC01_EST_VC01)) %>%
    select(GEO.id2, pop_total) %>%
    mutate(include = ifelse(pop_total > 500, 1, 0)) # I only want to include census tracts that have a population of > 500

# crowding
crw_dat <- read_csv("data-raw/ACS_15_5YR_B25014_crowding/ACS_15_5YR_B25014.csv") %>%
    mutate(total_units = HD01_VD01,
           lte_1_owner = HD01_VD03 + HD01_VD04,
           lte_1_rent = HD01_VD09 + HD01_VD10,
           total_gte1 = total_units - lte_1_owner - lte_1_rent,
           total_gte1_pct = round((total_gte1/total_units)*100,1),
           total_gte1_pct = ifelse(is.nan(total_gte1_pct), 0, total_gte1_pct),
           GEO.id2 = as.character(GEO.id2)) %>%
    select(GEO.id2, total_gte1, total_gte1_pct)

# race/ethnicity
eth_dat <- read_csv("data-raw/ACS_15_5YR_B03002_race_eth/ACS_15_5YR_B03002.csv") %>%
    mutate(white = round((HD01_VD03/HD01_VD01)*100, 1),
           black =  round((HD01_VD04/HD01_VD01)*100, 1),
           hispanic =  round((HD01_VD12/HD01_VD01)*100, 1),
           american_indian =  round((HD01_VD05/HD01_VD01)*100, 1),
           asian_nhpi =  round(((HD01_VD06 + HD01_VD07)/HD01_VD01)*100, 1), 
           other =  round((HD01_VD08/HD01_VD01)*100, 1),
           biracial =  round((HD01_VD09/HD01_VD01)*100, 1),
           GEO.id2 = as.character(GEO.id2)) %>%
    select(GEO.id2, white:biracial)


# all stats
acs_15_5yr <- full_join(inc_dat, edu_dat, by = "GEO.id2") %>%
    full_join(emp_dat, by = "GEO.id2") %>%
    full_join(age_dat, by = "GEO.id2") %>%
    full_join(pov_dat, by = "GEO.id2") %>%
    full_join(pop_dat, by = "GEO.id2") %>%
    full_join(crw_dat, by = "GEO.id2") %>%
    full_join(eth_dat, by = "GEO.id2") %>%
    filter(include == 1) %>% 
    mutate(min_inc = min(income_est, na.rm = TRUE),
           max_inc = max(income_est, na.rm = TRUE),
           min_edu = min(nothsgrad, na.rm = TRUE),
           max_edu = max(nothsgrad, na.rm = TRUE),
           min_emp = min(unemploy_est, na.rm = TRUE),
           max_emp = max(unemploy_est, na.rm = TRUE),
           min_age = min(age_under18_over65, na.rm = TRUE),
           max_age = max(age_under18_over65, na.rm = TRUE),
           min_pov = min(pov_est, na.rm = TRUE),
           max_pov = max(pov_est, na.rm = TRUE),
           min_crw = min(total_gte1_pct, na.rm = TRUE),
           max_crw = max(total_gte1_pct, na.rm = TRUE)) %>%
    rowwise() %>%
    mutate(stan_inc = (income_est - max_inc)/(min_inc - max_inc)*100,
           stan_edu = (nothsgrad - min_edu)/(max_edu - min_edu)*100,
           stan_emp = (unemploy_est - min_emp)/(max_emp - min_emp)*100,
           stan_age = (age_under18_over65 - min_age)/(max_age - min_age)*100,
           stan_pov = (pov_est - min_pov)/(max_pov - min_pov)*100,
           stan_crw = (total_gte1_pct - min_crw)/(max_crw - min_crw)*100,
           hardship_index_rawscore = (stan_inc + stan_edu + stan_emp + stan_age + stan_pov + stan_crw)/6) %>%
    ungroup() %>%
    arrange(hardship_index_rawscore) %>%
    filter(!is.na(hardship_index_rawscore)) %>%
    mutate(hardship_index_scaled = scales::rescale(hardship_index_rawscore, to = c(0,100)),
           hardship_index = percent_rank(hardship_index_scaled)*100)

displaytable <- acs_15_5yr %>%
    separate(geo_display, sep = ",", into = c("census_tract", "county", "state")) %>%
    mutate(census_tract = parse_number(census_tract)) %>%
    select(census_tract,
           county,
           hardship_index,
           hardship_index_rawscore,
           pop_total,
           percapita_income = income_est,
           percent_not_hs_grad = nothsgrad,
           percent_unemployed = unemploy_est,
           percent_dependent  = age_under18_over65,
           percent_poverty = pov_est,
           percent_crowding = total_gte1_pct) %>%
    mutate(community = case_when(
        census_tract %in% c(1606.02, 1606.01, 1605.02, 1605.01, 1604, 1601.01, 1602, 1603) ~ "Chelsea",
        census_tract %in% c(1011.01, 1011.02, 1010.02, 1009, 1010.01) ~ "Mattapan",
        census_tract %in% c(907, 913, 912, 911, 910.01, 909.01, 914, 915, 903, 918, 917, 916, 
                              901, 902, 919, 920, 911.01, 924, 923, 922, 1001, 1005,
                              1002, 1003, 1004, 1006.01, 1006,03, 1008, 1007) ~ "Dorchester",
        census_tract %in% c(108.02, 108.01, 107.01, 107.02, 105, 106) ~ "Back Bay",
        census_tract %in% c(202, 201.01, 203.02, 9817) ~ "Beacon Hill",
        census_tract %in% c(1301, 1302, 1106.01, 1303, 1304.02, 1304.04, 1304.06) ~ "West Roxbury",
        census_tract %in% c(406, 403, 402, 408.01, 404.01, 401, 404.01) ~ "Charlestown",
        census_tract %in% c(501.01, 509.01, 510, 511.01, 502, 507, 503, 506, 505, 504, 512) ~ "East Boston",
        census_tract %in% c(1703, 1704, 1705.02, 1705.01, 1702, 1701, 1706.01, 1707.02, 
                              1707.01, 1708) ~ "Revere",
        census_tract %in% c(3543, 3546, 3549, 3550, 3548, 3544, 3542, 3545, 3547, 3541, 3540, 
                              3536, 3539, 3537, 3535, 3538, 3529, 3534, 3530, 3528, 3527, 3533, 
                              3532, 3531.01, 3525, 3531.02, 3524, 3526, 3523, 3522, 3521.02, 
                              3521.01) ~ "Cambridge",
        census_tract %in% c(3507, 3508, 3505, 3506, 3509, 3504, 3510, 3503, 3512.04, 
                              3511, 3502, 3501.04, 3512.03, 3513, 3514.04, 3501.03, 3515, 
                              3514.03) ~ "Somerville",
        census_tract %in% c(612, 607, 608, 611.01, 610, 605.01, 604, 603.01, 605.01, 602, 
                              601.01) ~ "South Boston",
        census_tract %in% c(301, 302, 304, 305) ~ "North End",
        census_tract %in% c(203.03, 303, 701.01, 702) ~ "Downtown/Chinatown/LeatherDistrict",
        census_tract %in% c(707, 703, 704.02, 705, 706, 708, 709, 711.01, 712.01) ~ "South End",
        census_tract %in% c(805, 806.01, 804.01, 801, 803, 906, 814, 817, 818, 904,
                              813, 815, 819, 820, 821) ~ "Roxbury",
        census_tract %in% c(8.03, 8.02, 7.04, 7.03) ~ "Allston",
        census_tract %in% c(1, 3.01, 2.01, 2.02, 6.01, 6.02, 7.01,
                            3.02, 4.02, 4.01, 5.04, 5.03, 5.02) ~ "Brighton",
        census_tract %in% c(101.03, 101.04, 102.04, 102.03, 104.08, 
                            104.03, 104.04, 104.05) ~ "Fenway",
        census_tract %in% c(103) ~ "LMA",
        census_tract %in% c(810.01, 809, 808.01, 811) ~ "Mission Hill",
        census_tract %in% c(203.01) ~ "West End",
        census_tract %in% c(1207, 812, 9818, 1204, 1206, 1205, 1201.05, 1201.03,
                            1201.04, 1202.01, 1101.03) ~ "Jamaica Plain",
        census_tract %in% c(1106.07, 1104.01, 1103.01, 1105.01, 1105.02, 
                            1401.06, 1104.03, 1102.01) ~ "Roslindale",
        census_tract %in% c(1401.05, 1404, 1401.07, 1403, 1401.02, 1402.01, 1402.02) ~ "Hyde Park"),
        hardship_index = round(hardship_index, 2))

displaytable <- as.data.frame(displaytable)

rm(age_dat, edu_dat, emp_dat, inc_dat, pop_dat, pov_dat, crw_dat, eth_dat)

acs_15_5yr <- rename(acs_15_5yr, GEOID = GEO.id2)
tracts <- readOGR(dsn = "data-shapefiles/tl_2015_25_tract/tl_2015_25_tract.shp")
sp_data <- sp::merge(tracts, acs_15_5yr, by = "GEOID")

rm(tracts, acs_15_5yr)

save.image(file = "data-derived/acs_15_5yr_hardship.RData")

rm(sp_data, displaytable)





# DATA by Zip Code ---------------------------------------------------------------------------------
## not used in Shniy APP


# income
inc_dat <- read_csv("data-raw/zipcode_data/ACS_15_5YR_B19301_inc/ACS_15_5YR_B19301.csv") %>%
    rename(income_est = HD01_VD01,
           income_moe = HD02_VD01,
           geo_display = `GEO.display-label`)

# age
age_dat <- read_csv("data-raw/zipcode_data/ACS_15_5YR_S0101_age/ACS_15_5YR_S0101.csv") %>%
    select(GEO.id2,
           age18orgreater_est = HC01_EST_VC28,
           age18orgreater_moe = HC01_MOE_VC28,
           age65orgreater_est = HC01_EST_VC31,
           age65orgreater_moe = HC01_MOE_VC31,
           pop_total = HC01_EST_VC01) %>%
    mutate(age_under18_over65 = 100 - (age18orgreater_est - age65orgreater_est),
           pop_gt_500 = ifelse(pop_total > 500, 1, 0))

# education
edu_dat <- read_csv("data-raw/zipcode_data/ACS_15_5YR_S1501_edu/ACS_15_5YR_S1501.csv") %>%
    select(GEO.id2,
           lesshs_est = HC02_EST_VC09,
           lesshs_moe = HC02_MOE_VC09,
           lessdiploma_est = HC02_EST_VC10,
           lessdiploma_moe = HC02_MOE_VC10) %>%
    mutate(nothsgrad = lesshs_est + lessdiploma_est)

# poverty
pov_dat <- read_csv("data-raw/zipcode_data/ACS_15_5YR_S1701_pov/ACS_15_5YR_S1701.csv") %>%
    select(GEO.id2,
           pov_est = HC03_EST_VC01,
           pov_moe = HC03_MOE_VC01) 

# employment
emp_dat <- read_csv("data-raw/zipcode_data/ACS_15_5YR_S2301_unemploy/ACS_15_5YR_S2301.csv") %>%
    select(GEO.id2,
           unemploy_est = HC04_EST_VC01,
           unemploy_moe = HC04_MOE_VC01)

# crowding
crw_dat <- read_csv("data-raw/zipcode_data/ACS_15_5YR_B25014_crowd/ACS_15_5YR_B25014.csv") %>%
    mutate(total_units = HD01_VD01,
           lte_1_owner = HD01_VD03 + HD01_VD04,
           lte_1_rent = HD01_VD09 + HD01_VD10,
           total_gte1 = total_units - lte_1_owner - lte_1_rent,
           total_gte1_pct = round((total_gte1/total_units)*100,1),
           total_gte1_pct = ifelse(is.nan(total_gte1_pct), 0, total_gte1_pct)) %>%
    select(GEO.id2, total_gte1, total_gte1_pct)

# combine
acs_15_5yr <- full_join(inc_dat, edu_dat, by = "GEO.id2") %>%
    full_join(emp_dat, by = "GEO.id2") %>%
    full_join(age_dat, by = "GEO.id2") %>%
    full_join(pov_dat, by = "GEO.id2") %>%
    full_join(crw_dat, by = "GEO.id2") %>%
    filter(pop_gt_500 == 1) %>% 
    mutate(min_inc = min(income_est, na.rm = TRUE),
           max_inc = max(income_est, na.rm = TRUE),
           min_edu = min(nothsgrad, na.rm = TRUE),
           max_edu = max(nothsgrad, na.rm = TRUE),
           min_emp = min(unemploy_est, na.rm = TRUE),
           max_emp = max(unemploy_est, na.rm = TRUE),
           min_age = min(age_under18_over65, na.rm = TRUE),
           max_age = max(age_under18_over65, na.rm = TRUE),
           min_pov = min(pov_est, na.rm = TRUE),
           max_pov = max(pov_est, na.rm = TRUE),
           min_crw = min(total_gte1_pct, na.rm = TRUE),
           max_crw = max(total_gte1_pct, na.rm = TRUE)) %>%
    rowwise() %>%
    mutate(stan_inc = (income_est - max_inc)/(min_inc - max_inc)*100,
           stan_edu = (nothsgrad - min_edu)/(max_edu - min_edu)*100,
           stan_emp = (unemploy_est - min_emp)/(max_emp - min_emp)*100,
           stan_age = (age_under18_over65 - min_age)/(max_age - min_age)*100,
           stan_pov = (pov_est - min_pov)/(max_pov - min_pov)*100,
           stan_crw = (total_gte1_pct - min_crw)/(max_crw - min_crw)*100,
           hardship_index = (stan_inc + stan_edu + stan_emp + stan_age + stan_pov + stan_crw)/6) %>%
    ungroup() %>%
    arrange(hardship_index) %>%
    filter(!is.na(hardship_index)) %>%
    mutate(ranking = row_number(),
           ma_percentile = round((ranking/max(ranking))*100, 0),
           hardship_index = round(hardship_index, 1))


displaytable <- acs_15_5yr %>%
    select(GEO.id,
           GEO.id2,
           percapita_income = income_est,
           percent_not_hs_grad = nothsgrad,
           percent_unemployed = unemploy_est,
           percent_dependent = age_under18_over65,
           percent_poverty = pov_est,
           percent_crowding = total_gte1_pct,
           hardship_index,
           ma_percentile) 

displaytable <- as.data.frame(displaytable)

rm(age_dat, edu_dat, emp_dat, inc_dat, pop_dat, pov_dat, crw_dat)

save.image(file = "data-derived/acs_15_5yr_hardship_byzip.RData")












