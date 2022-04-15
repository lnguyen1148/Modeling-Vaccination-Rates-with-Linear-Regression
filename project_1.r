# CPSC 375 - 02
# Linh Nguyen
# Project 1

library(tidyverse)
library(modelr)

# Read datasets
vaccine_doses <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
hospital_beds <- read_csv("hospital_beds.csv")
demographics <- read_csv("demographics.csv")

# Discard unneeded data
vaccine_doses <- vaccine_doses %>% filter(is.na(Province_State))
vaccine_doses <- vaccine_doses %>% select(-c(1:7, 9:11))

#Tidy up vaccine_doses table
vaccine_doses <- vaccine_doses %>% pivot_longer(cols = -c(1,2), names_to = 'Date', values_to = 'Num_of_shots')
vaccine_doses <- vaccine_doses %>% filter(!is.na(Num_of_shots) & Num_of_shots != 0)

# Calculations
vaccine_doses %>% mutate(vaccine_rate = Num_of_shots / Population)
vaccine_doses <- vaccine_doses %>% group_by(Country_Region) %>% mutate(day_since_start = 1:n())

# Discard unneeded data of hospital beds. Keep the most recent 5 years
hospital_beds <- hospital_beds %>% group_by(Country) %>% arrange(desc(Year)) %>% slice(1:5)

# Discard unneeded data of demographics
demographics <- demographics %>% select(c(1, 4, 5))

# Tidy up demographics table
demographics %>% pivot_wider(names_from = `Series Code`, values_from = YR2015)

# Add up female and male population
demographics <- demographics %>% mutate(SP.POP.80UP = SP.POP.80UP.FE + SP.POP.80UP.MA)
demographics <- demographics %>% mutate(SP.POP.1564.IN = SP.POP.1564.FE.IN + SP.POP.1564.MA.IN)
demographics <- demographics %>% mutate(SP.POP.0014.IN = SP.POP.0014.FE.IN + SP.POP.0014.MA.IN)
demographics <- demographics %>% mutate(SP.DYN.AMRT = SP.DYN.AMRT.FE + SP.DYN.AMRT.MA)
demographics <- demographics %>% mutate(SP.POP.TOTL.IN = SP.POP.TOTL.FE.IN + SP.POP.TOTL.MA.IN)
demographics <- demographics %>% mutate(SP.POP.65UP.IN = SP.POP.65UP.FE.IN + SP.POP.65UP.MA.IN)
# Remove old columns
demographics <- demographics %>% select(-c(5:16))

# Fix the countries' names
demographics <- demographics %>% mutate(`Country Name` = replace(`Country Name`, `Country Name`=="Korea, Rep.", "South Korea"))
demographics <- demographics %>% mutate(`Country Name` = replace(`Country Name`, `Country Name`=="Iran, Islamic Rep.", "Iran"))
hospital_beds <- hospital_beds %>% mutate(Country = replace(Country, Country=="Iran (Islamic Republic of)", "Iran"))
hospital_beds <- hospital_beds %>% mutate(Country = replace(Country, Country=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
hospital_beds <- hospital_beds %>% mutate(Country = replace(Country, Country=="Republic of Korea", "South Korea"))
vaccine_doses <- vaccine_doses %>% mutate(Country_Region = replace(Country_Region, Country_Region=="Korea, South", "South Korea"))

# Rename column names to match variables before joining
demographics <- demographics %>% rename(Country = `Country Name`)
vaccine_doses <- vaccine_doses %>% rename(Country = Country_Region)

# Merge 3 tables
my_data <- vaccine_doses %>% full_join(hospital_beds, by = "Country")
my_data <- my_data %>% full_join(demographics, by = "Country")
