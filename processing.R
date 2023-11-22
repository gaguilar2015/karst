library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(stringr)
library(janitor)
library(DBI)

base <- read_sav('./KARST_FILES/tmnr.sav')
listing <- read_sav('./KARST_FILES/listingRoster.sav')
k2 <- read_sav('./KARST_FILES/k2_roster.sav')

k2

glimpse(base)

#47-88-11-61
#37-38-79-37

ignore_qs <- c('79-84-83-57',
               '30-26-25-41',
               '66-59-17-95',
               '92-56-91-59',
               '70-81-50-38',
               '17-39-72-74'
               )

base |> 
  select(interview__key, ctv, ed, building_number, household_number, dwelling_number, result) |> 
  get_dupes(ed, building_number, dwelling_number)

distinct(base, ctv)
distinct(ksample, ctv_official)


listing2 <- base |> 
  select(interview__key, ctv, ed, building_number, household_number, dwelling_number, result) |> 
  left_join(
    listing,
    by = c('interview__key')
  ) |> 
  filter(
    result == 1,
    !interview__key %in% ignore_qs
  )

ll <- listing2 |> 
  mutate(
    age_group = case_when(
      age >= 50 ~ '50+',
      age %in% 35:49 ~ '35-49',
      age %in% 18:35 ~ '18-35',
      age %in% 5:17 ~ '5-17',
      age %in% 0:15 ~ '0-4'
    )
  ) |> select(age, sex, age_group)

ll |> 
  ggplot(aes(x = age_group)) +
  geom_bar()

listing2 |> 
  filter(is.na(age)) |> select(interview__key)


