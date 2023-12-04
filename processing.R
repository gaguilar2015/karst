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


gtcars |>
  dplyr::select(model, year, hp, trq) |>
  dplyr::slice(1:8) |>
  gt(rowname_col = "model") |>
  tab_row_group(
    label = "numbered",
    rows = matches("^[0-9]")
  )

anal_vars <- c('strata', 'age_group', 'ethnicity', 'education', 'workertype_group')

households |> 
  select(all_of(anal_vars), starts_with('k4')) |> 
  select(-k4_1) |> 
  pivot_longer(
    cols = starts_with('k4'),
    names_to = 'k4_item',
    values_to = 'k4_value'
  ) |> 
  filter(k4_value == 1) |> 
  tabyl(strata, k4_item) |> 
  adorn_totals(c('col'))

households |> 
  select(all_of(anal_vars), starts_with('k4')) |> 
  select(-k4_1) |> 
  pivot_longer(
    cols = starts_with('k4'),
    names_to = 'item',
    values_to = 'value'
  ) |> 
  group_by(strata, item) |> 
  summarise(
    selected = sum(value == 1, na.rm = T),
    total = sum(!is.na(value), na.rm = T),
    prop = selected/total,
    pct = paste0(round(prop * 100, 1), '%'),
    full = paste0(pct, '(', selected,')')
  ) |> 
  select(strata, item, full) |> 
  pivot_wider(
    names_from = item,
    values_from = full
  )

multi_tabulate <- function(row, col){
  
  households |> 
    select(all_of(anal_vars), starts_with(col)) |> 
    pivot_longer(
      cols = starts_with(col),
      names_to = 'item',
      values_to = 'value'
    ) |> 
    group_by(!!as.name(row), item) |> 
    summarise(
      selected = sum(value == 1, na.rm = T),
      total = sum(!is.na(value), na.rm = T),
      prop = selected/total,
      pct = paste0(round(prop * 100, 1), '%'),
      full = paste0(pct, '(', selected,')')
    ) |> 
    select(!!as.name(row), item, full) |> 
    pivot_wider(
      names_from = item,
      values_from = full
    )
  
}


multi_aggregate <- function(var){
  df_list <- purrr::map(anal_vars, ~multi_func1(.x, var))
  
  rbind(df_list[[1]] |> rename(Item = strata),
        df_list[[2]] |> rename(Item = age_group),
        df_list[[3]] |> rename(Item = ethnicity),
        df_list[[4]] |> rename(Item = education),
        df_list[[5]] |> rename(Item = workertype_group)
  )
}


households |> 
  count(strata, age_group, wt = ind_weights) |>
  mutate(total = sum(n)) |>
  mutate(
    pct = n/total
  ) |>
  select(-c(total, n)) |>
  pivot_wider(
    names_from = age_group,
    values_from = pct
  )


households |> 
  tabyl(strata, age_group, show_missing_levels = F, show_na = F) |> 
  adorn_totals(c("col")) |> 
  adorn_percentages(c('row')) |> 
  adorn_pct_formatting(digits = 1) |> 
  adorn_ns() |> 
  rename(Item = strata)
households |>
  count(strata, wt = hh_final_weight)

households |> 
  count(strata, age_group, wt = ind_weights) |>
  group_by(strata) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(
    prop = n/total,
    pct = paste0(round(prop * 100, 1), '%')
  ) |>
  mutate(
    n_pct = paste0(pct, ' (', ceiling(n), ')')
  )  |>
  select(strata, age_group, n_pct) |>
  pivot_wider(
    names_from = age_group,
    values_from = n_pct
  )

households |> 
  tabyl(strata, !!as.name(var), show_missing_levels = F, show_na = F) |> 
  adorn_totals(c("col")) |> 
  rename(Item = strata)


#----------- function

households |> 
  count(k1, wt = ind_weights) |>
  mutate(Item = 'Total') |> 
  group_by(Item) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(
    prop = n/total,
    pct = paste0(round(prop * 100, 1), '%')
  ) |>
  mutate(
    n_pct = paste0(pct, ' (', ceiling(n), ')')
  )  |>
  select(Item, k1, n_pct) |>
  pivot_wider(
    names_from = k1,
    values_from = n_pct
  ) 



ct_single <- function(row, col, format = 'pct'){
  
  households |> 
    count(!!as.name(row), !!as.name(col), wt = ind_weights) |>
    group_by(!!as.name(row)) |>
    mutate(total = sum(n)) |>
    ungroup() |>
    mutate(
      prop = n/total,
      pct = paste0(round(prop * 100, 1), '%')
    ) |>
    mutate(
      n_pct = paste0(pct, ' (', ceiling(n), ')')
    )  |>
    select(!!as.name(row), !!as.name(col), n_pct) |>
    pivot_wider(
      names_from = !!as.name(col),
      values_from = n_pct
    )
  
}

ct_single_aggregate <- function(var){
  df_list <- purrr::map(anal_vars, ~ct_single(.x, var))
  
  df <- rbind(df_list[[1]] |> rename(Item = strata),
        df_list[[2]] |> rename(Item = age_group),
        df_list[[3]] |> rename(Item = ethnicity),
        df_list[[4]] |> rename(Item = education),
        df_list[[5]] |> rename(Item = workertype_group)
  )
  
  df[is.na(df)] <- '0% (0)'
  
  df
}

ct_single_aggregate('k1')


#######----

ct_multi <- function(row, col, format = 'pct'){
  
  
  households |> 
    select(all_of(anal_vars), starts_with(col), ind_weights) |> 
    pivot_longer(
      cols = starts_with(col),
      names_to = 'item',
      values_to = 'value'
    ) |> 
    group_by(!!as.name(row), item) |> 
    summarise(
      .groups = 'drop',
      selected = sum((value == 1) * ind_weights, na.rm = T) ,
      total = sum(!is.na(value) * ind_weights, na.rm = T),
      prop = selected/total,
      pct = paste0(round(prop * 100, 1), '%'),
      full = paste0(pct, '(', selected,')')
    ) |> 
    select(!!as.name(row), item, full) |> 
    mutate(
      item = stringr::str_replace(item, paste0(col, '_'), '')
    ) |> 
    pivot_wider(
      names_from = item,
      values_from = full
    )
  
}


ct_multi_aggregate <- function(var){
  df_list <- purrr::map(anal_vars, ~ct_multi(.x, var))
  
  rbind(df_list[[1]] |> rename(Item = strata),
        df_list[[2]] |> rename(Item = age_group),
        df_list[[3]] |> rename(Item = ethnicity),
        df_list[[4]] |> rename(Item = education),
        df_list[[5]] |> rename(Item = workertype_group)
  )
}

ct_multi_aggregate('k4')


households |> 
  select(strata, starts_with('k4'), ind_weights) |> 
  pivot_longer(
    cols = starts_with('k4'),
    names_to = 'item',
    values_to = 'value'
  ) |> 
  mutate(
    value = as.numeric(value) * ind_weights,
    ind_weights = ind_weights * !is.na(value)
  ) |> 
  group_by(strata, item) |> 
  summarise(
    .groups = 'drop',
    selected = ceiling(sum(value, na.rm = T)),
    total = ceiling(sum(ind_weights, na.rm = T)),
    prop = selected/total,
    pct = paste0(round(prop * 100, 1), '%'),
    full = paste0(pct, '(', selected,')')
  ) |> 
  select(!!as.name(row), item, full) |> 
  mutate(
    item = stringr::str_replace(item, paste0(col, '_'), '')
  ) |> 
  pivot_wider(
    names_from = item,
    values_from = full
  )

k2 |> as_factor() |> 
  filter(!is.na(k2_roster__id)) |> 
  select(interview__key, k2_roster__id, k2_rate) |> 
  pivot_wider(
    names_from = k2_roster__id,
    values_from = k2_rate
  )


k2 |> as_factor() |> 
  filter(!is.na(k2_roster__id)) |> 
  ggplot(aes(x = k2_roster__id)) +
  geom_bar(aes(fill = k2_rate)) +
  coord_flip() + 
  scale_fill_manual(values = MetBrewer::met.brewer(name="Hokusai2", n=5, type="discrete"))

library(HH)  

